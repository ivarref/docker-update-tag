(ns com.github.ivarref.docker-update-tag
  (:require
    [babashka.cli :as cli]
    [babashka.fs :as fs]
    [babashka.http-client :as http]
    [babashka.process :refer [shell]]
    [cheshire.core :as json]
    [clojure.edn :as edn]
    [clojure.java.io :as io]
    [clojure.pprint :refer [pprint]]
    [clojure.set :as set]
    [clojure.string :as str])
  (:import (java.net URLEncoder)))

; based on neil-quickadd
; https://github.com/teodorlu/neil-quickadd/blob/master/src/teodorlu/neil_quickadd.clj

(defn ^:private safely-slurp-edn [path orelse]
  (try (edn/read-string (slurp path))
       (catch Exception _ orelse)))

(defn ^:private safely-read [path orelse]
  (try (slurp path)
       (catch Exception _ orelse)))

(defn ^:private parse-dockerfile-line [line]
  (when (str/starts-with? line "FROM ")
    (let [img-and-tag (second (str/split line #"\s+"))]
      [(first (str/split img-and-tag #":"))])))

(defn ^:private parse-dockerfile [dockerfile-str]
  (->> (str/split-lines dockerfile-str)
       (mapcat parse-dockerfile-line)))

(defn ^:private scan-dockerfile-files
  ([path] (scan-dockerfile-files path nil))
  ([path {:keys [max-depth]}]
   (let [dockerfile-files (map str (concat
                                     (fs/glob path "Dockerfile")
                                     (fs/glob path "**/Dockerfile" (when max-depth {:max-depth max-depth}))))
         safe-read-deps (fn [dockerfile-file]
                          (or (seq (parse-dockerfile (safely-read dockerfile-file "")))
                              (list)))]
     (->> (mapcat safe-read-deps dockerfile-files)
          (into #{})
          sort))))

(defn ^:private share-path []
  (fs/expand-home "~/.local/share/docker-update-tag"))

(defn ^:private ensure-share-path! []
  (fs/create-dirs (share-path)))

(defn ^:private index-file-path []
  (str (share-path) "/index.edn"))

(defn ^:private blacklist-file-path []
  (str (share-path) "/blacklist.edn"))

(defn ^:private update-index! [k f & args]
  (let [m (safely-slurp-edn (index-file-path) {})
        m (if (map? m) m {})
        m (into (sorted-map) m)]
    (ensure-share-path!)
    (spit (index-file-path)
          (with-out-str
            (pprint (apply update m k f args))))))

(defn scan-and-update-index! [{:keys [opts]}]
  (when (or (:h opts) (:help opts))
    (println (str/trim "
Usage:

  docker-update-tag scan [OPTS...]
  docker-update-tag scan DIR [OPTS...]

Traverses DIR for Dockerfiles. DIR defaults to the current directory.
Docker image names are added to the docker-update-tag index.

Allowed OPTS:

  --maxdepth N     ; Limits the traversal to N layers down from DIR.
"))
    (System/exit 0))
  (let [root-dir (:path opts ".")
        root-dir (-> root-dir fs/expand-home fs/absolutize fs/canonicalize str)
        {:keys [max-depth]} opts
        all-deps (scan-dockerfile-files root-dir (when max-depth {:max-depth max-depth}))]
    (update-index! root-dir (fn [_] all-deps))))

(defn quickadd-libs-raw* []
  (into #{} (seq (apply concat (vals (safely-slurp-edn (index-file-path) {}))))))

(defn quickadd-blacklist* []
  (into #{} (:blacklist (safely-slurp-edn (blacklist-file-path) {}))))

(defn images* []
  (set/difference (quickadd-libs-raw*) (quickadd-blacklist*)))

(defn add-curr-dir []
  (parse-dockerfile (safely-read "Dockerfile" "")))

(declare update-tag)

(defn eligible-image-names []
  (distinct (into (vec (reverse (add-curr-dir))) (reverse (sort (images*))))))

(defn select-image-name []
  (let [fzf-result (shell {:out      :string
                           :continue true
                           :in       (str/join "\n" (into [":quit"] (eligible-image-names)))}
                          "fzf" "--no-sort")]
    (when (not= 0 (:exit fzf-result))
      ;; If FZF terminates, we terminate.
      (System/exit 0))
    (let [selected (str/trim (:out fzf-result))]
      (when (= ":quit" selected)
        ;; Also provide a "non crashing" exit option.
        (System/exit 0))
      selected)))

(defn select-image-and-update [{:keys [opts]}]
  (when (or (:h opts) (:help opts))
    #_(print-subcommands {})
    (System/exit 0))
  (if (not-empty (eligible-image-names))
    (update-tag {:opts {:image (select-image-name)}})
    (do (println "No libs indexed")
        (println "Please use `docker-find-tag scan` to populate the index")
        (System/exit 1))))

(defn uri-encode
  [string]
  (some-> string str (URLEncoder/encode "UTF-8") (.replace "+" "%20")))

; based on https://stackoverflow.com/questions/28320134/how-can-i-list-all-tags-for-a-docker-image-on-a-remote-registry

(defn name-of-user-or-org [image]
  (if (str/includes? image "/")
    (last (butlast (str/split image #"/")))
    "library"))

(defn image-part [image]
  (last (str/split image #"/")))

(defn fetch-from-other-repo [image]
  (let [host (first (str/split image #"/"))
        uri (str "https://"
                 host
                 "/v2/"
                 (uri-encode (str/join "/" (drop 1 (str/split image #"/"))))
                 "/tags/list?n=100")]
    (try
      (let [{:keys [body]} (http/get uri {:headers {"Accept" "application/json"}})]
        (->> (:tags (json/parse-string body keyword))
             (remove #(= "latest" %))
             (vec)))
      (catch Exception e
        (binding [*out* *err*]
          (println "Error during HTTP get for uri" (pr-str uri) "with error message:" (ex-message e))
          (System/exit 1))))))

; TODO does not work
(comment
  (fetch-from-other-repo "gcr.io/google_containers/spark-base"))

; works:
(comment
  (fetch-from-other-repo "mcr.microsoft.com/azure-cli"))

(defn fetch-tags [image]
  ;(println "Fetching tags for image" image)
  (let [uri (str "https://hub.docker.com/v2/namespaces/"
                 (uri-encode (name-of-user-or-org image))
                 "/repositories/"
                 (image-part image)
                 "/tags?page_size=100")]
    (try
      (let [{:keys [status body]} (http/get uri {:headers {"Accept" "application/json"}})]
        (when-not (= 200 status)
          (binding [*out* *err*]
            (println "Error during HTTP get for uri" (pr-str uri) ", status code:" status)
            (System/exit 1)))
        (->> (:results (json/parse-string body keyword))
             (mapv :name)
             (remove #(= "latest" %))
             (vec)))
      (catch Exception e
        (if (and (str/includes? image ".")
                 (str/includes? image "/")
                 (= 404 (:status (ex-data e))))
          (do
            #_(println "404 for" uri)
            (fetch-from-other-repo image))
          (binding [*out* *err*]
            (println "Error during HTTP get for uri" (pr-str uri) "with error message:" (ex-message e))
            (System/exit 1)))))))

(defn add-zeros [longs cnt]
  (if (= cnt (count longs))
    longs
    (add-zeros (into longs [0]) cnt)))

(defn tag->numbers [tag]
  (->> (str/split tag #"\D+")
       (remove empty?)
       (mapv parse-long)))

(defn version-sort
  [el1 el2]
  (let [longs-1 (tag->numbers el1)
        longs-2 (tag->numbers el2)
        max-count (max (count longs-1) (count longs-2))
        longs-1 (add-zeros longs-1 max-count)
        longs-2 (add-zeros longs-2 max-count)]
    (if (= longs-1 longs-2)
      (compare (count el1) (count el2))
      (compare longs-1 longs-2))))

(comment
  (sort version-sort
        ["temurin-22-lein-2.11.2-jammy"
         "tools-deps-bullseye-slim"]))
(comment
  (sort version-sort
        ["21.0.3_9-jdk-ubi9-minimal"
         "21.0.3_9-jre-ubi9-minimal"]))

(defn sort-tags [tags]
  (sort version-sort tags))

(comment
  (defonce tags (fetch-tags "eclipse-temurin")))

(comment
  (sort-tags (fetch-tags "eclipse-temurin")))

(defn list-tags [{:keys [opts]}]
  (let [image (or (:image opts)
                  (select-image-name))]
    (doseq [tag (sort-tags (fetch-tags image))]
      (println (str image ":" tag)))))

(defn update-dockerfile-line [line image tag]
  (if (str/starts-with? line "FROM ")
    (let [parts (str/split line #"\s" -1)
          found? (atom false)]
      (str/join " "
                (mapv
                  (fn [part]
                    (if (and (false? @found?)
                             (str/starts-with? part (str image ":")))
                      (do
                        (reset! found? true)
                        (str image ":" tag))
                      part))
                  parts)))
    line))

(comment
  (update-dockerfile-line
    "FROM clojure:temurin-17-tools-deps-1.11.1.1113-focal as builder #janei clojure:"
    "clojure"
    "temurin-22-tools-deps-1.11.3.1456-jammy"))

(defn update-dockerfile-str [dockerfile-str image tag]
  (str/join "\n"
            (mapv (fn [line] (update-dockerfile-line line image tag))
                  (str/split dockerfile-str #"\r?\n" -1))))

(defn update-dockerfile! [image tag]
  (let [old-content (safely-read "Dockerfile" "")
        new-content (update-dockerfile-str old-content image tag)]
    (cond
      (not= old-content new-content)
      (do
        (spit "Dockerfile" new-content)
        (println "Updated" image "to" tag))

      (not (.exists (io/file "Dockerfile")))
      (do
        (spit "Dockerfile" (str "FROM " image ":" tag "\n"))
        (println "Created new Dockerfile with " (str image ":" tag)))

      (and (.exists (io/file "Dockerfile"))
           (not (str/includes? new-content (str image ":" tag))))
      (do
        (spit "Dockerfile" (str "\nFROM " image ":" tag "\n") :append true)
        (println "Added" (str image ":" tag) "to Dockerfile"))

      :else
      (binding [*out* *err*]
        (println "No changes were made. Doing nothing.")))))

(comment
  (println
    (update-dockerfile!
      "clojure"
      "temurin-22-tools-deps-1.11.3.1456-jammy")))

(defn select-tag [image opts]
  (if-let [tag (:tag opts)]
    tag
    (let [tags (sort-tags (fetch-tags image))
          fzf-result (shell {:out      :string
                             :continue true
                             :in       (str/join "\n" (into [":quit"] (reverse tags)))}
                            "fzf" "--no-sort")]
      (when (not= 0 (:exit fzf-result))
        ;; If FZF terminates, we terminate.
        (System/exit 0))
      (let [selected (str/trim (:out fzf-result))]
        (when (= ":quit" selected)
          ;; Also provide a "non crashing" exit option.
          (System/exit 0))
        selected))))

(defn update-tag [{:keys [opts]}]
  (let [image (or (:image opts) (select-image-name))
        selected (select-tag image opts)]
    (update-dockerfile! image selected)))

(defn print-subcommands [{}]
  (println (str/trim "
Usage: docker-update-tag [COMMAND] [OPTION...]

Available commands:

  docker-update-tag                ; Add or update an image tag
  docker-update-tag help           ; Print subcommands
  docker-update-tag scan           ; Scan a folder for Dockerfiles
  docker-update-tag list           ; List available tags for an image

")))

(def dispatch-table
  [
   ;{:cmds ["clear-index"]    :fn quickadd-clear-index}
   ;{:cmds ["index-path"]     :fn quickadd-index-path}
   {:cmds ["help"] :fn print-subcommands}
   ;{:cmds ["libs"]           :fn quickadd-libs     }
   {:cmds ["scan"] :fn scan-and-update-index! :args->opts [:path]}
   {:cmds ["update"] :fn update-tag :args->opts [:image :tag]}
   {:cmds ["list"] :fn list-tags :args->opts [:image]}
   ;{:cmds ["blacklist"]      :fn quickadd-blacklist}
   ;{:cmds ["blacklist-lib"]  :fn quickadd-blacklist-lib :args->opts [:lib]}
   ;{:cmds ["blacklist-list"] :fn quickadd-blacklist-list}
   {:cmds [] :fn select-image-and-update}])

(defn ensure-env-ok
  "Terminate and give user error if the user needs to install something."
  []
  (when-not (fs/which "fzf")
    (println "docker-find-tag requires fzf.")
    (println "Please install fzf, then call `docker-find-tag` again.")
    (System/exit 1)))

(defn -main [& args]
  (ensure-env-ok)
  (cli/dispatch dispatch-table args))
