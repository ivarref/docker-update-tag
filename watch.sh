#!/usr/bin/env bash

printf "src/com/github/ivarref/docker_update_tag.clj" | entr -c ./dev.sh $@
