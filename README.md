# docker-update-tag

Add or update your Dockerfile tags _with style_.

<img width="600" src="https://raw.githubusercontent.com/ivarref/docker-update-tag/main/demo/demo2.svg">

## Usage

`docker-update-tag scan DIR` - Scan `DIR` for docker image names.

`docker-update-tag` - Add or update a docker image tag.

## Installing

0. Install Babashka: https://github.com/babashka/babashka
1. Install fzf: https://github.com/junegunn/fzf
2. Install bbin: https://github.com/babashka/bbin
3. Install docker-update-tag with bbin:

   ```
   bbin install com.github.ivarref/docker-update-tag --latest-sha
   ```

## Thanks!

Without [@teodorlu][teodorlu], [@borkdude][borkdude] and [@rads][rads], `docker-update-tag` wouldn't exist. Thank you!

[teodorlu]: https://github.com/teodorlu/
[borkdude]: https://github.com/borkdude/
[rads]: https://github.com/rads/
