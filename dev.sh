#!/usr/bin/env bash

echo "$(date +'%H:%M:%S') args: $@"

bbin install . --as docker-update-tag-dev

docker-update-tag-dev $@
echo "Done!"
#_cat $HOME/.local/share/docker-find-tag/index.edn
