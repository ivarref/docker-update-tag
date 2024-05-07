#!/usr/bin/env bash

echo "$(date +'%H:%M:%S') args: $@"

bbin install . --as docker-update-tag-dev

docker-update-tag-dev $@
echo "Done!"
