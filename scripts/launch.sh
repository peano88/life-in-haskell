#!/bin/bash

if [ -z "$1" ]; then
  echo "Bash"
  docker run -it --rm \
  --mount type=bind,source="$(pwd)"/../src,target=/tmp \
  --mount type=bind,source="$(pwd)"/../data,target=/tmp/data \
  haskell:latest \
  bash
else
  echo "Interactive"
  docker run -it --rm \
  --mount type=bind,source="$(pwd)"/../src,target=/tmp \
  --mount type=bind,source="$(pwd)"/../data,target=/tmp/data \
  haskell:latest
fi