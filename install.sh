#!/bin/bash

set -e

# Remove existing reddit directory if it exists
if [ -d "reddit" ]; then
  rm -rf reddit
fi

git clone https://github.com/8ta4/reddit.git
cd reddit
direnv allow
devenv shell build
devenv shell reddit -h
