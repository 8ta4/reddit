#!/bin/bash

set -e

git clone https://github.com/8ta4/reddit.git
cd reddit
direnv allow
devenv shell build
devenv shell reddit -h
