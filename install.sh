#!/bin/bash

git clone https://github.com/8ta4/reddit.git
cd reddit
direnv allow
install
reddit -h