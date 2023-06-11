#!/bin/bash

git clone https://github.com/8ta4/reddit.git
cd reddit || exit
direnv allow
install
reddit -h
