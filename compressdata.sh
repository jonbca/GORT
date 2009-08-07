#!/bin/sh
find -E guessdata ! -regex ".*/\..*" -type f > /tmp/guessfiles
tar jcvf guessdata.tar.bz2 -T /tmp/guessfiles
