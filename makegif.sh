#!/bin/zsh

printf %s\\n {0..50} | xargs -n 1 -P 8 -I {} ./gengif.sh {} $1

for i in images/gif/*.jpg ; convert -quality 85% $i $i

convert -delay 20 images/gif/*.jpg images/gif/go.gif
