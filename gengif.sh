#!/bin/sh
stack exec -- rezoomer --size $((25 + $1)) --inImage $2 --outImage images/gif/out_$1.jpg
