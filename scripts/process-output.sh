#!/bin/bash
sed -n -e '/^[^#]/p' $1  > $(dirname $1)/$(basename -s .csv $1)-samples.csv
sed -n -e '/^#/p' $1  > $(dirname $1)/$(basename -s .csv $1)-header.txt


