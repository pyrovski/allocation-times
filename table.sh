#!/bin/bash

grep -i error ./*noderuns*|cut -d':' -f1|uniq > error

ls ./*noderuns* | jtset -d error > no_error

cat header > table

cat no_error |xargs -I{} ./entry.sh {} >> table
