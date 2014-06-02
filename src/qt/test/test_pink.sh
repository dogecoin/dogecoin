#!/bin/sh
COLOR=pink
PINK_LINES=`grep -r "background-color: $COLOR" ../ |wc -l`

if [ "$PINK_LINES" -eq "0" ]; then
  exit 0;
else
  exit 1;
fi 
