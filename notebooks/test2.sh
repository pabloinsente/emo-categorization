#!/bin/bash

while read p; do
  while read q; do
   echo "$q"
  done < "$p" 
done < categories_names.txt

