#!/bin/bash

while read p; do
  find */ -name "$p" -exec cp "{}" ./fear \;
done < "fear.txt" 