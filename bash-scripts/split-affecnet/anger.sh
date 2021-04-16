#!/bin/bash

while read p; do
  find */ -name "$p" -exec cp "{}" ./anger \;
done < "anger.txt" 