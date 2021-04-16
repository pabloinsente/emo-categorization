#!/bin/bash

while read p; do
  find */ -name "$p" -exec cp "{}" ./happiness \;
done < "happiness.txt" 