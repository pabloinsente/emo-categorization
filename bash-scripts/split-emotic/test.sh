#!/bin/bash

while read f; do
  mkdir "$f"
done < categories_names_fold.txt

