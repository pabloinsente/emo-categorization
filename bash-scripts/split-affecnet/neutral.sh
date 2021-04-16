while read p; do
  find */ -name "$p" -exec cp "{}" ./neutral \;
done < "neutral.txt" 