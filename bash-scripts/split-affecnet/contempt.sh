while read p; do
  find */ -name "$p" -exec cp "{}" ./contempt \;
done < "contempt.txt" 