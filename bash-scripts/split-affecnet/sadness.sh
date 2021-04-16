while read p; do
  find */ -name "$p" -exec cp "{}" ./sadness \;
done < "sadness.txt" 