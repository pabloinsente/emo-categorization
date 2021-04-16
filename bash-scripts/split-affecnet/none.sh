while read p; do
  find */ -name "$p" -exec cp "{}" ./none \;
done < "none.txt" 