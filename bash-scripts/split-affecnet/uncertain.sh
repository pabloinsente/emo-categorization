while read p; do
  find */ -name "$p" -exec cp "{}" ./uncertain \;
done < "uncertain.txt" 