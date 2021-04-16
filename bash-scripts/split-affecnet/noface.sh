while read p; do
  find */ -name "$p" -exec cp "{}" ./noface \;
done < "noface.txt" 