while read p; do
  find */ -name "$p" -exec cp "{}" ./surprise \;
done < "surprise.txt" 