while read p; do
  find */ -name "$p" -exec cp "{}" ./disgust \;
done < "disgust.txt" 