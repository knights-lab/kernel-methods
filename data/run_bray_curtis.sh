for f in *Taxa*/*L6.biom
do
 echo $f
 beta_diversity.py -i $f -m  bray_curtis -o bray_curtis/
done
