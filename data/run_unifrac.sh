for f in biom/*.biom
do
 echo $f
 beta_diversity.py -i $f -m weighted_unifrac -o unifrac/ -t tree/97_otus.tree
done
