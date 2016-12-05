#!/bin/bash
Rscript src/runKerns.r 'data/mapping/HMP_filtered500_map.txt' 'data/otu/HMP_filtered500.txt' 'data/unifrac/weighted_unifrac_HMP_filtered500.txt' 'data/bray_curtis/bray_curtis_HMP_filtered500_L6.txt' "" 'SEX' "c('female')" "c('male')" "results/HMP_female_vs_male" > "results/HMP_female_vs_male_log.txt"
