#!/bin/bash
Rscript src/runKerns.r 'data/mapping/HMP_filtered500_map.txt' 'data/otu/HMP_filtered500.txt' 'data/unifrac/weighted_unifrac_HMP_filtered500.txt' 'data/bray_curtis/bray_curtis_HMP_filtered500_L6.txt' "" 'HMPBODYSUBSITE' "c('Tongue_dorsum')" "c('Supragingival_plaque')" "results/HMP_TongueVsSupraPlaque" > "results/HMP_TongueVsSupraPlaque_log.txt"
