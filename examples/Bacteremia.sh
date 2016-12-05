#!/bin/bash
Rscript src/runKerns.r 'data/mapping/Bacteremia_mapping.txt' 'data/otu/Bacteremia_gg_ref_13_8.txt' 'data/unifrac/weighted_unifrac_Bacteremia_gg_ref_13_8.txt' 'data/bray_curtis/bray_curtis_Bacteremia_gg_ref_13_8_L6.txt' "" 'Treatment' "c('bact')" "c('NObact')" "results/Bacteremia" > "results/Bacteremia_log.txt"
