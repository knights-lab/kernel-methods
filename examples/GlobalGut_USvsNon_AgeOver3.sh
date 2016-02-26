#!/bin/bash
echo hello
Rscript src/runKerns.r 'data/mapping/Yatsunenko_global_gut_study_850_mapping_file.txt' 'data/otu/Yatsunenko_global_gut_study_850_gg_ref_13_8.txt' 'data/unifrac/weighted_unifrac_Yatsunenko_global_gut_study_850_gg_ref_13_8_L7.txt' 'data/bray_curtis/bray_curtis_Yatsunenko_global_gut_study_850_gg_ref_13_8_L6.txt' "list(op='>', var='AGE', val=3)" 'COUNTRY' "c('GAZ:United States of America')" "c('GAZ:Malawi', 'GAZ:Venezuela')" "results/GlobalGut_WesternVsNon_AgeOver3" > "results/GlobalGut_WesternVsNon_AgeOver3_log.txt"
