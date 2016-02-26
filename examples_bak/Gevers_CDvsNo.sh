#!/bin/bash
Rscript src/runKerns.r 'data/mapping/Gevers_CCFA_RISK_study_1939_mapping_file.txt' 'data/otu/Gevers_CCFA_RISK_study_1939_gg_ref_13_8.txt' 'data/unifrac/weighted_unifrac_Gevers_CCFA_RISK_study_1939_gg_ref_13_8.txt' 'data/bray_curtis/bray_curtis_Gevers_CCFA_RISK_study_1939_gg_ref_13_8_L6.txt' "" 'DIAGNOSIS' "c('no','control','IC','UC')" "c('CD')" "results/Gevers_CD"
