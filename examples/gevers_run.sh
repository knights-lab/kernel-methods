#!/bin/bash
Rscript src/runKerns.r 'data/mapping/Gevers_CCFA_RISK_study_1939_mapping_file.txt' 'data/Gevers_Taxa/Gevers_CCFA_RISK_study_1939_gg_ref_13_8_L7.txt' 'data/unifrac/weighted_unifrac_Gevers_CCFA_RISK_study_1939_gg_ref_13_8.txt' "" 'ANTIBIOTICS' "c('no')" "c('None','yes')" "Gevers_Antibiotics"
