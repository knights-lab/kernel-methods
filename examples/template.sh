#!/bin/bash
Rscript src/runKerns.r 'data/mapping/' 'data/otu/' 'data/unifrac/' 'data/bray_curtis/' "list(op='filterfun',var='filtervar', val='c( \"filterval\")'" 'Test Variable' "c('Positive Class')" "c('Negative Class')" "Out_Prefix"
