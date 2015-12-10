# kernel-methods
Evaluation and development of kernel methods for microbiome classification

The goals of this project are two-fold. One is to examine and compare the predictive performance of various kernel-based methods on variables of a number of datasets used by Knights lab. The other is to explore new kernels with tunable parameters based on ecological distance metrics. 

##System Overview
<p>The kernel-methods project employs R scripts to run cross-validated predictive performance analysis of the classification of variables of different dataset otu files. In english, it means for example we want to see how well different kernel-methods can predict the value of a certain variable (eg whether or not you are obese) based on your microbiota. </p>


Refer to individual folders for description / explanation of content:
######doc
<p>Contains files that describe the system & how it works</p>
######src
<p>Contains the code & libraries</p>
######data
<p>Contains otu tables, beta-diversity files, and taxa summaries</p>
######results
<p>Contains output of running kernel-method experiments (tables, graphs, etc)</p>
######pub
<p>Contains files relevant to publications from this project</p>
