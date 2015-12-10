# kernel-methods
Evaluation and development of kernel methods for microbiome classification

The goals of this project are two-fold. One is to examine and compare the predictive performance of various kernel-based methods on variables of a number of datasets used by Knights lab. The other is to explore new kernels with tunable parameters based on ecological distance metrics. 

##System Overview
The kernel-methods project employs R scripts to run cross-validated predictive performance analysis of the classification of variables of different dataset otu files. In english, it means for example we want to see how well different kernel-methods can predict the value of a certain variable (eg whether or not you are obese) based on your microbiota.

###Usage
The main file is **runKerns.R**. This is really all you should need for actually running experiments. The syntax is as follows (assumes your working directory is the same as runKerns.r):
`Rscript runKerns 'path/to/mapping_file.txt' 'path/to/taxa_summary.txt' 'path/to/unifrac_file.txt' 'path/to/bray_curtis_file.txt' filters 'variable' positive_classes negative_classes 'path/to/output' `
Where:
* `filters` is a string representing R syntax for a list with the following key/value pairs:
  * `op="<value>"`, where `<value>` can be any binary mathematical operator
  * `var="<value>"`, where `<value>` is name of the variable to filter on (must match the mapping file column header)
  * `val=<value>`, where `<value>` is a numeric or string expression
  * Example (filter for persons having AGE > 3): `"list(op='>', var='AGE', val=50)"`
* `'variable'` is the name of the column header in the mapping file corresponding to the variable to predict
* `positive_classes` is a string representing R syntax for a vector of values to be considered 'positive' in binary classification.
 * Example: `"c('GAZ:United States of America')"`
* `negative_classes` has the same syntax as positive_classes, but specifies values to be considered 'negative' in binary classification.



Refer to individual folders for description / explanation of content:
######doc
Contains files that describe the system & how it works
######src
Contains the code & libraries
######data
Contains otu tables, beta-diversity files, and taxa summaries
######results
Contains output of running kernel-method experiments (tables, graphs, etc)
######pub
Contains files relevant to publications from this project
