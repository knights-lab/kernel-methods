# kernel-methods
Evaluation and development of kernel methods for microbiome classification

The goals of this project are two-fold. One is to examine and compare the predictive performance of various kernel-based methods on variables of a number of datasets used by Knights lab. The other is to explore new kernels with tunable parameters based on ecological distance metrics. 

##System Overview
The kernel-methods project employs R scripts to run cross-validated predictive performance analysis of the classification of variables of different dataset otu files. In english, it means for example we want to see how well different kernel-methods can predict the value of a certain variable (eg whether or not you are obese) based on your microbiota.
##Dependencies
You must have R installed on your machine, along with the RScript command in your environment variables if you want to do batch scripting. Additionally, the following packages must be installed in R:
* e1071
* kernlab
* randomForest
* parallel
* MASS
* RColorBrewer

R may automatically ask you to install packages if you try to run without installing them first.
###Usage
The main file is **runKerns.R**. This is all you should need to actually run experiments once data files are in place. The syntax is as follows (assumes your working directory is the same as runKerns.r):
`Rscript runKerns.r 'path/to/mapping_file.txt' 'path/to/taxa_summary.txt' 'path/to/unifrac_file.txt' 'path/to/bray_curtis_file.txt' filters 'variable' positive_classes negative_classes 'path/to/output' `
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

Note that these are all **required, positional** arguments. All data file paths are required. If you wish not to supply a value (for example, no filters), pass in a blank value: ""

A complete example is shown below. This particular call runs our predictive performance sweep on GlobalGut data, where the classification test is whether or not the individual is from the United States. Persons less than 3 years of age are filtered out:
* `Rscript src/runKerns.r 'data/mapping/Yatsunenko_global_gut_study_850_mapping_file.txt' 'data/Yatsunenko_Taxa/Yatsunenko_global_gut_study_850_gg_ref_13_8_L7.txt' 'data/unifrac/weighted_unifrac_Yatsunenko_global_gut_study_850_gg_ref_13_8.txt' 'data/bray_curtis/bray_curtis_Yatsunenko_global_gut_study_850_gg_ref_13_8_L6.txt' "list(op='>', var='AGE', val=50)" 'COUNTRY' "c('GAZ:United States of America')" "c('GAZ:Malawi', 'GAZ:Venezuela')" "results/GlobalGut_WesternVsNon_AgeOver3"`

##About this Repo
Refer to individual folders for description / explanation of content:
######doc
Contains files that describe the system & how it works
######examples
Contains sample .sh scripts that run the classification tests for various datasets/variables
######src 
Contains the code & libraries
######data
Contains otu tables, beta-diversity files, and taxa summaries.
######results
Contains output of running kernel-method experiments (tables, graphs, etc)
######pub
Contains files relevant to publications from this project
