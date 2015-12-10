#import dependencies
#libs
library('e1071')
library('kernlab')
library('randomForest')
#src
source('model.knn.r')
source('model.svm.r')
source('train.r')

#read in data
map = read.table('../data/Yatsunenko_global_gut_study_850_mapping_file.txt',sep='\t',head=T,row=1,check=F,comment='')
otus = read.table('../data/Yatsunenko_global_gut_study_850_gg_ref_13_8_L7.txt',sep='\t',head=T,row=1,check=F,comment='')
unidist = read.table('../data/unifracDistances/weighted_unifrac_Yatsunenko_global_gut_study_850_gg_ref_13_8.txt',sep='\t',head=T,row=1,check=F,comment='')

#switch otus to have points as rows instead of columns
otus = t(otus)
overlap = intersect(rownames(map),rownames(otus))
map = map[overlap,]
otus = otus[overlap,]

#create L2 distance matrix for knn
otudist = as.matrix(dist(otus))
#create response variable for desired outcome
outcomes = t(subset(map,select='COUNTRY'))
names(outcomes) = colnames(outcomes)
outcomes = factor(outcomes)

#run random forest
random.forest.result = mwas.evaluate(as.matrix(otus),outcomes,model.type='RF')
#run L2 knn
knn.result = mwas.evaluate(otudist,outcomes,model.type='KNN',knn.k=1:5)
#run unifrac knn
unidist = as.matrix(unidist)
uni.knn.result = mwas.evaluate(unidist,outcomes,model.type='KNN',knn.k=1:5)
#run standard svm
svm.result = mwas.evaluate(as.matrix(otus),outcomes,model.type='SVM', svm.c=2^(1:5))
#run unifrac svm
#uni.svm.result = mwas.evaluate(as.kernelMatrix(unidist),outcomes,model.type='SVM',svm.kernel='matrix', svm.c=2^(1:5))


#results
print('L2 KNN performances:')
knn.result$performances

print('SVM performances:')
svm.result$performances
