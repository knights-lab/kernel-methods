#import dependencies
#libs
library('e1071')
library('kernlab')
library('randomForest')
#src
source('model.svm.r')
source('model.svm.r')
source('train.r')

#DATA SETUP 
#----------
#read in data
map = read.table('../data/Yatsunenko_global_gut_study_850_mapping_file.txt',sep='\t',head=T,row=1,check=F,comment='')
otus = read.table('../data/Yatsunenko_global_gut_study_850_gg_ref_13_8_L7.txt',sep='\t',head=T,row=1,check=F,comment='')
uni.dist = read.table('../data/unifracDistances/weighted_unifrac_Yatsunenko_global_gut_study_850_gg_ref_13_8.txt',sep='\t',head=T,row=1,check=F,comment='')
#switch otus to have points as rows instead of columns
otus = t(otus)
#filter on intersection of rownames
overlap = intersect(rownames(map),rownames(otus))
map = map[overlap,]
otus = otus[overlap,]
# do the filtering for unifrac
uni.overlap = intersect(rownames(map),rownames(uni.dist))
uni.map = map[uni.overlap,]
uni.dist = as.matrix(uni.dist[uni.overlap,])
#create L2 distance matrix for otu svm
l2.dist = as.matrix(dist(otus))
#generate random sample indices
sampler = sample(1:nrow(uni.dist),nrow(uni.dist),replace=FALSE)
part = 0.8 * nrow(uni.dist)

#L2 OTU SVM
#----------
#grab the training set
l2.training.samples = l2.dist[sampler[1:part],]
l2.validation.samples = l2.dist[sampler[(part+1):nrow(l2.dist)],];
#grab the outcomes (US vs not US)
l2.training.outcomes = (map[rownames(l2.training.samples),'COUNTRY', drop=F] == "GAZ:United States of America")
#keep the names with the outcomes factor
outcome.names = rownames(l2.training.outcomes)
l2.training.outcomes = factor(l2.training.outcomes)
names(l2.training.outcomes) = outcome.names

#create the model
l2.svm.model = svm.model(as.kernelMatrix(l2.training.samples),l2.training.outcomes,C=8, kernel='matrix')
#run svm on test set (will return a named character list)
l2.svm.prediction = predict(l2.svm.model,as.kernelMatrix(l2.validation.samples))
#grab the actual outcomes, keep the names
l2.actual.outcomes = (map[rownames(l2.validation.samples),'COUNTRY', drop=F] == "GAZ:United States of America")
actual.outcome.names = rownames(l2.actual.outcomes)
l2.actual.outcomes = as.character(l2.actual.outcomes)
names(l2.actual.outcomes) = actual.outcome.names
#synchronize list indices
overlap = intersect(names(l2.actual.outcomes),names(l2.knn.prediction))

#make confusion matrix
l2.svm.confusion = table(l2.svm.prediction, l2.actual.outcomes)

#UNIFRAC SVM
#----------
#grab the training set
uni.training.samples = uni.dist[sampler[1:part],]
uni.training.samples = uni.training.samples[,rownames(uni.training.samples)]
uni.validation.samples = uni.dist[sampler[(part+1):nrow(uni.dist)],rownames(uni.training.samples)];
#grab the outcomes (US vs not US)
uni.training.outcomes = (uni.map[rownames(uni.training.samples),'COUNTRY', drop=F] == "GAZ:United States of America")
#keep the names with the outcomes factor
outcome.names = rownames(uni.training.outcomes)
uni.training.outcomes = factor(uni.training.outcomes)
names(uni.training.outcomes) = outcome.names

#create kernel matrix
make.uni.kern = function(x1,x2){
	
}
training.kernel.matrix = 
#create the model
uni.svm.model = svm.model(as.kernelMatrix(uni.training.samples),uni.training.outcomes,C=8, kernel='matrix')
#run svm on test set (will return a named character list)
uni.svm.prediction = predict(uni.svm.model,as.kernelMatrix(uni.validation.samples))
#grab the actual outcomes, keep the names
uni.actual.outcomes = (uni.map[rownames(uni.validation.samples),'COUNTRY', drop=F] == "GAZ:United States of America")
actual.outcome.names = rownames(uni.actual.outcomes)
uni.actual.outcomes = as.character(uni.actual.outcomes)
names(uni.actual.outcomes) = actual.outcome.names
#make confusion matrix
uni.svm.confusion = table(uni.svm.prediction, uni.actual.outcomes)


#tune L2 svm
#grab all the outcomes, the tuner will cross validate
l2.outcomes = (map[rownames(l2.dist),'COUNTRY', drop=F] == "GAZ:United States of America")
#keep the names with the outcomes factor
outcome.names = rownames(l2.training.outcomes)
l2.outcomes = factor(l2.outcomes)
names(l2.outcomes) = outcome.names
#tuning
l2.svm.result = mwas.evaluate(as.kernelMatrix(l2.dist),l2.outcomes,model.type='SVM',svm.C=2^(1:5), svm.kernel='matrix')

#tune unifrac svm
#grab all the outcomes, the tuner will cross validate
uni.outcomes = (uni.map[rownames(uni.dist),'COUNTRY', drop=F] == "GAZ:United States of America")
#keep the names with the outcomes factor
outcome.names = rownames(uni.training.outcomes)
uni.outcomes = factor(uni.outcomes)
names(uni.outcomes) = outcome.names
#tuning
uni.svm.result = mwas.evaluate(as.kernelMatrix(uni.dist),uni.outcomes,model.type='SVM',svm.C=2^(1:5),svm.kernel='matrix')