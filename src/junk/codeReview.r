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
#grab the outcomes (US vs not US)
l2.training.outcomes = factor(map[rownames(l2.training.samples),'COUNTRY', drop=F] == "GAZ:United States of America")
#grab test samples
l2.validation.samples = l2.dist[sampler[(part+1):nrow(l2.dist)],]
#grab test outcomes
l2.validation.outcomes = factor(map[rownames(l2.validation.samples),'COUNTRY', drop=F] == "GAZ:United States of America")
l2.svm.model = ksvm(l2.training.samples,l2.training.outcomes,kernel='vanilladot')
l2.svm.prediction = predict(l2.svm.model,l2.validation.samples)
library('e1071')
library('kernlab')
library('randomForest')
source('model.knn.r')
source('model.svm.r')
source('train.r')
l2.svm.model = ksvm(l2.training.samples,l2.training.outcomes,kernel='vanilladot')
l2.svm.prediction = predict(l2.svm.model,l2.validation.samples)
source('train.r')
l2.svm.model = ksvm(l2.training.samples,l2.training.outcomes,kernel='vanilladot')
l2.svm.prediction = predict(l2.svm.model,l2.validation.samples)
confusionMatrix(l2.svm.prediction,l2.validation.outcomes)
library('caret')
l2.svm.model = ksvm(l2.training.samples,l2.training.outcomes,kernel='vanilladot')
l2.svm.prediction = predict(l2.svm.model,l2.validation.samples)
confusionMatrix(l2.svm.prediction,l2.validation.outcomes)
#UNIFRAC SVM
#----------
#grab the training set
uni.training.samples = uni.dist[sampler[1:part],]
uni.training.samples = uni.training.samples[,rownames(uni.training.samples)]
uni.training.outcomes = factor(map[rownames(uni.training.samples),'COUNTRY', drop=F] == "GAZ:United States of America")
#grab test samples
uni.validation.samples = uni.dist[sampler[(part+1):nrow(uni.dist)],]
#grab test outcomes
uni.validation.outcomes = factor(map[rownames(uni.validation.samples),'COUNTRY', drop=F] == "GAZ:United States of America")
uni.svm.model = ksvm(uni.dist,uni.map[,'COUNTRY']=="GAZ:United States of America",kernel='vanilladot')
svm.prediction = predict(uni.svm.model,uni.dist)
confusionMatrix(svm.prediction,factor(uni.map[,'COUNTRY']=="GAZ:United States of America"))
uni.svm.model = ksvm(uni.training.samples,uni.training.outcomes,kernel='vanilladot')
svm.prediction = predict(uni.svm.model,uni.validation.samples)
confusionMatrix(svm.prediction,factor(uni.map[rownames(uni.validation.samples),'COUNTRY']=="GAZ:United States of America"))#UNIFRAC SVM
#----------
#grab the training set
uni.training.samples = uni.dist[sampler[1:part],]
uni.training.outcomes = factor(map[rownames(uni.training.samples),'COUNTRY', drop=F] == "GAZ:United States of America")
#grab test samples
uni.validation.samples = uni.dist[sampler[(part+1):nrow(uni.dist)],]
#grab test outcomes
uni.validation.outcomes = factor(map[rownames(uni.validation.samples),'COUNTRY', drop=F] == "GAZ:United States of America")
#UNIFRAC SVM
#----------
#grab the training set
uni.training.samples = uni.dist[sampler[1:part],]
uni.training.outcomes = factor(map[rownames(uni.training.samples),'COUNTRY', drop=F] == "GAZ:United States of America")
#grab test samples
uni.validation.samples = uni.dist[sampler[(part+1):nrow(uni.dist)],]
#grab test outcomes
uni.validation.outcomes = factor(map[rownames(uni.validation.samples),'COUNTRY', drop=F] == "GAZ:United States of America")
uni.svm.model = ksvm(uni.training.samples,uni.training.outcomes,kernel='vanilladot')
svm.prediction = predict(uni.svm.model,uni.validation.samples)
confusionMatrix(svm.prediction,factor(uni.map[rownames(uni.validation.samples),'COUNTRY']=="GAZ:United States of America"))
#L2 OTU SVM
#----------
#grab the training set
l2.training.samples = l2.dist[sampler[1:part],]
#limit cols to training rows (to simluate introduction of new data)
l2.training.samples = l2.training.samples[,rownames(l2.training.samples)]
#grab the outcomes (US vs not US)
l2.training.outcomes = factor(map[rownames(l2.training.samples),'COUNTRY', drop=F] == "GAZ:United States of America")
#grab test samples
l2.validation.samples = l2.dist[sampler[(part+1):nrow(l2.dist)],rownames(l2.training.samples)]
#grab test outcomes
l2.validation.outcomes = factor(map[rownames(l2.validation.samples),'COUNTRY', drop=F] == "GAZ:United States of America")
#UNIFRAC SVM
#----------
#grab the training set
uni.training.samples = uni.dist[sampler[1:part],]
#limit cols to training rows (to simluate introduction of new data)
uni.training.samples = uni.training.samples[,rownames(uni.training.samples)]
#grab training outcomes
uni.training.outcomes = factor(map[rownames(uni.training.samples),'COUNTRY', drop=F] == "GAZ:United States of America")
#grab test samples
uni.validation.samples = uni.dist[sampler[(part+1):nrow(uni.dist)],rownames(uni.training.samples)]
#grab test outcomes
uni.validation.outcomes = factor(map[rownames(uni.validation.samples),'COUNTRY', drop=F] == "GAZ:United States of America")
l2.svm.model = ksvm(l2.training.samples,l2.training.outcomes,kernel='vanilladot')
l2.svm.prediction = predict(l2.svm.model,l2.validation.samples)
confusionMatrix(l2.svm.prediction,l2.validation.outcomes)
uni.svm.model = ksvm(uni.training.samples,uni.training.outcomes,kernel='vanilladot')
svm.prediction = predict(uni.svm.model,uni.validation.samples)
confusionMatrix(svm.prediction,factor(uni.map[rownames(uni.validation.samples),'COUNTRY']=="GAZ:United States of America"))
#uni as kernel
uni.svm.model = ksvm(as.kernelMatrix(uni.training.samples),uni.training.outcomes,kernel='matrix')
svm.prediction = predict(uni.svm.model,uni.validation.samples)
dim(uni.validation.samples)
dim(uni.training.asamples)
svm.prediction = predict(uni.svm.model,t(uni.validation.samples))
class(uni.validation.samples)
?predict.ksvm
#uni as kernel
uni.svm.model = ksvm(as.kernelMatrix(uni.training.samples),uni.training.outcomes,kernel='matrix')
svm.prediction = predict(uni.svm.model,as.kernelMatrix(uni.validation.samples))
confusionMatrix(svm.prediction,factor(uni.map[rownames(uni.validation.samples),'COUNTRY']=="GAZ:United States of America"))
#UNIFRAC SVM
#----------
#grab the training set
uni.training.samples = uni.dist[sampler[1:part],]
#limit cols to training rows (to simluate introduction of new data)
uni.training.samples = uni.training.samples[,rownames(uni.training.samples)]
#grab training outcomes
uni.training.outcomes = factor(map[rownames(uni.training.samples),'COUNTRY', drop=F] == "GAZ:United States of America")
#grab test samples
uni.validation.samples = uni.dist[sampler[(part+1):nrow(uni.dist)],rownames(uni.training.samples)]
#grab test outcomes
uni.validation.outcomes = factor(map[rownames(uni.validation.samples),'COUNTRY', drop=F] == "GAZ:United States of America")
?kernelMatrix
confusionMatrix(l2.svm.prediction,l2.validation.outcomes)
uni.sim = 1-uni.dist
#UNIFRAC SVM
#----------
#grab the training set
uni.training.samples = uni.sim[sampler[1:part],]
#limit cols to training rows (to simluate introduction of new data)
uni.training.samples = uni.training.samples[,rownames(uni.training.samples)]
#grab training outcomes
uni.training.outcomes = factor(map[rownames(uni.training.samples),'COUNTRY', drop=F] == "GAZ:United States of America")
#grab test samples
uni.validation.samples = uni.sim[sampler[(part+1):nrow(uni.sim)],rownames(uni.training.samples)]
#grab test outcomes
uni.validation.outcomes = factor(map[rownames(uni.validation.samples),'COUNTRY', drop=F] == "GAZ:United States of America")
#uni as kernel
uni.svm.model = ksvm(as.kernelMatrix(uni.training.samples),uni.training.outcomes,kernel='matrix')
svm.prediction = predict(uni.svm.model,as.kernelMatrix(uni.validation.samples))
confusionMatrix(svm.prediction,factor(uni.map[rownames(uni.validation.samples),'COUNTRY']=="GAZ:United States of America"))
uni.svm.model
#uni as kernel
uni.svm.model = ksvm(as.kernelMatrix(uni.training.samples),uni.training.outcomes,kernel='matrix')
svm.prediction = predict(uni.svm.model,as.kernelMatrix(uni.validation.samples[,SVindex(uni.svm.model), drop=F]))
confusionMatrix(svm.prediction,factor(uni.map[rownames(uni.validation.samples),'COUNTRY']=="GAZ:United States of America"))
savehistory('codeReview.r')
