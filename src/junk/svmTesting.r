#SRC
library('e1071')
library('kernlab')
library('randomForest')
source('model.knn.r')
source('model.svm.r')
source('train.r')
library('caret')

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
#convert to similarity matrix (to use as kernelMatrix)
uni.sim = 1-uni.dist
#create L2 distance matrix for otu svm
l2.dist = as.matrix(dist(otus))

#number of cross-validation folds to do
nfolds = 10
#generate random sample indices
sampler = sample(1:nrow(uni.dist),nrow(uni.dist),replace=FALSE)
#how many samples per holdout?
part = nrow(uni.dist)%/%nfolds

#true positives, true negatives, false positives, false negatives
results = matrix(0,nfolds,5)
colnames(results) = c('method','TP','TN','FP','FN','cor(pearson)')
#cross-validation loop
for(n in 0:nfolds-1){
	inc = 1 +  n * part
	#UNIFRAC SVM
	#----------
	#grab the holdout
	holdout.logic = c(1:nrow(uni.sim)) %in% sampler[inc:((inc+part)-1)]
	holdout = uni.sim[holdout.logic,]
	#grab the training set
	uni.training.samples = uni.sim[!holdout.logic,]
	#limit cols to training rows (to simluate introduction of new data)
	uni.training.samples = uni.training.samples[,rownames(uni.training.samples)]
	#grab training outcomes
	uni.training.outcomes = factor(map[rownames(uni.training.samples),'COUNTRY', drop=F] == "GAZ:United States of America")
	#grab test samples
	uni.validation.samples = uni.sim[sampler[(part+1):nrow(uni.sim)],rownames(uni.training.samples)]
	#grab test outcomes
	uni.validation.outcomes = factor(map[rownames(uni.validation.samples),'COUNTRY', drop=F] == "GAZ:United States of America")
	uni.svm.model = ksvm(as.kernelMatrix(uni.training.samples),uni.training.outcomes,kernel='matrix')
	svm.prediction = predict(uni.svm.model,as.kernelMatrix(uni.validation.samples[,SVindex(uni.svm.model), drop=F]))
	res = confusionMatrix(svm.prediction,uni.validation.outcomes)
	pearsons = cor(as.numeric(svm.prediction),as.numeric(uni.validation.outcomes))
	results[n+1,] = c(res$table[1,1],res$table[2,2],res$table[1,2],res$table[2,1], pearsons)
	
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
	l2.svm.model = ksvm(l2.training.samples,l2.training.outcomes,kernel='vanilladot')
	l2.svm.prediction = predict(l2.svm.model,l2.validation.samples)
	res = confusionMatrix(l2.svm.prediction,l2.validation.outcomes)
	
}

#uni as data
uni.svm.model = ksvm(uni.training.samples,uni.training.outcomes,kernel='vanilladot')
svm.prediction = predict(uni.svm.model,uni.validation.samples)
confusionMatrix(svm.prediction,factor(uni.map[rownames(uni.validation.samples),'COUNTRY']=="GAZ:United States of America"))

#uni as kernel
uni.svm.model = ksvm(as.kernelMatrix(uni.training.samples),uni.training.outcomes,kernel='matrix')
svm.prediction = predict(uni.svm.model,as.kernelMatrix(uni.validation.samples[,SVindex(uni.svm.model), drop=F]))
confusionMatrix(svm.prediction,factor(uni.map[rownames(uni.validation.samples),'COUNTRY']=="GAZ:United States of America"))