#import dependencies
#libs
library('e1071')
library('kernlab')
library('randomForest')
#src
source('model.knn.r')
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
#FIX: do this once for all 3 so that testing data is identical
overlap = intersect(rownames(map),rownames(otus))
map = map[overlap,]
otus = otus[overlap,]
# do the filtering for unifrac
uni.overlap = intersect(rownames(map),rownames(uni.dist))
uni.map = map[uni.overlap,]
uni.dist = uni.dist[uni.overlap,]
#create L2 distance matrix for otu knn
l2.dist = as.matrix(dist(otus))
#generate random sample indices
sampler = sample(1:nrow(uni.dist),nrow(uni.dist),replace=FALSE)
part = 0.8 * nrow(uni.dist)

#L2 OTU KNN
#----------
#grab the training set
l2.training.samples = l2.dist[sampler[1:part],]
#grab the outcomes (US vs not US)
l2.training.outcomes = (map[rownames(l2.training.samples),'COUNTRY', drop=F] == "GAZ:United States of America")
#keep the names with the outcomes factor
outcome.names = rownames(l2.training.outcomes)
l2.training.outcomes = factor(l2.training.outcomes)
names(l2.training.outcomes) = outcome.names
#create the model
l2.knn.model = knn.dist(l2.training.samples,l2.training.outcomes,k=10)
#run knn on test set (will return a named character list)
l2.knn.prediction = predict(l2.knn.model,l2.dist)
#grab the actual outcomes, keep the names
l2.actual.outcomes = (map[rownames(l2.dist[sampler[(part+1):nrow(l2.dist)],]),'COUNTRY', drop=F] == "GAZ:United States of America")
actual.outcome.names = rownames(l2.actual.outcomes)
l2.actual.outcomes = as.character(l2.actual.outcomes)
names(l2.actual.outcomes) = actual.outcome.names
#synchronize list indices
overlap = intersect(names(l2.actual.outcomes),names(l2.knn.prediction))
#make confusion matrix
confusionMatrix(l2.knn.prediction[overlap], l2.actual.outcomes[overlap])
l2.knn.confusion = table(l2.knn.prediction[overlap], l2.actual.outcomes[overlap])

#UNIFRAC KNN
#-----------
#grab the training set
uni.training.samples = uni.dist[sampler[1:part],]
#grab the outcomes (US vs not US)
uni.training.outcomes = (uni.map[rownames(uni.training.samples),'COUNTRY', drop=F] == "GAZ:United States of America")
#keep the names with the outcomes factor
outcome.names = rownames(uni.training.outcomes)
uni.training.outcomes = factor(uni.training.outcomes)
names(uni.training.outcomes) = outcome.names
#create the model
uni.knn.model = knn.dist(uni.training.samples,uni.training.outcomes,k=10)
#run knn on test set
uni.knn.prediction = predict(uni.knn.model,uni.dist)
#grab the actual outcomes, keep the names
uni.actual.outcomes = (uni.map[rownames(uni.dist[sampler[(part+1):nrow(uni.dist)],]),'COUNTRY', drop=F] == "GAZ:United States of America")
uni.actual.outcome.names = rownames(uni.actual.outcomes)
uni.actual.outcomes = as.character(uni.actual.outcomes)
names(uni.actual.outcomes) = uni.actual.outcome.names
#synchronize list indices
uni.overlap = intersect(names(uni.actual.outcomes),names(uni.knn.prediction))
#make confusion matrix
confusionMatrix(uni.knn.prediction[overlap], uni.actual.outcomes[overlap])
uni.knn.confusion = table(uni.knn.prediction[overlap], uni.actual.outcomes[overlap])


#tune L2 knn
#grab all the outcomes, the tuner will cross validate
l2.outcomes = (map[rownames(l2.dist),'COUNTRY', drop=F] == "GAZ:United States of America")
#keep the names with the outcomes factor
outcome.names = rownames(l2.training.outcomes)
l2.outcomes = factor(l2.outcomes)
names(l2.outcomes) = outcome.names
#tuning
l2.knn.result = mwas.evaluate(l2.dist,l2.outcomes,model.type='KNN',knn.k=c(10,20,30))

#tune unifrac knn
#grab all the outcomes, the tuner will cross validate
uni.outcomes = (uni.map[rownames(uni.dist),'COUNTRY', drop=F] == "GAZ:United States of America")
#keep the names with the outcomes factor
outcome.names = rownames(uni.training.outcomes)
uni.outcomes = factor(uni.outcomes)
names(uni.outcomes) = outcome.names
#tuning
uni.knn.result = mwas.evaluate(uni.dist,uni.outcomes,model.type='KNN',knn.k=(1:40))


