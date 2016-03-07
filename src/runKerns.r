library('e1071')
library('kernlab')
library('randomForest')
source('src/model.knn.r')
source('src/model.svm.r')
source('src/train.r')
#outer cross-validation loop
source('src/cvLoop.r')
source('src/balancedFolds.r')
source('src/f1.r')
library('caret')
library('parallel')
library('MASS')
library('RColorBrewer')
library('ROCR')
options(error=traceback)
#on.exit(traceback())
# consider using ROCR
#DATA SETUP 
#----------
#read in data
args = commandArgs(trailingOnly = TRUE) 
mapFile = args[1] #ex '../data/Yatsunenko_global_gut_study_850_mapping_file.txt'
otuFile = args[2] #'../data/Yatsunenko_global_gut_study_850_gg_ref_13_8_L7.txt'
uniFile = args[3] #'../data/unifrac/weighted_unifrac_Yatsunenko_global_gut_study_850_gg_ref_13_8.txt'
bcFile = args[4]
filterstr = args[5]
variable = args[6] #'COUNTRY'
positiveClasses = eval(parse(text=args[7])) #'GAZ:United States of America'
negativeClasses = eval(parse(text=args[8]))
outid = args[9]
print('positive classes: ')
print(positiveClasses)
print('negative classes:')
print(negativeClasses)

#mapFile = 'data/mapping/Yatsunenko_global_gut_study_850_mapping_file.txt'
#otuFile = 'data/Yatsunenko_Taxa/Yatsunenko_global_gut_study_850_gg_ref_13_8_L7.txt'
#uniFile =  'data/unifrac/weighted_unifrac_Yatsunenko_global_gut_study_850_gg_ref_13_8_L7.txt'
#bcFile = 'data/bray_curtis/bray_curtis_Yatsunenko_global_gut_study_850_gg_ref_13_8_L6.txt'
#variable = 'COUNTRY'
#positiveClasses = c('GAZ:United States of America')
#negativeClasses = c('GAZ:Venezuela','GAZ:Malawi')
#filterstr = "list(op='>', var='AGE', val=50)"


map = read.table(mapFile,sep='\t',head=T,row=1,check=F,comment='', quote='"')
otus = read.table(otuFile,sep='\t',head=T, skip=1,row=1,check=F,comment='', quote='"')
uni.dist = read.table(uniFile,sep='\t',head=T,row=1,check=F,comment='', quote='"')
bc.dist = read.table(bcFile,sep='\t',head=T,row=1,check=F,comment='', quote='"')
#switch otus to have points as rows instead of columns
otus = t(otus)

print('map size before filtering:')
print(dim(map))

#filter map based on input args
if(nchar(filterstr) > 0){
	filterlist = eval(parse(text=filterstr))
	filterfunstr = paste("function(m){as.",class(filterlist$val),"(","as.character(m)",")",filterlist$op,as.character(filterlist$val),"}", sep='')
	print('filter function: ')
	print(filterfunstr)
	filterfun = eval(parse(text=filterfunstr))
	map.filter.logic = apply(as.matrix(map[,filterlist$var]),1,filterfun)
	map = map[map.filter.logic,]
}
print('map size after user filter:')
print(dim(map))
#filter map to have only specified classes (the other data structures will be filtered by the map)
map = map[map[,variable]%in% c(positiveClasses, negativeClasses),]
print('map size after class filter:')
print(dim(map))
#filter on intersection of rownames
overlap = intersect(rownames(map),rownames(otus))
map = map[overlap,]
otus = otus[overlap,]
# do the filtering for unifrac
uni.overlap = intersect(rownames(map),rownames(uni.dist))
uni.map = map[uni.overlap,]
uni.dist = as.matrix(uni.dist[uni.overlap,uni.overlap])
#convert to similarity matrix (to use as kernelMatrix)
uni.sim = 1-uni.dist

bc.overlap = intersect(rownames(map),rownames(bc.dist))
bc.map = map[bc.overlap,]
bc.dist = as.matrix(bc.dist[bc.overlap,bc.overlap])
#convert to similarity matrix (to use as kernelMatrix)
bc.sim = 1-bc.dist

#create L2 distance matrix for otu svm
l2.dist = as.matrix(dist(otus))
#create L2 distance matrix for otu svm
l2.dist = as.matrix(dist(otus))
#grab all the outcomes, the tuner will cross validate
l2.outcomes = (map[rownames(l2.dist),variable] %in% positiveClasses)
#keep the names with the outcomes factor
outcome.names = rownames(l2.outcomes)
l2.outcomes = factor(l2.outcomes)
names(l2.outcomes) = outcome.names

#number of cross-validation folds to do
nfolds = 10

#generate random sample indices
filtered.var = factor(map[,variable])
print(levels(filtered.var))
fold.ids = balanced.folds(filtered.var,nfolds)
nfolds = max(as.numeric(levels(as.factor(fold.ids))))
#uni.sampler = sample(1:nrow(uni.dist),nrow(uni.dist),replace=FALSE)
#bc.sampler = sample(1:nrow(bc.dist),nrow(bc.dist),replace=FALSE)
#l2.sampler = sample(1:nrow(l2.dist),nrow(l2.dist),replace=FALSE)
#otu.sampler = sample(1:nrow(otus), nrow(otus), replace=FALSE)
#how many samples per holdout?
#uni.part = nrow(bc.dist)%/%nfolds
#bc.part = nrow(uni.dist)%/%nfolds
#l2.part = nrow(uni.dist)%/%nfolds
#otu.part = nrow(otus)%/%nfolds

numMethods = 3 #svm, knn, rf
numKernels = 3 #l2, unifrac, bray_curtis, none (raw)
#true positives, true negatives, false positives, false negatives
results = matrix(0,0,7)
colnames(results) = c('method','TP','TN','FP','FN','performance','best.model')

clust = makeCluster((detectCores()-1))
#run in parallel
clusterEvalQ(clust, library('e1071'))
clusterEvalQ(clust, library('kernlab'))
clusterEvalQ(clust, library('randomForest'))
clusterEvalQ(clust, source('src/model.knn.r'))
clusterEvalQ(clust, source('src/model.svm.r'))
clusterEvalQ(clust, source('src/train.r'))
clusterEvalQ(clust, source('src/f1.r'))
clusterEvalQ(clust, library('caret'))
clusterExport(clust,"map")
clusterExport(clust,"otus")
clusterExport(clust,"uni.map")
clusterExport(clust,"uni.dist")
clusterExport(clust,"uni.sim")
clusterExport(clust,"bc.map")
clusterExport(clust,"bc.dist")
clusterExport(clust,"bc.sim")
clusterExport(clust,"l2.dist")
clusterExport(clust,"l2.outcomes")
#clusterExport(clust,"uni.sampler")
#clusterExport(clust,"bc.sampler")
#clusterExport(clust,"l2.sampler")
#clusterExport(clust,"otu.sampler")
#clusterExport(clust,"uni.part")
#clusterExport(clust,"bc.part")
#clusterExport(clust,"l2.part")
#clusterExport(clust,"otu.part")
clusterExport(clust,"fold.ids")
clusterExport(clust,"numMethods")
clusterExport(clust,"numKernels")
clusterExport(clust,"variable")
clusterExport(clust,"positiveClasses")
clusterExport(clust,"variable")
res = parLapply(clust, 1:nfolds, doOuterCV)
#for(i in 1:nfolds){
#	doOuterCV(i)
#}
for(r in res){
	results = rbind(results,r)
}
results = results[order(results[,1]),]
write.csv(results,file=paste(outid,'_results.txt', sep=''))
f1.means = aggregate(performance ~ method ,data = results, FUN='mean')
performance = f1.means$performance
names(performance) = f1.means$method
png(paste(outid,'_results.png', sep=''))
par(mar=c(8,4,2,2))
midpoints = barplot(performance, las=2, ylim=c(min(performance)-0.02, max(performance)+0.02), xpd=FALSE,col=brewer.pal(8,'Set2'))
text(midpoints,round(performance,4),labels=round(performance,4))
axis(1,at=c(-1e6, 1e6),labels=NA)
dev.off()
