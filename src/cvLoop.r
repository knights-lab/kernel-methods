doOuterCV = function(n){
	cols = c('method','TP','TN','FP','FN','f1','matthews','class-wise','auc','observed_over_baseline','best_model')
	results = matrix(0,0,length(cols))
	#inc = 1 +  n * uni.part
	index = 1
	print (n)
	#holdout.logic = 1:nrow(uni.sim) %in% uni.sampler[inc:((inc+uni.part)-1)]
	holdout.logic = fold.ids==n
	#UNIFRAC
	print('UNIFRAC')
	#----------
	#grab the holdout
	holdout = uni.sim[holdout.logic,]
	#grab the training set
	uni.training.samples = uni.sim[!holdout.logic,]
	#limit cols to training rows (to simluate introduction of new data)
	uni.training.samples = uni.training.samples[,rownames(uni.training.samples)]
	#grab training outcomes
	uni.training.outcomes = factor(map[rownames(uni.training.samples),variable] %in% positiveClasses)
	#grab test samples
	uni.validation.samples = holdout[,rownames(uni.training.samples)]
	#grab test outcomes
	uni.validation.outcomes = factor(map[rownames(uni.validation.samples),variable] %in% positiveClasses)
	#SVM
	print('--svm')
	#tune params with cross-validation
	#TODO make other spots match this syntax
	uni.svm.model = invisible(tune(svm.model, ranges = list(kernel='matrix',C=2^(-2:3)), uni.training.samples,uni.training.outcomes)$best.model)
	print(paste('post svm tuning in fold ',n))
	#uni.svm.model = ksvm(as.kernelMatrix(uni.training.samples),uni.training.outcomes,kernel='matrix')
	svm.prediction = predict(uni.svm.model,uni.validation.samples)
	print(paste('post svm prediction in fold ',n))
	levels(svm.prediction) = union(levels(svm.prediction),levels(uni.validation.outcomes))
	svm.uni.validation.outcomes = uni.validation.outcomes
	levels(svm.uni.validation.outcomes) = union(levels(svm.prediction),levels(uni.validation.outcomes))
	print(paste('post svm stuff in fold ',n))
	pf = performance(svm.prediction,svm.uni.validation.outcomes)
	results = rbind(results, c('SVM.unifrac',pf$TP,pf$TN,pf$FP,pf$FN, pf$f1, pf$mcc, NA, NA, NA, paste('C:',uni.svm.model$svm@param$C)))
	#KNN
	print('--knn')
	uni.knn.model = tune(ranges = list(k=seq(1:20)),knn.dist, uni.training.samples,uni.training.outcomes)$best.model
	knn.prediction = as.factor(predict(uni.knn.model,rbind(uni.training.samples,uni.validation.samples)))
	levels(knn.prediction) = union(levels(knn.prediction),levels(uni.validation.outcomes))
	knn.uni.validation.outcomes = uni.validation.outcomes
	levels(knn.uni.validation.outcomes) = union(levels(knn.prediction),levels(uni.validation.outcomes))
	pf = performance(knn.prediction,knn.uni.validation.outcomes)
	
	results = rbind(results, c('KNN.unifrac',pf$TP,pf$TN,pf$FP,pf$FN, pf$f1, pf$mcc, NA, NA, NA, paste('k:',uni.knn.model$k)))
	#Unifrac RF
	print('--rf')
	uni.rf = randomForest(uni.training.samples,uni.training.outcomes)
	rf.prediction = predict(uni.rf, uni.validation.samples)
	levels(rf.prediction) = union(levels(rf.prediction),levels(uni.validation.outcomes))
	rf.uni.validation.outcomes = uni.validation.outcomes
	levels(rf.uni.validation.outcomes) = union(levels(rf.prediction),levels(uni.validation.outcomes))
	pf = performance(rf.prediction,rf.uni.validation.outcomes)
	results = rbind(results, c('RF.unifrac',pf$TP,pf$TN,pf$FP,pf$FN, pf$f1, pf$mcc, NA, NA, NA, ''))
	
	
	#pc.uni
	print('PC.UNI')
	#----------
	#grab the holdout
	#holdout.logic = 1:nrow(pc.uni) %in% uni.sampler[inc:((inc+uni.part)-1)]
	holdout.logic = fold.ids==n
	#pc.all is pcoa of unifrac distances
	holdout = uni.dist[holdout.logic,]
	#grab the training set (only want distances between training set and themselves)
	pc.training.samples = uni.dist[!holdout.logic,!holdout.logic]
	#grab training outcomes
	pc.training.outcomes = factor(map[rownames(pc.training.samples),variable] %in% positiveClasses)
	#grab test samples
	pc.validation.samples = uni.dist[holdout.logic,!holdout.logic]
	#grab test outcomes
	pc.validation.outcomes = factor(map[rownames(pc.validation.samples),variable] %in% positiveClasses)
	
	#SVM
	print('--svm')
	#tune params with cross-validation
	kernCustomData = function(x,model,...){
	  rest.of.args = list(...)
	  custom.data = list()
	  # x is some subset of the data, becuase the tune function
	  # does its own cross validation.
	  # it has all the distances we need,
	  # but we have to grab the same training set of the columns 
	  # and align them with the rows
	  # because apparently they get scrambled
	  training.ids = intersect(rownames(x),colnames(x))
	  training.set = x[training.ids,training.ids]
	  # P is k-dimensional PCoA matrix of training samples only
	  P = cmdscale(training.set,k=rest.of.args$pc.k)
	  custom.data[['pc.k']] = rest.of.args$pc.k
	  custom.data[['ref.pcoa']] = P
	  custom.data[['ref.dist']] = x
	  return(custom.data)
	}
	kernFunTrain = function(x,model,...){
	  rest.of.args = list(...)
	  # P is k-dimensional PCoA matrix of training samples only
	  P = model$customData$ref.pcoa
		#want similarity matrix
		P = 1-as.matrix(dist(P))
		return(P)
	}
	kernFunPredict = function(x,model,...){
	  rest.of.args = list(...)
	  # now we have some new data we need to predict,
	  # but we need to transform it into the pcoa space
	  # of only the training samples. 
	  train.ids = rownames(model$customData$ref.dist)
	  test.ids = rownames(x)
	  #can reconstruct D from holdout & reference training set
	  D = rbind(model$customData$ref.dist, x)
	  # P is PCoA matrix of training samples only
	  P = model$customData$ref.pcoa
	  # project new samples into PCoA space, preserving original
	  # distances best we can
	  transformed.samples = PCoPD(P,D,train.ids,test.ids)
	  #want similarity matrix of testing samples to training samples
	  new.P = rbind(P,transformed.samples)
	  new.P.dist = as.matrix(dist(new.P))
	  test.set = new.P.dist[test.ids,train.ids]
	  return(test.set)
	}
	knnMakeDist = function(x,model,...){
	  rest.of.args = list(...)
	  # want to make full distance matrix, but pcoa must only be performed on 
	  # training samples
    train.ids = rownames(model$customData$ref.dist)
    test.ids = rownames(x)
    #can reconstruct D from holdout & reference training set
    D = rbind(model$customData$ref.dist, x)
    # P is PCoA matrix of training samples only
    P = model$customData$ref.pcoa
    # project new samples into PCoA space, preserving original
    # distances best we can
    transformed.samples = PCoPD(P,D,train.ids,test.ids)
    #want similarity matrix of testing samples to training samples
    new.P = rbind(P,transformed.samples)
    new.P.dist = as.matrix(dist(new.P))
    #want to hand the whole set instead of just training samples
    return(new.P.dist)
	}
	pc.svm.model = invisible(tune( ranges = list(kernel='custom',C=2^(-2:3),pc.k = seq(2,20,2) ),kernelCustomData=kernCustomData, kernelFunTrain = kernFunTrain, kernelFunPredict = kernFunPredict, svm.model, pc.training.samples,pc.training.outcomes)$best.model)
	print(paste('post svm tuning in fold ',n))
	

	svm.prediction = predict(pc.svm.model,pc.validation.samples)
	print(paste('post svm prediction in fold ',n))
	levels(svm.prediction) = union(levels(svm.prediction),levels(pc.validation.outcomes))
	svm.pc.validation.outcomes = pc.validation.outcomes
  levels(svm.pc.validation.outcomes) = union(levels(svm.prediction),levels(svm.pc.validation.outcomes))
	print(paste('post svm stuff in fold ',n))
	pf = performance(svm.prediction,svm.pc.validation.outcomes)
	results = rbind(results, c(paste('SVM.pc.uni',sep=''),pf$TP,pf$TN,pf$FP,pf$FN, pf$f1, pf$mcc, NA, NA, NA, paste('k:',pc.svm.model$customData$pc.k,'C:',pc.svm.model$svm@param$C)))
	#KNN
	print('--knn')
	pc.knn.model = tune(ranges = list(k=seq(1:20),pc.k=seq(2,20,2)), customKernel = knnMakeDist, customData=kernCustomData, knn.dist, pc.training.samples,pc.training.outcomes)$best.model
	knn.prediction = as.factor(predict(pc.knn.model,rbind(pc.training.samples,pc.validation.samples)))
	knn.pc.validation.outcomes = pc.validation.outcomes
  levels(knn.prediction) = union(levels(knn.prediction),levels(pc.validation.outcomes))
	levels(knn.pc.validation.outcomes) = union(levels(knn.prediction),levels(pc.validation.outcomes))
	pf = performance(knn.prediction,knn.pc.validation.outcomes)
	
	results = rbind(results, c(paste('KNN.pc.uni',sep=''),pf$TP,pf$TN,pf$FP,pf$FN, pf$f1, pf$mcc, NA,NA,NA, paste('k:',uni.knn.model$k,'pc.k:',uni.knn.model$customData$pc.k)))
	#pc.uni RF
	print('--rf')
	pc.uni.rf = randomForest(pc.training.samples,uni.training.outcomes)
	rf.prediction = predict(pc.uni.rf, pc.validation.samples)
	rf.pc.validation.outcomes = pc.validation.outcomes
  levels(rf.prediction) = union(levels(rf.prediction),levels(pc.validation.outcomes))
	levels(rf.pc.validation.outcomes) = union(levels(rf.prediction),levels(pc.validation.outcomes))
	pf = performance(rf.prediction,rf.pc.validation.outcomes)
		
	results = rbind(results, c(paste('RF.pc.uni.',sep=''),pf$TP,pf$TN,pf$FP,pf$FN, pf$f1, pf$mcc, NA, NA, NA, ''))

	
	#BRAY_CURTIS
	print('BRAY_CURTIS')
	#----------
	#grab the holdout
	#holdout.logic = 1:nrow(bc.sim) %in% bc.sampler[inc:((inc+bc.part)-1)]
	holdout = bc.sim[holdout.logic,]
	#grab the training set
	bc.training.samples = bc.sim[!holdout.logic,]
	#limit cols to training rows (to simluate introduction of new data)
	bc.training.samples = bc.training.samples[,rownames(bc.training.samples)]
	#grab training outcomes
	bc.training.outcomes = factor(map[rownames(bc.training.samples),variable] %in% positiveClasses)
	#grab test samples
	bc.validation.samples = holdout[,rownames(bc.training.samples)]
	#grab test outcomes
	bc.validation.outcomes = factor(map[rownames(bc.validation.samples),variable] %in% positiveClasses)
	#SVM
	print('--svm')
	#tune params with cross-validation
	bc.svm.model = invisible(tune( ranges = list(kernel='matrix',C=2^(-2:3)),svm.model, bc.training.samples,bc.training.outcomes)$best.model)
	svm.prediction = predict(bc.svm.model,bc.validation.samples)
	levels(svm.prediction) = union(levels(svm.prediction),levels(bc.validation.outcomes))
	svm.bc.validation.outcomes = bc.validation.outcomes
	levels(svm.bc.validation.outcomes) = union(levels(svm.prediction),levels(bc.validation.outcomes))
	pf = performance(svm.prediction,svm.bc.validation.outcomes)
	
	
	results = rbind(results, c('SVM.bc',pf$TP,pf$TN,pf$FP,pf$FN, pf$f1, pf$mcc, NA, NA, NA, paste('C:',bc.svm.model$svm@param$C)))
	#KNN
	bc.knn.model = tune(knn.dist,ranges = list(k=seq(1:20)), bc.training.samples,bc.training.outcomes)$best.model
	knn.prediction = as.factor(predict(bc.knn.model,rbind(bc.training.samples,bc.validation.samples)))
	levels(knn.prediction) = union(levels(knn.prediction),levels(bc.validation.outcomes))
	knn.bc.validation.outcomes = bc.validation.outcomes
	levels(knn.bc.validation.outcomes) = union(levels(knn.prediction),levels(bc.validation.outcomes))
	pf = performance(knn.prediction,knn.bc.validation.outcomes)
	
	
	results = rbind(results, c('KNN.bc',pf$TP,pf$TN,pf$FP,pf$FN, pf$f1, pf$mcc, NA, NA, NA, paste('k:',bc.knn.model$k)))
	#Bray curtis RF
	print('--rf')
	bc.rf = randomForest(bc.training.samples,bc.training.outcomes)
	rf.prediction = predict(bc.rf, bc.validation.samples)
	levels(rf.prediction) = union(levels(rf.prediction),levels(bc.validation.outcomes))
	rf.bc.validation.outcomes = bc.validation.outcomes
	levels(rf.bc.validation.outcomes) = union(levels(rf.prediction),levels(bc.validation.outcomes))
	
	pf = performance(rf.prediction,rf.bc.validation.outcomes)
	
	
	results = rbind(results, c('RF.bc',pf$TP,pf$TN,pf$FP,pf$FN, pf$f1, pf$mcc, NA, NA, NA, ''))
	#L2 OTU
	print('EUCLIDEAN')
	#-------
	#grab the holdout
	#holdout.logic = c(1:nrow(l2.dist)) %in% l2.sampler[inc:((inc+l2.part)-1)]
	holdout = l2.dist[holdout.logic,]
	#grab the training set
	l2.training.samples = l2.dist[!holdout.logic,]
	#limit cols to training rows (to simluate introduction of new data)
	l2.training.samples = l2.training.samples[,rownames(l2.training.samples)]
	#grab the outcomes (US vs not US)
	l2.training.outcomes = factor(map[rownames(l2.training.samples),variable] %in% positiveClasses)
	#grab test samples
	l2.validation.samples = holdout[,rownames(l2.training.samples)]
	#grab test outcomes
	l2.validation.outcomes = factor(map[rownames(l2.validation.samples),variable] %in% positiveClasses)	
	#KNN
	print('--knn')
	l2.knn.model = knn.dist(l2.training.samples,l2.training.outcomes,k=10)
	l2.knn.model = tune(knn.dist,ranges = list(k=seq(1:20)), l2.training.samples,l2.training.outcomes)$best.model
	knn.prediction = as.factor(predict(l2.knn.model,rbind(l2.training.samples,l2.validation.samples)))
	levels(knn.prediction) = union(levels(knn.prediction),levels(l2.validation.outcomes))
	knn.l2.validation.outcomes = l2.validation.outcomes
	levels(knn.l2.validation.outcomes) = union(levels(knn.prediction),levels(l2.validation.outcomes))	
	pf = performance(knn.prediction,knn.l2.validation.outcomes)
	
	
	results = rbind(results, c('KNN.euc',pf$TP,pf$TN,pf$FP,pf$FN, pf$f1, pf$mcc, NA, NA, NA,paste('k:',l2.knn.model$k)))
	#L2 RF
	print('--rf')
	l2.rf = randomForest(l2.training.samples,l2.training.outcomes)
	rf.prediction = predict(l2.rf, l2.validation.samples)
	levels(rf.prediction) = union(levels(rf.prediction),levels(l2.validation.outcomes))
	rf.l2.validation.outcomes = l2.validation.outcomes
	levels(rf.l2.validation.outcomes) = union(levels(rf.prediction),levels(l2.validation.outcomes))
	pf = performance(rf.prediction,rf.l2.validation.outcomes)
	results = rbind(results, c('RF.euc',pf$TP,pf$TN,pf$FP,pf$FN, pf$f1, pf$mcc, NA, NA, NA,''))	
	
	
	#RAW OTU 
	print('RAW OTU')
	#-------
	
	print('--rf')
	#grab the holdout
	#holdout.logic = 1:nrow(otus) %in% otu.sampler[inc:((inc+otu.part)-1)]
	holdout = otus[holdout.logic,]
	#grab the training set
	otu.training.samples = otus[!holdout.logic,]
	#grab training outcomes
	otu.training.outcomes = factor(map[rownames(otu.training.samples),variable] %in% positiveClasses)
	#grab test samples
	otu.validation.samples = holdout
	#grab test outcomes
	otu.validation.outcomes = factor(map[rownames(otu.validation.samples),variable] %in% positiveClasses)
	#SVM
	print('--svm')
	for(kern in c('vanilladot','rbfdot', 'polydot')){
		print(paste(kern,'...',sep=''))
	  otu.svm.model = invisible(tune( ranges = list(C=2^(-2:3)), kernel=kern, svm.model, otu.training.samples,otu.training.outcomes)$best.model)
	  svm.prediction = predict(otu.svm.model,otu.validation.samples)
		
		levels(svm.prediction) = union(levels(svm.prediction),levels(otu.validation.outcomes))
		svm.otu.validation.outcomes = otu.validation.outcomes
		levels(svm.otu.validation.outcomes) = union(levels(svm.prediction),levels(otu.validation.outcomes))
		pf = performance(svm.prediction,svm.otu.validation.outcomes)
		results = rbind(results, c(paste('SVM.',kern,sep=''),pf$TP,pf$TN,pf$FP,pf$FN, pf$f1, pf$mcc, NA, NA, NA, paste('C:',otu.svm.model$svm@param$C)))
	
	
	}
	#RF
	otu.rf = randomForest(otu.training.samples,otu.training.outcomes)
	rf.prediction = predict(otu.rf, otu.validation.samples)
	levels(rf.prediction) = union(levels(rf.prediction),levels(otu.validation.outcomes))
	rf.otu.validation.outcomes = otu.validation.outcomes
	levels(rf.otu.validation.outcomes) = union(levels(rf.prediction),levels(otu.validation.outcomes))
	pf = performance(rf.prediction,rf.otu.validation.outcomes)
	
	
	results = rbind(results, c('RF.otu',pf$TP,pf$TN,pf$FP,pf$FN, pf$f1, pf$mcc, NA, NA, NA, ''))
	return(results)
}
