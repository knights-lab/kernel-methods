doOuterCV = function(n){
	cols = c('method','TP','TN','FP','FN','f1','matthews','class-wise','auc','observed_over_baseline','best_model')
	results = matrix(0,0,length(cols))
	#inc = 1 +  n * uni.part
	index = 1
	print (n)
	#UNIFRAC
	print('UNIFRAC')
	#----------
	#grab the holdout
	#holdout.logic = 1:nrow(uni.sim) %in% uni.sampler[inc:((inc+uni.part)-1)]
	holdout.logic = fold.ids==n
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
	uni.svm.model = invisible(tune( ranges = list(kernel=1:3,C=2^(-2:3)),svm.model, uni.training.samples,uni.training.outcomes)$best.model)
	print(paste('post svm tuning in fold ',n))
	#uni.svm.model = ksvm(as.kernelMatrix(uni.training.samples),uni.training.outcomes,kernel='matrix')
	svm.prediction = predict(uni.svm.model,uni.validation.samples)
	print(paste('post svm prediction in fold ',n))
	levels(svm.prediction) = union(levels(svm.prediction),levels(uni.validation.outcomes))
	levels(uni.validation.outcomes) = union(levels(svm.prediction),levels(uni.validation.outcomes))
	print(paste('post svm stuff in fold ',n))
	pf = performance(svm.prediction,uni.validation.outcomes)
	results = rbind(results, c('SVM.unifrac',pf$TP,pf$TN,pf$FP,pf$FN, pf$f1, pf$mcc, NA, NA, NA, paste('kernel:',uni.svm.model$kernel,'C:',uni.svm.model$svm@param$C)))
	#KNN
	print('--knn')
	holdout = uni.dist[holdout.logic,]
	#grab the training set
	uni.training.samples = uni.dist[!holdout.logic,]
	#limit cols to training rows (to simluate introduction of new data)
	uni.training.samples = uni.training.samples[,rownames(uni.training.samples)]
	#grab training outcomes
	uni.training.outcomes = factor(map[rownames(uni.training.samples),variable] %in% positiveClasses)
	#grab test samples
	uni.validation.samples = holdout[,rownames(uni.training.samples)]
	#grab test outcomes
	uni.validation.outcomes = factor(map[rownames(uni.validation.samples),variable] %in% positiveClasses)
	#uni.knn.model = knn.dist(uni.training.samples,uni.training.outcomes,k=10)
	uni.knn.model = tune(knn.dist,ranges = list(k=seq(1:20)), uni.training.samples,uni.training.outcomes)$best.model
	knn.prediction = as.factor(predict(uni.knn.model,rbind(uni.training.samples,uni.validation.samples)))
	levels(knn.prediction) = union(levels(knn.prediction),levels(uni.validation.outcomes))
	levels(uni.validation.outcomes) = union(levels(knn.prediction),levels(uni.validation.outcomes))
	pf = performance(knn.prediction,uni.validation.outcomes)
	
	results = rbind(results, c('KNN.unifrac',pf$TP,pf$TN,pf$FP,pf$FN, pf$f1, pf$mcc, NA, NA, NA, paste('k:',uni.knn.model$k)))
	#Unifrac RF
	print('--rf')
	uni.rf = randomForest(uni.training.samples,uni.training.outcomes)
	rf.prediction = predict(uni.rf, uni.validation.samples)
	levels(rf.prediction) = union(levels(rf.prediction),levels(uni.validation.outcomes))
	levels(uni.validation.outcomes) = union(levels(rf.prediction),levels(uni.validation.outcomes))
	pf = performance(rf.prediction,uni.validation.outcomes)
	results = rbind(results, c('RF.unifrac',pf$TP,pf$TN,pf$FP,pf$FN, pf$f1, pf$mcc, NA, NA, NA, ''))
	
	
	#pc.uni
	print('PC.UNI')
	#----------
	#grab the holdout
	#holdout.logic = 1:nrow(pc.uni) %in% uni.sampler[inc:((inc+uni.part)-1)]
	holdout.logic = fold.ids==n
	for(i in seq(2,20,2)){
	  print(paste("pc.uni k=",as.character(i)))
	  #pc.all is pcoa of unifrac distances
	  pc.uni = as.matrix(dist(pc.all[,1:i]))
  	pc.sim = 1-pc.uni
	  holdout = pc.sim[holdout.logic,]
  	#grab the training set
  	pc.training.samples = pc.sim[!holdout.logic,]
  	#limit cols to training rows (to simluate introduction of new data)
  	pc.training.samples = pc.training.samples[,rownames(pc.training.samples)]
  	#grab training outcomes
  	pc.training.outcomes = factor(map[rownames(pc.training.samples),variable] %in% positiveClasses)
  	#grab test samples
  	pc.validation.samples = holdout[,rownames(pc.training.samples)]
  	#grab test outcomes
  	pc.validation.outcomes = factor(map[rownames(pc.validation.samples),variable] %in% positiveClasses)
  	#SVM
  	print('--svm')
  	#tune params with cross-validation
  	#TODO make other spots match this syntax
  	pc = invisible(tune( ranges = list(kernel=1:3,C=2^(-2:3)),svm.model, pc.training.samples,pc.training.outcomes)$best.model)
  	print(paste('post svm tuning in fold ',n))
  	#uni.svm.model = ksvm(as.kernelMatrix(pc.training.samples),uni.training.outcomes,kernel='matrix')
  	svm.prediction = predict(uni.svm.model,pc.validation.samples)
  	print(paste('post svm prediction in fold ',n))
  	levels(svm.prediction) = union(levels(svm.prediction),levels(pc.validation.outcomes))
  	levels(pc.validation.outcomes) = union(levels(svm.prediction),levels(pc.validation.outcomes))
  	print(paste('post svm stuff in fold ',n))
  	pf = performance(svm.prediction,pc.validation.outcomes)
  	results = rbind(results, c(paste('SVM.pc.uni.',as.character(i),sep=''),pf$TP,pf$TN,pf$FP,pf$FN, pf$f1, pf$mcc, NA, NA, NA, paste('kernel:',uni.svm.model$kernel,'C:',uni.svm.model$svm@param$C)))
  	#KNN
  	print('--knn')
  	holdout = pc.uni[holdout.logic,]
  	#grab the training set
  	pc.training.samples = pc.uni[!holdout.logic,]
  	#limit cols to training rows (to simluate introduction of new data)
  	pc.training.samples = pc.training.samples[,rownames(pc.training.samples)]
  	#grab training outcomes
  	pc.training.outcomes = factor(map[rownames(pc.training.samples),variable] %in% positiveClasses)
  	#grab test samples
  	pc.validation.samples = holdout[,rownames(pc.training.samples)]
  	#grab test outcomes
  	pc.validation.outcomes = factor(map[rownames(pc.validation.samples),variable] %in% positiveClasses)
  	#uni.knn.model = knn.dist(pc.training.samples,uni.training.outcomes,k=10)
  	pc.knn.model = tune(knn.dist,ranges = list(k=seq(1:20)), pc.training.samples,pc.training.outcomes)$best.model
  	knn.prediction = as.factor(predict(pc.knn.model,rbind(pc.training.samples,pc.validation.samples)))
  	levels(knn.prediction) = union(levels(knn.prediction),levels(pc.validation.outcomes))
  	levels(pc.validation.outcomes) = union(levels(knn.prediction),levels(pc.validation.outcomes))
  	pf = performance(knn.prediction,pc.validation.outcomes)
  	
  	results = rbind(results, c(paste('KNN.pc.uni.',as.character(i),sep=''),pf$TP,pf$TN,pf$FP,pf$FN, pf$f1, pf$mcc, NA,NA,NA, paste('k:',uni.knn.model$k)))
  	#pc.uni RF
  	print('--rf')
  	pc.uni.rf = randomForest(pc.training.samples,uni.training.outcomes)
  	rf.prediction = predict(pc.uni.rf, pc.validation.samples)
  	levels(rf.prediction) = union(levels(rf.prediction),levels(pc.validation.outcomes))
  	levels(pc.validation.outcomes) = union(levels(rf.prediction),levels(pc.validation.outcomes))
  	pf = performance(rf.prediction,pc.validation.outcomes)
  		
  	results = rbind(results, c(paste('RF.pc.uni.',as.character(i),sep=''),pf$TP,pf$TN,pf$FP,pf$FN, pf$f1, pf$mcc, NA, NA, NA, ''))
	}
	
	
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
	#TODO make other spots match this syntax
	bc.svm.model = tune( ranges = list(kernel=1:3,C=2^(-2:3)),svm.model, bc.training.samples,bc.training.outcomes)$best.model
	#bc.svm.model = ksvm(as.kernelMatrix(bc.training.samples),bc.training.outcomes,kernel='matrix')
	svm.prediction = predict(bc.svm.model,bc.validation.samples)
	levels(svm.prediction) = union(levels(svm.prediction),levels(bc.validation.outcomes))
	levels(bc.validation.outcomes) = union(levels(svm.prediction),levels(bc.validation.outcomes))
	pf = performance(svm.prediction,bc.validation.outcomes)
	
	
	results = rbind(results, c('SVM.bc',pf$TP,pf$TN,pf$FP,pf$FN, pf$f1, pf$mcc, NA, NA, NA, paste('kernel:',bc.svm.model$kernel,'C:',bc.svm.model$svm@param$C)))
	#KNN
	print('--knn')
	holdout = bc.dist[holdout.logic,]
	#grab the training set
	bc.training.samples = bc.dist[!holdout.logic,]
	#limit cols to training rows (to simluate introduction of new data)
	bc.training.samples = bc.training.samples[,rownames(bc.training.samples)]
	#grab training outcomes
	bc.training.outcomes = factor(map[rownames(bc.training.samples),variable] %in% positiveClasses)
	#grab test samples
	bc.validation.samples = holdout[,rownames(bc.training.samples)]
	#grab test outcomes
	bc.validation.outcomes = factor(map[rownames(bc.validation.samples),variable] %in% positiveClasses)
	#bc.knn.model = knn.dist(bc.training.samples,bc.training.outcomes,k=10)
	bc.knn.model = tune(knn.dist,ranges = list(k=seq(1:20)), bc.training.samples,bc.training.outcomes)$best.model
	knn.prediction = as.factor(predict(bc.knn.model,rbind(bc.training.samples,bc.validation.samples)))
	levels(knn.prediction) = union(levels(knn.prediction),levels(bc.validation.outcomes))
	levels(bc.validation.outcomes) = union(levels(knn.prediction),levels(bc.validation.outcomes))
	pf = performance(knn.prediction,bc.validation.outcomes)
	
	
	results = rbind(results, c('KNN.bc',pf$TP,pf$TN,pf$FP,pf$FN, pf$f1, pf$mcc, NA, NA, NA, paste('k:',bc.knn.model$k)))
	#Bray curtis RF
	print('--rf')
	bc.rf = randomForest(bc.training.samples,bc.training.outcomes)
	rf.prediction = predict(bc.rf, bc.validation.samples)
	levels(rf.prediction) = union(levels(rf.prediction),levels(bc.validation.outcomes))
	levels(bc.validation.outcomes) = union(levels(rf.prediction),levels(bc.validation.outcomes))
	
	pf = performance(rf.prediction,bc.validation.outcomes)
	
	
	results = rbind(results, c('RF.bc',pf$TP,pf$TN,pf$FP,pf$FN, pf$f1, pf$mcc, NA, NA, NA, ''))
	#L2 OTU
	print('L2')
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
	#SVM
	print('--svm')
	#l2.svm.model = ksvm(l2.training.samples,l2.training.outcomes,kernel='vanilladot')
	l2.svm.model = tune(svm.model,l2.training.samples,l2.training.outcomes, ranges = list(kernel=c(1:3),C=c(0.25,1,2,4,8)))$best.model
	svm.prediction = predict(l2.svm.model,l2.validation.samples)
	
	levels(svm.prediction) = union(levels(svm.prediction),levels(l2.validation.outcomes))
	levels(l2.validation.outcomes) = union(levels(svm.prediction),levels(l2.validation.outcomes))
	pf = performance(svm.prediction,l2.validation.outcomes)
	
	
	results = rbind(results, c('SVM.l2',pf$TP,pf$TN,pf$FP,pf$FN, pf$f1, pf$mcc, NA, NA, NA,paste('kernel:',l2.svm.model$kernel,'C:',l2.svm.model$svm@param$C)))
	#KNN
	print('--knn')
	l2.knn.model = knn.dist(l2.training.samples,l2.training.outcomes,k=10)
	l2.knn.model = tune(knn.dist,ranges = list(k=seq(1:20)), l2.training.samples,l2.training.outcomes)$best.model
	knn.prediction = as.factor(predict(l2.knn.model,rbind(l2.training.samples,l2.validation.samples)))
	levels(knn.prediction) = union(levels(knn.prediction),levels(l2.validation.outcomes))
	levels(l2.validation.outcomes) = union(levels(knn.prediction),levels(l2.validation.outcomes))	
	pf = performance(knn.prediction,l2.validation.outcomes)
	
	
	results = rbind(results, c('KNN.l2',pf$TP,pf$TN,pf$FP,pf$FN, pf$f1, pf$mcc, NA, NA, NA,paste('k:',l2.knn.model$k)))
	#L2 RF
	print('--rf')
	l2.rf = randomForest(l2.training.samples,l2.training.outcomes)
	rf.prediction = predict(l2.rf, l2.validation.samples)
	levels(rf.prediction) = union(levels(rf.prediction),levels(l2.validation.outcomes))
	levels(l2.validation.outcomes) = union(levels(rf.prediction),levels(l2.validation.outcomes))
	pf = performance(rf.prediction,l2.validation.outcomes)
	
	
	
	results = rbind(results, c('RF.l2',pf$TP,pf$TN,pf$FP,pf$FN, pf$f1, pf$mcc, NA, NA, NA, ''))
	results
	#RAW OTU 
	print('RAW OTU')
	#-------
	#RF
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
	otu.rf = randomForest(otu.training.samples,otu.training.outcomes)
	rf.prediction = predict(otu.rf, otu.validation.samples)
	levels(rf.prediction) = union(levels(rf.prediction),levels(otu.validation.outcomes))
	levels(otu.validation.outcomes) = union(levels(rf.prediction),levels(otu.validation.outcomes))
	pf = performance(rf.prediction,otu.validation.outcomes)
	
	
	results = rbind(results, c('RF.otu',pf$TP,pf$TN,pf$FP,pf$FN, pf$f1, pf$mcc, NA, NA, NA, ''))
	results
}
