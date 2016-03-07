doOuterCV = function(n){
	results = matrix(0,10,7)
	colnames(results) = c('method','TP','TN','FP','FN','f1','best.model')
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
	res = confusionMatrix(svm.prediction,uni.validation.outcomes)
	f1scores = f1(res$table[1,1],res$table[2,2],res$table[1,2],res$table[2,1])
	results[index,] = c('SVM.unifrac',res$table[1,1],res$table[2,2],res$table[1,2],res$table[2,1], f1scores$f1, paste('kernel:',uni.svm.model$kernel,'C:',uni.svm.model$svm@param$C))
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
	res = confusionMatrix(knn.prediction,uni.validation.outcomes)
	f1scores = f1(res$table[1,1],res$table[2,2],res$table[1,2],res$table[2,1])
	
	index = index+1
	results[index,] = c('KNN.unifrac',res$table[1,1],res$table[2,2],res$table[1,2],res$table[2,1], f1scores$f1, paste('k:',uni.knn.model$k))
	#Unifrac RF
	uni.rf = randomForest(uni.training.samples,uni.training.outcomes)
	rf.prediction = predict(uni.rf, uni.validation.samples)
	levels(rf.prediction) = union(levels(rf.prediction),levels(uni.validation.outcomes))
	levels(uni.validation.outcomes) = union(levels(rf.prediction),levels(uni.validation.outcomes))
	res = confusionMatrix(rf.prediction,uni.validation.outcomes)
	f1scores = f1(res$table[1,1],res$table[2,2],res$table[1,2],res$table[2,1])
	
	index = index+1
	results[index,] = c('RF.unifrac',res$table[1,1],res$table[2,2],res$table[1,2],res$table[2,1], f1scores$f1, '')
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
	res = confusionMatrix(svm.prediction,bc.validation.outcomes)
	f1scores = f1(res$table[1,1],res$table[2,2],res$table[1,2],res$table[2,1])
	index = index+1
	results[index,] = c('SVM.bc',res$table[1,1],res$table[2,2],res$table[1,2],res$table[2,1], f1scores$f1, paste('kernel:',bc.svm.model$kernel,'C:',bc.svm.model$svm@param$C))
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
	res = confusionMatrix(knn.prediction,bc.validation.outcomes)
	f1scores = f1(res$table[1,1],res$table[2,2],res$table[1,2],res$table[2,1])
	index = index+1
	results[index,] = c('KNN.bc',res$table[1,1],res$table[2,2],res$table[1,2],res$table[2,1], f1scores$f1, paste('k:',bc.knn.model$k))
	#Bray curtis RF
	bc.rf = randomForest(bc.training.samples,bc.training.outcomes)
	rf.prediction = predict(bc.rf, bc.validation.samples)
	levels(rf.prediction) = union(levels(rf.prediction),levels(bc.validation.outcomes))
	levels(bc.validation.outcomes) = union(levels(rf.prediction),levels(bc.validation.outcomes))
	
	res = confusionMatrix(rf.prediction,bc.validation.outcomes)
	f1scores = f1(res$table[1,1],res$table[2,2],res$table[1,2],res$table[2,1])
	index = index+1
	results[index,] = c('RF.bc',res$table[1,1],res$table[2,2],res$table[1,2],res$table[2,1], f1scores$f1, '')
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
	f1scores = f1(res$table[1,1],res$table[2,2],res$table[1,2],res$table[2,1])
	levels(svm.prediction) = union(levels(svm.prediction),levels(l2.validation.outcomes))
	levels(l2.validation.outcomes) = union(levels(svm.prediction),levels(l2.validation.outcomes))
	res = confusionMatrix(svm.prediction,l2.validation.outcomes)
	f1scores = f1(res$table[1,1],res$table[2,2],res$table[1,2],res$table[2,1])
	index = index+1
	results[index,] = c('SVM.l2',res$table[1,1],res$table[2,2],res$table[1,2],res$table[2,1], f1scores$f1,paste('kernel:',l2.svm.model$kernel,'C:',l2.svm.model$svm@param$C))
	#KNN
	print('--knn')
	l2.knn.model = knn.dist(l2.training.samples,l2.training.outcomes,k=10)
	l2.knn.model = tune(knn.dist,ranges = list(k=seq(1:20)), l2.training.samples,l2.training.outcomes)$best.model
	knn.prediction = as.factor(predict(l2.knn.model,rbind(l2.training.samples,l2.validation.samples)))
	levels(knn.prediction) = union(levels(knn.prediction),levels(l2.validation.outcomes))
	levels(l2.validation.outcomes) = union(levels(knn.prediction),levels(l2.validation.outcomes))	
	res = confusionMatrix(knn.prediction,l2.validation.outcomes)
	f1scores = f1(res$table[1,1],res$table[2,2],res$table[1,2],res$table[2,1])
	index = index+1
	results[index,] = c('KNN.l2',res$table[1,1],res$table[2,2],res$table[1,2],res$table[2,1], f1scores$f1,paste('k:',l2.knn.model$k))
	#L2 RF
	print('--rf')
	l2.rf = randomForest(l2.training.samples,l2.training.outcomes)
	rf.prediction = predict(l2.rf, l2.validation.samples)
	levels(rf.prediction) = union(levels(rf.prediction),levels(l2.validation.outcomes))
	levels(l2.validation.outcomes) = union(levels(rf.prediction),levels(l2.validation.outcomes))
	res = confusionMatrix(rf.prediction,l2.validation.outcomes)
	f1scores = f1(res$table[1,1],res$table[2,2],res$table[1,2],res$table[2,1])
	
	index = index+1
	results[index,] = c('RF.l2',res$table[1,1],res$table[2,2],res$table[1,2],res$table[2,1], f1scores$f1, '')
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
	res = confusionMatrix(rf.prediction,otu.validation.outcomes)
	f1scores = f1(res$table[1,1],res$table[2,2],res$table[1,2],res$table[2,1])
	index = index+1
	results[index,] = c('RF.otu',res$table[1,1],res$table[2,2],res$table[1,2],res$table[2,1], f1scores$f1, '')
	results
}
