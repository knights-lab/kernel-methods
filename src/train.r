# training a model means:
# 1. tuning any parameters using cross-validation if needed
# 2. creating a single final model using the resulting parameters
"train.mwas" <- function(x, y, model.type=c('RF','SVM','KNN','NB','LR','RI')[1],
			rf.ntree=1000,
			rf.tune.mtry=TRUE,
			n.folds=10,
			knn.k=1,
			svm.c = 1,
			svm.kernel = 'rbfdot',
			error.fun=NULL,
			ri.tune.alpha=TRUE,
			ri.tune.spec=TRUE,
			ri.threshold.method='spec',
			ri.alpha=0.05,
			ri.spec=.8,
			verbose=FALSE){
	tunecontrol <- tune.control(cross=n.folds, error.fun=error.fun)
	if(model.type == 'RF'){
		if(is.logical(y)) y <- factor(y)
		if(rf.tune.mtry){
			model <- randomForest(x, y, ntree=rf.ntree)
		} else{
			ranges <- list(mtry=2^seq(1, floor(log2(ncol(x)))))
			model <- best.tune("randomForest", x, y, ranges=ranges, tunecontrol=tunecontrol)
		}
	} else if(model.type == 'KNN'){
		if(length(knn.k) == 1){
			model <- knn.dist(as.matrix(x), y, k=knn.k)
		} else {
			ranges <- list(k=knn.k)
			model <- best.tune("knn.dist", as.matrix(x), y, ranges=ranges, tunecontrol=tunecontrol)
		}
	} else if(model.type == 'SVM'){
		require('kernlab')
		if(length(svm.c) == 1){
			model = svm.model(x,y,C=svm.c,tunecontrol=tunecontrol)
		}else{
			ranges = list(C=svm.c)
			model = best.tune("svm.model",x,y,kernel=svm.kernel, ranges=ranges,tunecontrol=tunecontrol)
		}
	} else if(model.type == 'NB'){
	
	} else if(model.type == 'LR'){
	
	} else if(model.type == 'RI'){
		if(!is.logical(y)) stop('Risk index only accepts logical outcomes')
		if(ri.tune.alpha && !ri.tune.spec){
			# tune the p-value for inclusion of features
			ranges <- list(alpha=c(.001, .01, .05, .25))
# 			model  <- best.tune('get.risk.index', x, y, ranges=ranges, tune.control, threshold.spec=ri.spec, threshold.method=ri.threshold.method)
			model  <- best.risk.index(x, y, ranges=ranges, threshold.method=ri.threshold.method)
		} else if(ri.tune.spec && !ri.tune.alpha) {
			if(ri.threshold.method != 'spec') warning('Tuning of risk index specificity was requested with a different threshold choice method; using specificity as the threshold method.')
			ranges <- list(threshold.spec=seq(.75,1,.05))
# 			model  <- best.tune('get.risk.index', x, y, ranges=ranges, tune.control, alpha=ri.alpha, threshold.method='spec')
			model  <- best.risk.index(x, y, ranges=ranges, threshold.method=ri.threshold.method)
		} else if(ri.tune.spec && ri.tune.alpha) {
			if(ri.threshold.method != 'spec') warning('Tuning of risk index specificity was requested with a different threshold choice method; using specificity as the threshold method.')
			ranges <- list(alpha=c(.001, .01, .05, .25), threshold.spec=seq(.75,1,.05))
# 			model  <- best.tune('get.risk.index', x, y, ranges=ranges, tune.control, threshold.method='spec')
			model  <- best.risk.index(x, y, ranges=ranges, threshold.method=ri.threshold.method)
		} else {
# 			model <- get.risk.index(x, y, alpha=ri.alpha, threshold.spec=ri.spec, threshold.method=ri.threshold.method)
			model  <- best.risk.index(x, y, ranges=ranges, threshold.method=ri.threshold.method)
		}
	} else if(model.type == 'RI2'){
		if(!is.logical(y)) stop('Risk index only accepts logical outcomes')
		if(ri.tune.alpha && !ri.tune.spec){
			# tune the p-value for inclusion of features
			ranges <- list(alpha=c(.001, .01, .05, .25))
			model  <- best.tune.risk.index(x, y, ranges=ranges, tune.control, threshold.spec=ri.spec, threshold.method=ri.threshold.method)
		} else if(ri.tune.spec && !ri.tune.alpha) {
			if(ri.threshold.method != 'spec') warning('Tuning of risk index specificity was requested with a different threshold choice method; using specificity as the threshold method.')
			ranges <- list(threshold.spec=seq(.75,1,.05))
			model  <- best.tune('get.risk.index', x, y, ranges=ranges, tune.control, alpha=ri.alpha, threshold.method='spec')
		} else if(ri.tune.spec && ri.tune.alpha) {
			if(ri.threshold.method != 'spec') warning('Tuning of risk index specificity was requested with a different threshold choice method; using specificity as the threshold method.')
			ranges <- list(alpha=c(.001, .01, .05, .25), threshold.spec=seq(.75,1,.05))
			model  <- best.tune('get.risk.index', x, y, ranges=ranges, tune.control, threshold.method='spec')
		} else {
			model <- get.risk.index(x, y, alpha=ri.alpha, threshold.spec=ri.spec, threshold.method=ri.threshold.method)
		}
	}
	return(model)
}

# evaluating a model means:
# 1. training the model using a subset of data (which may involve cross-validation)
# 2. evaluating performance on another subset of data
# 3. repeating if necessary (cross validation)
"mwas.evaluate" <- function(x, y, eval.type=c('CV','bootstrap')[1],
			n.folds.eval=10, error.fun=NULL, nrepeat=1, ...){
	require('e1071')

	if(n.folds.eval == -1) n.folds.eval <- nrow(x)
	tunecontrol <- tune.control(cross=n.folds.eval, error.fun=error.fun, nrepeat=nrepeat)
	result <- tune("train.mwas", x, y, tunecontrol=tunecontrol, ...)
	return(result)
}

# TO-DO: add custom error functions for MCC, etc.
