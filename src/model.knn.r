# trains k-nearest neighbor classifier
"knn.dist" <- function(x,y,k=1){
	model = list()
	model[['train.ids']] <- rownames(x)
	model[['k']] <- min(k, length(y))
	model[['y']] <- as.factor(y)
	class(model) <- "knn.dist"
	return(model)
}

# performs k-nearest-neighbor classification 
# using a given distance matrix
# output is prediction vector
# d must be a distance matrix
# d must have row and column names
# d must contain the training samples and the test samples
"predict.knn.dist" <- function(model,x,type=c('response','prob')[1]){
	d <- x
	if(is.null(rownames(d)) || is.null(colnames(d))) stop('KNN distance matrix must have row/column names')
	
	test.ix <- !(rownames(d) %in% model$train.ids)
	train.ix <- (colnames(d) %in% model$train.ids)
	d <- d[test.ix,train.ix]
	d <- d[,model[['train.ids']]]
	#yhat <- factor(levels=levels(model$y))
	yhat <- c()
	probs <- matrix(0,nr=nrow(d),nc=length(levels(model$y)))
	rownames(probs) <- rownames(d)
	colnames(probs) <- levels(model$y)

	# one sample at a time
	if(!(nrow(d) > 0)) stop('No test data in distance matrix ')
	for(i in 1:nrow(d)){
		neighbor.order <- order(d[i,])
		counts <- table(model$y[neighbor.order[1:model$k]])
		# if tie, break randomly
		probs[i,names(counts)] <- counts / sum(counts)
		if(sum(counts == max(counts)) > 1){
			hits <- which(counts == max(counts))
			class.name <- names(counts)[sample(hits,1)]
		} else {
			class.name <- names(counts)[which.max(counts)]
		}
		yhat <- c(yhat,class.name)
	}

	names(yhat) <- rownames(d)

	if(type == 'response'){
		return(yhat)
	} else {
		return(list(yhat=yhat,probabilities=probs))
	}
}



# provides cross-validated errors and predictions
# for knn
"cv.knn.dist" <- function(d,y,k=1,nfolds=10,verbose=FALSE){
    if(nfolds==-1) nfolds <- length(y)
	folds <- balanced.folds(y,nfolds=nfolds)
	result <- list()
    result$y <- as.factor(y)
    result$probabilities <- matrix(0,nr=length(y),nc=length(unique(y)))
    colnames(result$probabilities) <- sort(unique(y))
    result$predicted <- result$y
    result$errs <- numeric(length(unique(folds)))

    # K-fold cross-validation
    for(fold in sort(unique(folds))){
        if(verbose) cat(sprintf('Fold %d...\n',fold))
        foldix <- folds == fold
        knn.obj <- knn.dist(d,y,train.ix=(1:length(y))[!foldix],k=k)
		result$probabilities[foldix,] <- knn.obj$probabilities
		result$predicted[foldix] <- knn.obj$yhat
        result$errs[fold] <- mean(result$predicted[foldix] != result$y[foldix])
    }
    result$confusion.matrix <- t(sapply(levels(y), function(level) table(result$predicted[y==level])))
    result$err <- mean(result$predicted != result$y)
    return(result)
}