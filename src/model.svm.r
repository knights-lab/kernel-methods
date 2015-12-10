# wrapper for kernel based svm 
# allows best.tune calls in e1071 package, which expects an S3 object (ksvm returns S4)
require('kernlab')

"svm.model" <- function(x,y,kernel,...){
	kernels = c('matrix','vanilladot','rbfdot');
	kernel = kernels[kernel]
	model = list()
	if(kernel=='matrix'){
		x = as.kernelMatrix(x)
	}
	model[['kernel']] = kernel
	model[['svm']] <- ksvm(x,y,kernel=model$kernel,...)
	class(model) <- "svm.model"
	return(model)
}

# wrapper for ksvm predict function
"predict.svm.model" <- function(model,x){
	if(model$kernel == 'matrix'){
		#have to subset columns of kernel matrix to match SUPPORT VECTORS
		x = as.kernelMatrix(x[,SVindex(model$svm), drop=F])
	}
	return (predict(model$svm,x))
}
