# wrapper for kernel based svm 
# allows best.tune calls in e1071 package, which expects an S3 object (ksvm returns S4)
require('kernlab')

svm.model = function(x,y,kernel,...){
	rest.of.args = list(...)
	#print('before:')
	#print(dim(x))
	#kernels = c('custom','matrix','vanilladot','rbfdot');
	#kernel = kernels[kernel]
	model = list()
	if(kernel=='matrix'){
		x = as.kernelMatrix(x)
	}
	if(kernel=='custom'){
	  model[['customData']] = rest.of.args$kernelCustomData(x,model,...)
	  model[['customKernelTrain']] = rest.of.args$kernelFunTrain
		model[['customKernelPredict']] = rest.of.args$kernelFunPredict
		model[['fullDist']] = rest.of.args$fullDist
		x = as.kernelMatrix(model$customKernelTrain(x, model, ...))
		
	}
	model[['kernel']] = kernel
	#print(length(y))
	model[['svm']] <- ksvm(x,y,kernel=model$kernel,kpar=list(),...)
	class(model) <- "svm.model"
	return(model)
}

# wrapper for ksvm predict function
predict.svm.model = function(model,x,...){
	rest.of.args = list(...)
	if(model$kernel == 'matrix'){
		#have to subset columns of kernel matrix to match SUPPORT VECTORS
		x = as.kernelMatrix(x[,SVindex(model$svm), drop=F])
	}
	if(model$kernel=='custom'){
	  #print('running custom prediction')
		x = as.kernelMatrix(model$customKernelPredict(x, model, ...))
	}
	return (predict(model$svm,x))
}
