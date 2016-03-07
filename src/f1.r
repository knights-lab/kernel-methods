f1 = function(TP, TN, FP, FN){
	result = list()
	precision = TP/(TP+FP)
	result$precision = precision
	recall = TP/(TP+FN)
	result$recall = recall
	result$f1 = 2 * precision * recall/ (precision + recall)
	return(result)
}