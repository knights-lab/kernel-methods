require('caret')
performance = function(x,y){
	result = list()
	res = confusionMatrix(x,y)
	TP = res$table[1,1]
	TN = res$table[2,2]
	FP = res$table[1,2]
	FN = res$table[2,1]
	result$TP = TP
	result$TN = FN
	result$FP = FP
	result$FN = FN
	precision = TP/(TP+FP)
	result$precision = precision
	recall = TP/(TP+FN)
	result$recall = recall
	result$f1 = 2 * precision * recall/ (precision + recall)
	result$mcc = (TP*TN - FP*FN) / sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))
	trapezoidal.sum = 0;
	#should be logical vectors, so adding / subtracting will coerce logicals to 0/1
for(i in 2:length(x)){
		trapezoidal.sum = trapezoidal.sum + (x[i] - x[i-1])*(y[i] + y[i-1])
	}
	gini = 1-trapezoidal.sum
	result$auc = (gini + 1)/2
	return(result)
}