confusion.matrix = function(x,y,positive = TRUE){
  #x is predicted, y is true values
  if(positive %in% levels(x) || positive %in% levels(y)){
    #set positive to be the level that it maches
    positive = levels(y)[which(positive == levels(y))[1]]
  }
  comparison = x == y
  true.positives = sum(comparison[which(x == positive)])
  false.positives = sum((!comparison)[which(x == positive)])
  true.negatives = sum(comparison[which(x != positive)])
  false.negatives = sum((!comparison)[which(x != positive)])
  
  conf.mat = rbind(c(true.positives,false.positives),
                   c(false.negatives,true.negatives))
  return(conf.mat)
}

performance = function(x,y){
	result = list()
	res = confusion.matrix(x,y)
	TP = res[1,1]
	TN = res[2,2]
	FP = res[1,2]
	FN = res[2,1]
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
	#class wise accuracy for the classes that were present
	c.acc.pos = 0
	c.acc.neg = 0
	if((TP + FN) > 0){c.acc.pos = TP/(TP+FN)}
	if((TN + FP) > 0){c.acc.neg = TN/(TN+FP)}
	result$class.acc = c.acc.pos+c.acc.neg
	trapezoidal.sum = 0;
	#should be logical vectors, so adding / subtracting will coerce logicals to 0/1
  for(i in 2:length(x)){
		trapezoidal.sum = trapezoidal.sum + (x[i] - x[i-1])*(y[i] + y[i-1])
	}
	gini = 1-trapezoidal.sum
	result$auc = (gini + 1)/2
	return(result)
}