library('ggplot2')
library('RColorBrewer')
result.columns = c('task','method','f1','f1rank','class.acc', 'classrank')
full.res = matrix(0,0,length(result.columns))
files = list.files("results/")

for(argfile in files[grep(".*args.txt",list.files("results/"))]){
  args = readLines(paste("results/",argfile,sep=''))
  outid = args[9]
  task = gsub(".*/","",outid)
  filename = paste(outid,'_results.txt', sep='')
  print(filename)
  fromFile = read.csv(filename)
  
  psums = fromFile$TP + fromFile$FN
  nsums = fromFile$TN + fromFile$FP
  denom = (psums > 0)+ (nsums > 0)
  pacc = fromFile$TP / psums
  nacc = fromFile$TN / nsums
  pacc[is.nan(pacc)] = 0
  nacc[is.nan(nacc)] = 0
  class.acc = (pacc+nacc) / denom 
  
  means = aggregate(f1 ~ method, data = fromFile, FUN=mean)
  cmeans = cbind(as.data.frame(fromFile$method), class.acc)
  colnames(cmeans) = c('method','class.acc')
  cmeans= aggregate(class.acc ~ method, data= cmeans, FUN=mean)
  means = cbind(means, rank(means$f1), cmeans$class.acc, rank(cmeans$class.acc))
  full.res = rbind(full.res, cbind(task,means))
}

colnames(full.res)= result.columns

#######################
# BOX PLOTS 
######################

# BOX PLOT BY TASK
  #F1 Score
png('task_f1_box.png',width=1280,height=1024)
myplot = ggplot(full.res, aes(x=reorder(task, f1, FUN=median),y=f1))
myplot = myplot + labs(y="F1 Score",x="Task")+ geom_boxplot() + geom_point()
myplot = myplot + coord_flip() + theme(axis.text.y=element_text(face="bold",size=14))
myplot = myplot + theme(axis.title = element_text(face="bold",size=16))
myplot
dev.off()

  # CLASS ACC
png('task_class.acc_box.png',width=1280,height=1024)
myplot = ggplot(full.res, aes(x=reorder(task, class.acc, FUN=median),y=class.acc, label=task))
myplot = myplot + geom_boxplot()+geom_point()
myplot = myplot+ theme(axis.text.y=element_text( face='bold', size=14)) 
myplot = myplot + labs(x="Classification Task",y="Class Accuracy")  +coord_flip()
myplot = myplot+ geom_point(data =mean.f1, aes(x=reorder(task,f1,FUN=mean),y=f1, label=round(f1,4)))
myplot = myplot + theme(axis.title = element_text(face="bold",size=16))
myplot
dev.off()

#BOX BY METHOD
   #F1 Score
png('method_f1_box.png',width=1280,height=1024)
myplot = ggplot(full.res, aes(x=reorder(method, f1, FUN=median),y=f1))
myplot = myplot + labs(y="F1 Score",x="Classifier")+ geom_boxplot() + geom_point()
myplot = myplot + coord_flip() + theme(axis.text.y=element_text(face="bold",size=14))
myplot = myplot + theme(axis.title = element_text(face="bold",size=16))
myplot
dev.off()

   #F1 RANK 
png('method_f1ranks_box.png',width=1280,height=1024)
myplot = ggplot(full.res, aes(x=reorder(method, f1rank, FUN=median),y=f1rank))
myplot = myplot+ coord_flip() +labs(x="Classifier",y="F1 Rank")+ geom_boxplot() 
myplot = myplot + geom_point() + theme(axis.text.y=element_text(face="bold",size=14))
myplot = myplot + theme(axis.title = element_text(face="bold",size=16))
myplot
dev.off()


#BOX BY METHOD
#try beeswarm
bs <- beeswarm(class.acc ~ method, data = full.res, method = 'swarm', pwcol=method )[, c(1, 2, 4, 6)]
colnames(bs) <- c("x", "y", "method", "class.acc") 

png('method_class_acc_box.png',width=1280,height=1024)
myplot = ggplot(full.res, aes(x=reorder(method, class.acc, FUN=median),y=class.acc))
myplot = myplot +labs(x="Classifier",y="Class Accuracy")+ geom_boxplot() + geom_point()
myplot = myplot + coord_flip() + theme(axis.text.y=element_text(face="bold",size=14))
myplot = myplot + theme(axis.title = element_text(face="bold",size=16))
myplot
dev.off()

png('method_class_ranks_box.png',width=1280,height=1024)
myplot = ggplot(full.res, aes(x=reorder(method, classrank, FUN=median),y=classrank))
myplot = myplot +labs(x="Classifier",y="Class Accuracy Rank")+ geom_boxplot() 
myplot = myplot + geom_point()+ coord_flip() + theme(axis.text.y=element_text(face="bold",size=14))
myplot = myplot + theme(axis.title = element_text(face="bold",size=16))
myplot
dev.off()


#######################
# BAR PLOTS
#####################
# BAR AVG F1 BY TASK
mean.f1 = aggregate(f1 ~ task, data = full.res, FUN=mean)
f1means = mean.f1$f1
names(f1means) = mean.f1$task
f1means = f1means[order(f1means)]
png('task_f1means_bar.png',width=1280,height=1024)
myplot= ggplot(mean.f1, aes(x=reorder(task,f1,FUN=mean),y=f1, label=round(f1,4)))
myplot = myplot+ labs(x="Classification Task",y="F1 Score (mean)") + coord_cartesian(ylim=c(0.2,1.2)) 
myplot= myplot+ geom_bar(stat="identity")+ geom_text(aes( y=round(f1,4)+0.1)) 

myplot = myplot +labs(x="Task",y=" Avg F1 Score")
myplot = myplot + coord_flip() + theme(axis.text.y=element_text(face="bold",size=14))
myplot = myplot + theme(axis.title = element_text(face="bold",size=16))
myplot
dev.off()

# BAR AVG RANK BY METHOD
mean.ranks = aggregate(cbind(f1,f1rank) ~ method, data = full.res, FUN=mean)
f1ranks = mean.ranks$f1rank
names(f1ranks) = mean.ranks$method
f1ranks = f1ranks[order(f1ranks)]
png('method_f1ranks_bar.png',width=1280,height=1024)
myplot= ggplot(mean.ranks, aes(x=reorder(method,f1rank,FUN=mean),y=f1rank, label=round(f1rank,2)))
myplot= myplot + coord_cartesian(ylim=c(10,40))  + geom_bar(stat="identity")
myplot= myplot + geom_text(aes(y=round(f1rank,2)+2))
myplot = myplot +labs(x="Classifier",y="Avg F1 Rank")
myplot = myplot + coord_flip() + theme(axis.text.y=element_text(face="bold",size=14))
myplot = myplot + theme(axis.title = element_text(face="bold",size=16))
myplot
dev.off()


# AVG BAR PLOT BY TASK
mean.class.acc = aggregate(class.acc ~ task, data = full.res, FUN=mean)
cmeans = mean.class.acc$class.acc
names(cmeans) = mean.class.acc$task
cmeans = cmeans[order(cmeans)]
png('task_class_acc_means_bar.png',width=1280,height=1024)
myplot= ggplot(mean.class.acc, aes(x=reorder(task,class.acc,FUN=mean),y=class.acc, label=round(class.acc,4)))
myplot = myplot + geom_bar(stat="identity")
myplot = myplot + geom_text(aes( y=round(class.acc,4)+0.1)) 
myplot = myplot +labs(x="Task",y=" Avg Class Accuracy")
myplot = myplot + coord_flip() + theme(axis.text.y=element_text(face="bold",size=14))
myplot = myplot + theme(axis.title = element_text(face="bold",size=16))
myplot
myplot
dev.off()


#CLASS ACC BY METHOD
mean.class.acc = aggregate(class.acc ~ method, data = full.res, FUN=mean)
cmeans = mean.class.acc$class.acc
names(cmeans) = mean.class.acc$method
cmeans = cmeans[order(cmeans)]
png('method_class_acc_means_bar.png',width=1280,height=1024)
myplot= ggplot(mean.class.acc, aes(x=reorder(method,class.acc,FUN=mean),y=class.acc, label=round(class.acc,4)))
myplot = myplot + geom_bar(stat="identity")
myplot = myplot + geom_text(aes( y=round(class.acc,4)+0.1)) 
myplot = myplot +labs(x="Classifier",y="Avg Class Accuracy")
myplot = myplot + coord_flip() + theme(axis.text.y=element_text(face="bold",size=14))
myplot = myplot + theme(axis.title = element_text(face="bold",size=16))
myplot
myplot
dev.off()


# AVG RANK BY METHOD
mean.ranks = aggregate(cbind(class.acc,classrank) ~ method, data = full.res, FUN=mean)
classranks = mean.ranks$classrank
names(classranks) = mean.ranks$method
classranks = classranks[order(classranks)]
png('method_class_ranks_bar.png',width=1280,height=1024)
myplot= ggplot(mean.ranks, aes(x=reorder(method,classrank,FUN=mean),y=classrank, label=round(classrank,4)))
myplot = myplot + geom_bar(stat="identity")+ geom_text(aes(y=round(classrank,4)+3))
myplot = myplot +labs(x="Classifier",y="Avg Class Accuracy Rank")
myplot = myplot + coord_flip() + theme(axis.text.y=element_text(face="bold",size=14))
myplot = myplot + theme(axis.title = element_text(face="bold",size=16))
myplot
myplot
dev.off()


# who came in first?
png('classifier_1st_f1ranks.png')
first.ranks = aggregate(f1rank ~ method, data=full.res[full.res$f1rank==1.0,c('f1rank','method')], FUN='sum')
myplot = ggplot(first.ranks, aes(x=reorder(method,f1rank,FUN=mean),y=f1rank, label=f1rank))
myplot = myplot +labs(x="Classifier",y="Number of 1st F1 Ranks")+ geom_bar(stat="identity")
myplot = myplot + geom_text(aes(y=(f1rank+1))) 
myplot = myplot + coord_flip() + theme(axis.text.y=element_text(face="bold",size=14))
myplot = myplot + theme(axis.title = element_text(face="bold",size=16))
myplot
dev.off()

png('classifier_1st_class_ranks.png')
first.class.ranks = aggregate(classrank ~ method, data=full.res[full.res$classrank==1.0,c('classrank','method')], FUN='sum')
myplot = ggplot(first.class.ranks, aes(x=reorder(method,classrank,FUN=mean),y=classrank, label=classrank))
myplot = myplot +labs(x="Classifier",y="Number of 1st Class Accuracy Ranks")+ geom_bar(stat="identity")
myplot = myplot + geom_text(aes(y=(classrank+1))) 
myplot = myplot + coord_flip() + theme(axis.text.y=element_text(face="bold",size=14))
myplot = myplot + theme(axis.title = element_text(face="bold",size=16))
myplot
dev.off()


write.csv(full.res,file='full.res.csv')