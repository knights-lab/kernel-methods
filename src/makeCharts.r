library(ggplot)
files = list.files("results/")
for(argfile in files[grep(".*args.txt",list.files("results/"))]){
  args = readLines(paste("results/",argfile,sep=''))
  outid = args[9]
  
  filename = paste(outid,'_results.txt', sep='')
  fromFile = read.csv(filename)
  #png(paste(outid,'_results_box.png', sep=''),width=1024,height=1024)
  #boxplot(f1 ~ method, data = fromFile)
  #dev.off()
  means = aggregate(f1 ~ method ,data = fromFile, FUN='mean')
  
  means.save = means
  means.SVM.PC = rbind(means.save[grep("SVM\\.pc\\.uni",means.save$method),],means.save[grep("SVM\\.unifrac",means.save$method),])
  means.KNN.PC = rbind(means.save[grep("KNN\\.pc\\.uni",means.save$method),],means.save[grep("KNN\\.unifrac",means.save$method),])
  means.RF.PC  = rbind(means.save[grep("RF\\.pc\\.uni",means.save$method),], means.save[grep("RF\\.unifrac", means.save$method),])
  means.Rest = means.save[ !(means.save$method %in% means.save[grep("\\.pc\\.uni",means.save$method),"method"]),]
  performance.list = list("1"=means.SVM.PC, "2"=means.KNN.PC, "3"=means.RF.PC, "4" = means.Rest)
  png(paste(outid,'_results.png', sep=''),width=1024,height=1024)
  #plot.new()
  par(mfrow=c(2,2), oma=c(0,0,2,0))
  for(i in 1:4){
    means = performance.list[[i]];
    nums = as.numeric(gsub(".*pc\\.uni\\.", "",means$method))
    means = means[order(nums),]
    performance = means$f1
    names(performance) = means$method
    par(mar=c(8,4,2,2))
    midpoints = barplot(performance, las=2, ylim=c(min(means.save$f1)-0.02, max(means.save$f1)+0.02), xpd=FALSE,col=brewer.pal(8,'Set2'))
    text(midpoints,round(performance,4),labels=round(performance,4))
    axis(1,at=c(-1e6, 1e6),labels=NA)
  }
  mtext(paste(gsub(".*/","",outid)," (f1 score)"), outer=TRUE, cex=1.5)
  dev.off()
}