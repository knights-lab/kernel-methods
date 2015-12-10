library('e1071')
library('kernlab')
x <- matrix(rnorm(10000),100,100)

coeffs <- runif(10)
coeff.ix <- sample(1:30,10)
y <- sweep(x[,coeff.ix], 2, coeffs, '*'); y <- rowSums(y); y <- factor(y > median(y))

# try random forests
mrf <- randomForest(x,y)
mrf

# build an svm model
m <- ksvm(x,y)

# predict using that model and new data
yhat <- predict(m,x)

# confusion matrix of predictions
table(y, yhat)

# "tune" svm with no parameter sweep
cv <- tune(ksvm, x, y, tunecontrol=tune.control(best.model=FALSE))
cv$performances

# tune svm with parameter sweep for sigma and C using our wrapper
"my.rbf.ksvm" <- function(x, y, sigma, C) { ksvm(x, y, C=C, kpar=list(sigma=sigma))}
cv <- tune(my.rbf.ksvm, x, y, ranges=list(C=2^(-2:2),sigma=seq(1,2,.5)), tunecontrol=tune.control(best.model=FALSE))
cv$performances

# note: letting ksvm automatically select sigma worked better
cv <- tune(ksvm, x, y, ranges=list(C=2^(-5:5)), tunecontrol=tune.control(best.model=FALSE))
cv$performances


