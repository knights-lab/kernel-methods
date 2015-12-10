library('parallel')
clust = makeCluster((detectCores()-1))
n = 10
results = matrix(0,n*3,2)
doLoop = function(n){
	results [n * 3 + 1,1] = n
	results [n * 3 + 1,2] = 'first'
	results [n * 3 + 2,1] = n
	results [n * 3 + 2,2] = 'second'
	results [n * 3 + 3,1] = n
	results[n*3+3,2] = 'third'
	results
}
clusterExport(clust,"results")
res  = parLapply(clust, 0:(n-1), doLoop)
