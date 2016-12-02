PCoPD = function(P,D,ref.ids, new.ids){
  #Need to project testing samples onto training set PCoA space
  # use numerical optimization to find the PCoA position that minimizes
  # the error in the new distances to the training set from the original distances
  
  # P is existing n x k PCoA matrix with 
  # D is n+m x n distance matrix of original distances
  # ref.ids are indices into D that were a part of the original pcoa 
  # new.ids are indices into the rows of D that need to be added 
  obj.fun <- function(v, P, D, ref.ix, new.ix){
    new.dists <- apply(P,1,function(xx) sqrt(sum((xx-v)^2)))
    sq.err <- sum((D[new.ix,ref.ix] - new.dists)^2)
    return(sq.err)
  }
  
  converted.set = matrix(0,0,dim(P)[2])
  for(i in new.ids){
    # note: rows of P must be same order as train.ix rows of D
    #initial guess (vector of k 0's)
    v = matrix(0,1,dim(P)[2])[1,]
    # this is the numerical optimization
    converted.set = rbind(converted.set, optim(v, obj.fun, method="CG", P=P, D=D, ref.ix=ref.ids, new.ix=i)$par)
  }
  rownames(converted.set) = new.ids
  return(converted.set)
}