getStrata <- function(treatment, dataset, drop, breaks=NULL){

  # Remove dropped observations
  dropped <- match(drop, colnames(dataset))
  if(length(dropped) > 0){
    dataset <- dataset[-dropped]
  }

  ## stuff borrowed from cem.main to add user defined breaks
  vnames <- colnames(dataset)
  nv <- dim(dataset)[2]
  mycut <- vector(nv, mode="list")
  names(mycut) <- vnames
  for (i in 1:nv) {	
    tmp <- reduceVar(dataset[[i]], breaks[[vnames[i]]])
    dataset[[i]] <- tmp$x
    mycut[[vnames[i]]] <- tmp$breaks
  }

  # Calculate strata
  strata <- stratify(dataset)
  return(list(strata=strata, mycut=mycut))
}
