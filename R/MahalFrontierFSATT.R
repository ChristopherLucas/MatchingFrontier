MahalFrontierFSATT <- function(treatment, dataset, drop, mdist = NULL){
## the vector of matching covariates
  matchVars <-  colnames(dataset)[!(colnames(dataset) %in% drop)]
  # Get distance matrix
  if(is.null(mdist)){mdist <- calculateMdist(dataset, treatment, matchVars)}
  # Check distance matrix
  checkMatrix(mdist, dataset, treatment)

  ## calculate the length to the closest unit in the opposite treatment condition
  ## for each unit.
  minlist <- c(apply(mdist,1,min), apply(mdist,2,min))
   
  strataList <- makeStrata(mdist, dataset, minlist)
  
  ## make a vector of all the unique minimum distances
  ## (this is the number of calculations we have to make)
  distvec <- rev(sort(unique(minlist)))

  return(frontierLoop(dataset, treatment, distvec, minlist, mdist, strataList))
}
