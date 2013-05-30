## This is a function for calculating a matrix of mahalanobis distances
## From: ftp://85.158.30.137/lib.stat.cmu.edu/R/CRAN/doc/vignettes/optmatch/mahalanobisMatching.pdf
myMH <- function(Tnms, Cnms, inv.cov, data) {
 stopifnot(!is.null(dimnames(inv.cov)[[1]]), dim(inv.cov)[1] >
 1, all.equal(dimnames(inv.cov)[[1]], dimnames(inv.cov)[[2]]),
 all(dimnames(inv.cov)[[1]] %in% names(data)))
 covars <- dimnames(inv.cov)[[1]]
 xdiffs <- as.matrix(data[Tnms, covars])
 xdiffs <- xdiffs - as.matrix(data[Cnms, covars])
 rowSums((xdiffs %*% inv.cov) * xdiffs)
}

MahalFrontier <- function(treatment, dataset, drop){
## the vector of matching covariates
  matchVars <-  colnames(dataset)[!(colnames(dataset) %in% drop)]
  rownames(dataset) <- seq(nrow(dataset))

  ## calculate the inverse covariance matrix
  icv <- solve(cov(dataset[, matchVars]))
  ## get the names of the treated
  trtnms <- row.names(dataset)[as.logical(dataset[[treatment]])]
  ## and the names of the control units
  ctlnms <- row.names(dataset)[!as.logical(dataset[[treatment]])]
  ## calculate the mahalanobis distances
  mdist <- outer(trtnms, ctlnms, FUN = myMH, inv.cov = icv, data = dataset)
  ## lable the distance matrix with the unit names
  dimnames(mdist) <- list(trtnms, ctlnms)


  ## calculate the length to the closest unit in the opposite treatment condition
  ## for each unit.
  minlist <- c(apply(mdist,1,min), apply(mdist,2,min))

  ## make a vector of all the unique minimum distances
  ## (this is the number of calculations we have to make)
  distvec <- rev(sort(unique(minlist)))

  ## this is a holder that saves the unit IDs for each matched data set
  matchedDataIDlist <- list()
  ## This is a holder that holds the information about the frontier and the
  ## ATE estimates
  outholder <- c()
  dropped <- c()
  imbalance <- c()

  ## Start a loop over the number of unique minimum distances
  for(i in 1:length(distvec)){
    ## We keep all units that have minimum distances <= this value
    currentThreshold <- distvec[i]
    ## This is the subset of minimum units that remain
    remainingMinimums <- minlist[minlist <= currentThreshold]

    this.drop <- seq(nrow(dataset))[!(seq(nrow(dataset)) %in% c(names(remainingMinimums), dropped))]
    dropped <- c(dropped, this.drop)

    ## calculate the Avg of all the minimum Mahalanobis discrepancies
    imbalance <- c(imbalance, rep(mean(remainingMinimums), length(this.drop)))
  }
  return(list(balance = imbalance, drops = dropped))
}






