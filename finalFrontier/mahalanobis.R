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



MahalFrontier <- function(treatment, dataset, drop, mdist){
## the vector of matching covariates
  matchVars <-  colnames(dataset)[!(colnames(dataset) %in% drop)]

  if(is.null(mdist)){
    ## calculate the inverse covariance matrix
    icv <- solve(cov(dataset[, matchVars]))
    ## get the names of the treated
    trtnms <- row.names(dataset)[as.logical(dataset[[treatment]])]
    ## and the names of the control units
    ctlnms <- row.names(dataset)[!as.logical(dataset[[treatment]])]
    ## calculate the mahalanobis distances if not specified
    mdist <- outer(trtnms, ctlnms, FUN = myMH, inv.cov = icv, data = dataset)                                                                                  
    dimnames(mdist) <- list(trtnms, ctlnms)
  }

  ## Check matrix
  if(!is.matrix(mdist)){stop("the mdist provided is not a matrix")}
  ## are all the rownames of mdist treated units in the data?
  if(sum(rownames(mdist) %in% rownames(dataset[dataset[[treatment]]==1,])) != nrow(mdist)){stop("the rownames of mdist do not match the names of the treated units in the data.")}
  ## are all the treated units in the data listed as rows in mdist?
  if(sum(rownames(dataset[dataset[[treatment]]==1,]) %in% rownames(mdist)) != nrow(mdist)){stop("the rownames of mdist do not match the names of the treated units in the data.")}
  ## are all the colnames of mdist control units in the data?
  if(sum(colnames(mdist) %in% rownames(dataset[dataset[[treatment]]==0,])) != ncol(mdist)){stop("the colnames of mdist do not match the names of the control units in the data.")}  
  ## are all the control units in the data listed as columns in mdist?
  if(sum(rownames(dataset[dataset[[treatment]]==0,]) %in% colnames(mdist)) != ncol(mdist)){stop("the colnames of mdist do not match the names of the control units in the data.")}

  ## calculate the length to the closest unit in the opposite treatment condition
  ## for each unit.
  minlist <- c(apply(mdist,1,min), apply(mdist,2,min))
  
  ##NEW -- try to calculate the parts for cemweights
  ## identify all obs that are mutually minimum to each other and call those strata
  ss <- list()
  for(i in 1:nrow(mdist)){
    ss[[length(ss)+1]] <- c(rownames(mdist)[i], names(which(mdist[i,] == minlist[rownames(mdist)[i]])))
  }
  for(i in 1:ncol(mdist)){
    ss[[length(ss)+1]] <- c(colnames(mdist)[i], names(which(mdist[,i] == minlist[colnames(mdist)[i]])))
  }
  ## Then combine all of them so that
  names(ss) <- as.character(1:length(ss))
  ss.names <- names(ss)
  for(i in 2:length(ss.names)){
    tmp <- ss[[ss.names[i]]]
    whichshare <- lapply(lapply(ss[1:(i-1)], function(x){tmp %in% x}), sum)
    ## if there is only one prior strata to combine with...
    if(sum(unlist(whichshare)) == 1){
      ss[[names(whichshare)[whichshare >0]]] <- unique(c(ss[[names(whichshare)[whichshare >0]]], tmp))
      ss[[ss.names[i]]] <- ""
    }
    ## if there is MORE THAN one prior strata to combine with...
    if(sum(unlist(whichshare)) > 1){
      ss[[ names(whichshare)[whichshare >0][1] ]] <- unique(c(unlist(ss[ names(whichshare)[whichshare >0] ]), tmp))
      ss[[ss.names[i]]] <- ""
      for(j in 2:sum(unlist(whichshare))){
        ss[[ names(whichshare)[whichshare >0][j] ]] <- ""
      }
    }
  }
  ## remove the empty holders
  ss <- ss[lapply(ss,function(x){sum(x=="")})==0]
  if( length(unlist(ss))!=nrow(dataset)){stop("The internal calculation of strata weights has messed up.")}
  ## then make an observation list with strata
  strataList <- rep(NA, nrow(dataset))
  names(strataList) <- rownames(dataset)
  for(i in 1:length(strataList)){
    strataList[i] <- names(ss)[unlist(lapply(ss, function(x){rownames(dataset)[i] %in% x}))]
  }

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
  matchedSampleSize <- c()
  wList <- list()

  ## Start a loop over the number of unique minimum distances
  for(i in 1:length(distvec)){
    ## We keep all units that have minimum distances <= this value
    currentThreshold <- distvec[i]
    ## This is the subset of minimum units that remain
    remainingMinimums <- minlist[minlist <= currentThreshold]
    ## we need the matched sample size
    matchedSampleSize <- c(matchedSampleSize, length(remainingMinimums))

    this.drop <- rownames(dataset)[!(rownames(dataset) %in% c(names(remainingMinimums), dropped))]
    dropped <- c(dropped, this.drop)

    ## calculate the Avg of all the minimum Mahalanobis discrepancies
    imbalance <- c(imbalance, mean(remainingMinimums))

    ## NEW -- calcalate a weights vector for this drop
    m_T <- table(dataset[[treatment]])[["1"]]
    m_C <- table(dataset[[treatment]])[["0"]]
    
    W <- rep(NA, nrow(dataset))
    names(W) <- rownames(dataset)
    ## fill in 1s for treated units that are still in
    W[(names(W) %in% rownames(mdist)) & (names(W) %in% names(remainingMinimums))] <- 1
    ## fill in 0s for all units that re out
    W[!(names(W) %in% names(remainingMinimums))] <- 0
    ## fill in weights for the rest
    cLeft <- W[(names(W) %in% colnames(mdist)) & (names(W) %in% names(remainingMinimums))]
    for(j in 1:length(cLeft)){
      sTab <- table(dataset[ names(strataList)[ strataList == strataList[names(cLeft[j])] ], ][[treatment]])
      m_T_s <- sTab["1"]
      m_C_s <- sTab["0"]
      W[names(cLeft[j])] <- (m_C/m_T)*(m_T_s/m_C_s)
    }
    wList[[i]] <- W 
  }
  return(list(balance = imbalance, drops = dropped, samplesize = matchedSampleSize, metric="Mahal", weights=wList))
}


## A version of Mahalanobis that creates j-to-k matched datasets
MahalFrontier2 <- function(treatment, dataset, drop, mdist, j=1, k=1){
  ## the vector of matching covariates
  matchVars <-  colnames(dataset)[!(colnames(dataset) %in% drop)]

  if(is.null(mdist)){
    ## calculate the inverse covariance matrix
    icv <- solve(cov(dataset[, matchVars]))
    ## get the names of the treated
    trtnms <- row.names(dataset)[as.logical(dataset[[treatment]])]
    ## and the names of the control units
    ctlnms <- row.names(dataset)[!as.logical(dataset[[treatment]])]
    ## calculate the mahalanobis distances if not specified
    mdist <- outer(trtnms, ctlnms, FUN = myMH, inv.cov = icv, data = dataset)                                                                                  
    dimnames(mdist) <- list(trtnms, ctlnms)
  }

  ## Check matrix
  if(!is.matrix(mdist)){stop("the mdist provided is not a matrix")}
  ## are all the rownames of mdist treated units in the data?
  if(sum(rownames(mdist) %in% rownames(dataset[dataset[[treatment]]==1,])) != nrow(mdist)){stop("the rownames of mdist do not match the names of the treated units in the data.")}
  ## are all the treated units in the data listed as rows in mdist?
  if(sum(rownames(dataset[dataset[[treatment]]==1,]) %in% rownames(mdist)) != nrow(mdist)){stop("the rownames of mdist do not match the names of the treated units in the data.")}
  ## are all the colnames of mdist control units in the data?
  if(sum(colnames(mdist) %in% rownames(dataset[dataset[[treatment]]==0,])) != ncol(mdist)){stop("the colnames of mdist do not match the names of the control units in the data.")}  
  ## are all the control units in the data listed as columns in mdist?
  if(sum(rownames(dataset[dataset[[treatment]]==0,]) %in% colnames(mdist)) != ncol(mdist)){stop("the colnames of mdist do not match the names of the control units in the data.")}

  ## calculate the length to the closest unit in the opposite treatment condition
  ## for each unit.
  minlist <- c(apply(mdist,1,min), apply(mdist,2,min))
  
  ##NEW -- try to calculate the parts for cemweights
  ## identify all obs that are mutually minimum to each other and call those strata
  ss <- list()
  for(i in 1:nrow(mdist)){
    ss[[length(ss)+1]] <- c(rownames(mdist)[i], names(which(mdist[i,] == minlist[rownames(mdist)[i]])))
  }
  for(i in 1:ncol(mdist)){
    ss[[length(ss)+1]] <- c(colnames(mdist)[i], names(which(mdist[,i] == minlist[colnames(mdist)[i]])))
  }
  ## Then combine all of them so that
  names(ss) <- as.character(1:length(ss))
  ss.names <- names(ss)
  for(i in 2:length(ss.names)){
    tmp <- ss[[ss.names[i]]]
    whichshare <- lapply(lapply(ss[1:(i-1)], function(x){tmp %in% x}), sum)
    ## if there is only one prior strata to combine with...
    if(sum(unlist(whichshare)) == 1){
      ss[[names(whichshare)[whichshare >0]]] <- unique(c(ss[[names(whichshare)[whichshare >0]]], tmp))
      ss[[ss.names[i]]] <- ""
    }
    ## if there is MORE THAN one prior strata to combine with...
    if(sum(unlist(whichshare)) > 1){
      ss[[ names(whichshare)[whichshare >0][1] ]] <- unique(c(unlist(ss[ names(whichshare)[whichshare >0] ]), tmp))
      ss[[ss.names[i]]] <- ""
      for(jj in 2:sum(unlist(whichshare))){
        ss[[ names(whichshare)[whichshare >0][jj] ]] <- ""
      }
    }
  }
  ## remove the empty holders
  ss <- ss[lapply(ss,function(x){sum(x=="")})==0]
  if( length(unlist(ss))!=nrow(dataset)){stop("The internal calculation of strata weights has messed up.")}
  ## then make an observation list with strata
  strataList <- rep(NA, nrow(dataset))
  names(strataList) <- rownames(dataset)
  for(i in 1:length(strataList)){
    strataList[i] <- names(ss)[unlist(lapply(ss, function(x){rownames(dataset)[i] %in% x}))]
  }

  ## remove strata that don't fit the j-to-k
  strataToDrop <- c()
  for(i in 1:length(unique(strataList))){
    obs <- names(strataList)[strataList==unique(strataList)[i]]
    tab <- table(dataset[obs,treatment])
    if(tab["1"] < j | tab["0"] < k){strataToDrop <- c(strataToDrop,unique(strataList)[i])}
  }
  ## Strata that have enough j and k    
  strataToKeep <- unique(strataList)[!(unique(strataList) %in% strataToDrop)]
  obsToDrop <- names(strataList)[strataList %in% strataToDrop]
  ## Then, go through each of the strata and trim it down to j-to-k
  for(i in 1:length(strataToKeep)){ 
    obs <- names(strataList)[strataList==strataToKeep[i]]
    tab <- table(dataset[obs,treatment])
    if(tab["1"] > j){
      extra <- tab["1"] - j
      tt <- dataset[[treatment]]
      names(tt) <- rownames(dataset)
      treatedNames <- names(tt[obs][tt[obs]==1])
      obsToDrop <- c(obsToDrop, names(rev(sort(minlist[treatedNames]))[1:extra]))
    }
    if(tab["0"] > k){
      extra <- tab["0"] - k
      tt <- dataset[[treatment]]
      names(tt) <- rownames(dataset)
      controlNames <- names(tt[obs][tt[obs]==0])
      obsToDrop <- c(obsToDrop, names(rev(sort(minlist[controlNames]))[1:extra]))
    }
  }
  goodObsStrata <- strataList[!(names(strataList) %in% obsToDrop)]
  ## Now re-calculate the strata distances
  strataMaxMin <- rep(NA,length(unique(goodObsStrata)))
  names(strataMaxMin) <- unique(goodObsStrata)
  for(i in 1:length(unique(goodObsStrata))){
    obs <- names(goodObsStrata)[goodObsStrata==unique(goodObsStrata)[i]]
    strataMaxMin[i] <- max(minlist[obs])
  }
    
  ## make a vector of all the unique minimum distances
  ## (this is the number of calculations we have to make)
  distvec <- rev(sort(unique(strataMaxMin)))

  ## this is a holder that saves the unit IDs for each matched data set
  matchedDataIDlist <- list()
  ## This is a holder that holds the information about the frontier and the
  ## ATE estimates
  outholder <- c()
  dropped <- c()
  imbalance <- c()
  matchedSampleSize <- c()
  wList <- list()
    
  ## Start a loop over the number of unique minimum distances
  for(i in 1:length(distvec)){
    ## We keep all units that have minimum distances <= this value
    currentThreshold <- distvec[i]
    ## This is the subset of minimum units that remain 
    keepObs <- names(goodObsStrata)[!(goodObsStrata %in% names(strataMaxMin)[strataMaxMin > currentThreshold])]   
    remainingMinimums <- minlist[keepObs]
    ## we need the matched sample size
    matchedSampleSize <- c(matchedSampleSize, length(remainingMinimums))

    this.drop <- rownames(dataset)[!(rownames(dataset) %in% c(names(remainingMinimums), dropped))]
    dropped <- c(dropped, this.drop)

    ## calculate the Avg of all the minimum Mahalanobis discrepancies
    imbalance <- c(imbalance, mean(remainingMinimums))
    ## make weights
    W <- rep(NA, nrow(dataset))
    names(W) <- rownames(dataset)
    W[dropped] <- 0
    W[!(names(W) %in% dropped)] <- 1   
    wList[[i]] <- W 
  }
  return(list(balance = imbalance, drops = dropped, samplesize = matchedSampleSize, metric="Mahalj2k", weights=wList))
}


##########################################################################################
## Old versions of functions

## this version is outdated as of roughly 14 June 2013 (this was the version before I added the code that makes weights)

#MahalFrontier <- function(treatment, dataset, drop, mdist){
## the vector of matching covariates
#  matchVars <-  colnames(dataset)[!(colnames(dataset) %in% drop)]
#
#  if(is.null(mdist)){
#    ## calculate the inverse covariance matrix
#    icv <- solve(cov(dataset[, matchVars]))
#    ## get the names of the treated
#    trtnms <- row.names(dataset)[as.logical(dataset[[treatment]])]
#    ## and the names of the control units
#    ctlnms <- row.names(dataset)[!as.logical(dataset[[treatment]])]
#    ## calculate the mahalanobis distances if not specified
#    mdist <- outer(trtnms, ctlnms, FUN = myMH, inv.cov = icv, data = dataset)                                                                         #         
#    dimnames(mdist) <- list(trtnms, ctlnms)
#  }
#
#  ## Check matrix
#  if(!is.matrix(mdist)){stop("the mdist provided is not a matrix")}
#  ## are all the rownames of mdist treated units in the data?
#  if(sum(rownames(mdist) %in% rownames(dataset[dataset[[treatment]]==1,])) != nrow(mdist)){stop("the rownames of mdist do not match the names of the #treated units in the data.")}
#  ## are all the treated units in the data listed as rows in mdist?
#  if(sum(rownames(dataset[dataset[[treatment]]==1,]) %in% rownames(mdist)) != nrow(mdist)){stop("the rownames of mdist do not match the names of the #treated units in the data.")}
#  ## are all the colnames of mdist control units in the data?
#  if(sum(colnames(mdist) %in% rownames(dataset[dataset[[treatment]]==0,])) != ncol(mdist)){stop("the colnames of mdist do not match the names of the control units in the data.")}  
#  ## are all the control units in the data listed as columns in mdist?
#  if(sum(rownames(dataset[dataset[[treatment]]==0,]) %in% colnames(mdist)) != ncol(mdist)){stop("the colnames of mdist do not match the names of the control units in the data.")}
#
#  ## calculate the length to the closest unit in the opposite treatment condition
#  ## for each unit.
#  minlist <- c(apply(mdist,1,min), apply(mdist,2,min))
#
#  ## make a vector of all the unique minimum distances
#  ## (this is the number of calculations we have to make)
#  distvec <- rev(sort(unique(minlist)))
#
#  ## this is a holder that saves the unit IDs for each matched data set
#  matchedDataIDlist <- list()
#  ## This is a holder that holds the information about the frontier and the
#  ## ATE estimates
#  outholder <- c()
#  dropped <- c()
#  imbalance <- c()
#  matchedSampleSize <- c()
#
#  ## Start a loop over the number of unique minimum distances
#  for(i in 1:length(distvec)){
#    ## We keep all units that have minimum distances <= this value
#    currentThreshold <- distvec[i]
#    ## This is the subset of minimum units that remain
#    remainingMinimums <- minlist[minlist <= currentThreshold]
#    ## we need the matched sample size
#    matchedSampleSize <- c(matchedSampleSize, length(remainingMinimums))
#
#    this.drop <- rownames(dataset)[!(rownames(dataset) %in% c(names(remainingMinimums), dropped))]
#    dropped <- c(dropped, this.drop)
#
#    ## calculate the Avg of all the minimum Mahalanobis discrepancies
#    imbalance <- c(imbalance, mean(remainingMinimums))
#  }
#  return(list(balance = imbalance, drops = dropped, samplesize = matchedSampleSize, metric="Mahal"))
#}


## A second version that creates 1-to-k matched datasets
#MahalFrontier2 <- function(treatment, dataset, drop, mdist, j, k){
## the vector of matching covariates
#  if(is.null(k)){stop("A k must be specified for j2k matching with mahalanobis.")}
#  if(is.null(j)){j <-1}
#  if(j!=1){stop("j must be 1 for mahalanobis with j2k = TRUE")}
#
#  require(optmatch)
#  matchVars <-  colnames(dataset)[!(colnames(dataset) %in% drop)]
#
#  if(is.null(mdist)){
#    ## calculate the inverse covariance matrix
#    icv <- solve(cov(dataset[, matchVars]))
#    ## get the names of the treated
#    trtnms <- row.names(dataset)[as.logical(dataset[[treatment]])]
#    ## and the names of the control units
#    ctlnms <- row.names(dataset)[!as.logical(dataset[[treatment]])]
#    ## calculate the mahalanobis distances if not specified
#    mdist <- outer(trtnms, ctlnms, FUN = myMH, inv.cov = icv, data = dataset)                                                                                  
#    dimnames(mdist) <- list(trtnms, ctlnms)
#  }
#
#  ## Check matrix
#  if(!is.matrix(mdist)){stop("the mdist provided is not a matrix")}
#  ## are all the rownames of mdist treated units in the data?
#  if(sum(rownames(mdist) %in% rownames(dataset[dataset[[treatment]]==1,])) != nrow(mdist)){stop("the rownames of mdist do not match the names of the treated units in the data.")}
#  ## are all the treated units in the data listed as rows in mdist?
#  if(sum(rownames(dataset[dataset[[treatment]]==1,]) %in% rownames(mdist)) != nrow(mdist)){stop("the rownames of mdist do not match the names of the treated units in the data.")}
#  ## are all the colnames of mdist control units in the data?
#  if(sum(colnames(mdist) %in% rownames(dataset[dataset[[treatment]]==0,])) != ncol(mdist)){stop("the colnames of mdist do not match the names of the control units in the data.")}  
#  ## are all the control units in the data listed as columns in mdist?
#  if(sum(rownames(dataset[dataset[[treatment]]==0,]) %in% colnames(mdist)) != ncol(mdist)){stop("the colnames of mdist do not match the names of the control units in the data.")}
#
#  ## make sure there are at least k controls per treated
#  if(nrow(mdist) > (k*ncol(mdist)) ){stop("k cannot be set higher than ",floor(ncol(mdist)/nrow(mdist))," with this dataset.",sep="")}
#
#  ## perform optimal pair matching
#  pm <- pairmatch(distance=mdist, controls = k, data=dataset, remove.unmatchables = T)
#  
#  ## drop the obs that aren't matched
#  pm <- na.omit(pm)
# 
#  ## calculate distances for the strata that do have matches
#  ustrata <- unique(na.omit(pm))
#  stratadists <- rep(NA,length(ustrata))
#  names(stratadists) <- ustrata
#  observationdists <- rep(NA, length(pm))
#  names(observationdists) <- names(pm)
#  for(i in 1:length(ustrata)){
#    this.strata <- names(pm)[pm==ustrata[i]]
#    this.strata.t <- this.strata[which(this.strata %in% rownames(mdist))]
#    this.strata.c <- this.strata[!(this.strata %in% this.strata.t)]
#    combdistholder <- rep(NA,length(this.strata))
#    names(combdistholder) <- c(this.strata.c,this.strata.t)
#    ## calculate distances from each control unit to the treated unit
#    for(j in 1:length(this.strata.c)){
#      combdistholder[j] <- mdist[this.strata.t, this.strata.c[j]]
#    }
#    ## and from the treate unit to the closest control
#    combdistholder[length(combdistholder)] <- min(combdistholder[-(length(combdistholder))])
#    observationdists[names(combdistholder)] <- combdistholder
#    stratadists[i] <- mean(combdistholder)
#  }
#
#  ## vector of the unique strata distances
#  ustratadists <- rev(sort(unique(stratadists)))
#  
#  ## this is a holder that saves the unit IDs for each matched data set
#  matchedDataIDlist <- list()
#  ## This is a holder that holds the information about the frontier and the
#  ## ATE estimates
#  outholder <- c()
#  dropped <- c()
#  imbalance <- c()
#  matchedSampleSize <- c()
#
#  ## Start a loop over the number of unique strata distances
#  for(i in 1:length(ustratadists)){
#    ## We keep all strata that have minimum distances <= this value
#    currentThreshold <- ustratadists[i]
#    ## This is the subset of strata that remain    
#    remainingStrata <- stratadists[stratadists <= currentThreshold]
#    ## These are the retained obs
#    remainingObs <- names(pm)[pm %in% names(remainingStrata)]
#    ## we need the matched sample size
#    matchedSampleSize <- c(matchedSampleSize, length(remainingObs))
#    ## which obs do we drop this time
#    this.drop <- rownames(dataset)[!(rownames(dataset) %in% c(remainingObs, dropped))]
#    dropped <- c(dropped, this.drop)
#
#
#    ## calculate the Avg of all the minimum Mahalanobis discrepancies
#    imbalance <- c(imbalance, mean(observationdists[remainingObs]))
#  }
#  return(list(balance = imbalance, drops = dropped, samplesize = matchedSampleSize, metric="Mahal2"))
#}


