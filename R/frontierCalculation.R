#####################
# CONTROL FUNCTIONS #
#####################

# These are the functions that users are supposed to call. 

# This gets the frontier
makeFrontier <- function(treatment, dataset, drop, mdist = NULL, QOI, metric, ratio){
    if(sum(is.na(dataset)) > 0) stop('ERROR: Dataframe has missing values')
    if(QOI == 'FSATT' & metric == 'Mahal' & ratio == 'variable'){
        return(MahalFrontierFSATT(treatment=treatment, dataset, drop, mdist))
    }
    if(QOI == 'SATT' & metric == 'L1' & ratio == 'fixed'){
        return(L1FrontierSATT(treatment=treatment, dataset, drop))
    }
    if(QOI == 'FSATT' & metric == 'L1' & ratio == 'variable'){
        return(L1FrontierCEM(treatment=treatment, dataset, drop))
    }
}
 
## A function to estimate causal effects
frontierEst <- function(frontierObject, dataset, myform=NULL, treatment=NULL, estCall=NULL){

  ## Mahal
  if(frontierObject$metric=="Mahal" | frontierObject$metric=="Mahalj2k" | frontierObject$metric=="L1w"){
    # Causal Effects
    effectholder <- c()
    seholder <- c() 

    cat("Calculating estimates along the frontier\n")
    pb <- txtProgressBar(min=1,max=length(frontierObject$balance),initial = 1, style = 3)
    ## some warnings 
    if(is.null(treatment)){stop("\"treatment\" must be specified (as a string).")}
        
    for(i in 1:length(frontierObject$balance)){
      setTxtProgressBar(pb, i)
      ## how far through drops do we go?
#      dropseq <- (nrow(dataset)-frontierObject$samplesize[1]):(nrow(dataset)-frontierObject$samplesize[i])
      ## If there isn't a model specified, we just do linear regression with the formula
      if(is.null(estCall)){
        dataset$myw <- frontierObject$weights[[i]][rownames(dataset)]
#        m1 <- lm(myform, data=dataset[!(rownames(dataset) %in% frontierObject$drops[dropseq]),])
        m1 <- lm(myform, data=dataset, weights=myw)
        effectholder <- c(effectholder, summary(m1)$coeff[treatment,1])
        seholder <- c(seholder, summary(m1)$coeff[treatment,2])
      }
      ## if estCall is specified, then we just take whatever quantity of interest it is
      if(!is.null(estCall)){
        est <- estCall(dataset=dataset, weights=frontierObject$weights[[i]][rownames(dataset)])
        effectholder <- c(effectholder, est$effect)
        seholder <- c(seholder, est$se)
      }
    }
    close(pb)

    q <- data.frame(x = nrow(dataset)-frontierObject$samplesize)
    q$mean <- effectholder
    q$sd <- seholder
  }

  ## L1
  if(frontierObject$metric=="L1"){
    # Causal Effects
    effectholder <- c()
    seholder <- c() 

    cat("Calculating estimates along the frontier\n")
    pb <- txtProgressBar(min=1,max=length(frontierObject$balance),initial = 1, style = 3)

    for(i in 1:length(frontierObject$drops)){
      setTxtProgressBar(pb, i)
      if(is.null(estCall)){
        m1 <- lm(myform, data=dataset[!(rownames(dataset)  %in% frontierObject$drops[1:i]),], )
        effectholder <- c(effectholder, summary(m1)$coeff[treatment,1])
        seholder <- c(seholder, summary(m1)$coeff[treatment,2])
      }
      if(!is.null(estCall)){
        est <- estCall(data=dataset[!(rownames(dataset)  %in% frontierObject$drops[1:i]),], weights=NULL)
        effectholder <- c(effectholder, est$effect)
        seholder <- c(seholder, est$se)
      }     
    }
    close(pb)

    q <- data.frame(x = seq(1, length(frontierObject$drops)))

    q$mean <- effectholder
    q$sd <- seholder

  }
  return(q)
}

generateDataset <- function(finalFrontierObject, dataset, number.dropped){
  ## If L1 frontier
  if(finalFrontierObject$metric=="L1"){
    keep <- !(nrow(dataset) %in% finalFrontierObject$drops[0:number.dropped])
    return(dataset[keep,])
  } 
  ## If Mahalanobis frontier
  if(finalFrontierObject$metric=="Mahal" | finalFrontierObject$metric=="Mahalj2k"){
    if(number.dropped > length(finalFrontierObject$drops)){stop(paste("number.dropped must be less than ",length(finalFrontierObject $drops),".",sep=""))}
    if(number.dropped %in% (nrow(dataset)-finalFrontierObject$samplesize)){
      dataset$w = finalFrontierObject$weights[[which((nrow(dataset)-finalFrontierObject$samplesize) == number.dropped)]]
      keep <- !(rownames(dataset) %in% finalFrontierObject$drops[0:number.dropped])
      return(dataset[keep,])
    } else {
      ## calculate the nearest two options
      min2 <- sort(abs((nrow(dataset)-finalFrontierObject$samplesize) - number.dropped))[1]
      suggestions <- (nrow(dataset)-finalFrontierObject$samplesize)[which(abs((nrow(dataset)-finalFrontierObject$samplesize) - number.dropped) %in%  min2)]
      stop(paste("There is no dataset on the frontier with ",number.dropped," dropped observations.\n  The closest options for number.dropped  are",paste(paste(suggestions[1:(length(suggestions)-1)],collapse=", ")," or ",tail(suggestions,1),".",sep="")))
    }
  }
}

######################
# PLOTTING FUNCTIONS #
######################

# Make some pretty pictures
# function standardizes everything to 0 - 1
range01 <- function(x){
  if(min(x) == max(x)){
    x <- rep(0, length(x))
    return(x)
  }
  (x-min(x))/(max(x)-min(x))
}

# Function for combining plots made with ggplot
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

plotFrontier <- function(frontierObject, dataset, frontierEstObject=NULL, zoom = NULL, drop=NULL){
  if(frontierObject$metric == 'L1'){   
    starting.index <- 1
    ending.index <- length(frontierObject$drops)
    if(!is.null(zoom)){
      starting.index <- zoom[1]
      ending.index <- tail(zoom, 1)
    }
    
    # Frontier
    df <- data.frame(x = seq(starting.index, ending.index), y = frontierObject$balance[starting.index:ending.index])
    
    p1 <- ggplot(df, aes(x=x, y=y)) +
      geom_line() +
      xlab("Number of Observations Dropped") +
      ylab("Imbalance") +
      ggtitle("Imbalance Frontier")   
  }

  if(frontierObject$metric=="Mahal" | frontierObject$metric=="Mahalj2k" | frontierObject$metric=="L1w"){
    
    starting.index <- 1
    ending.index <- length(frontierObject$balance)
    if(!is.null(zoom)){
      ## I made a warning for zoom
      if(zoom[1]<0 | tail(zoom, 1) > length(frontierObject$drops)){stop(paste("zoom must be between 0 and ",length(frontierObject$drops)," for this data  set.",sep=""))}
    ## get the index corresponding with the requested number of obs to remove for the start of the zoom
      starting.index <- which(abs((frontierObject$samplesize[1] - frontierObject$samplesize)-zoom[1]) == min(abs((frontierObject$samplesize[1] -  frontierObject$samplesize)-zoom[1])))[1]
      ## get the index corresponding with the requested number of obs to remove for the end of the zoom
      ending.index <- tail( which(abs((frontierObject$samplesize[1] - frontierObject$samplesize)-tail(zoom, 1)) == min(abs((frontierObject$samplesize[1]  - frontierObject$samplesize)-tail(zoom, 1)))), 1)
    }
  
  # Frontier
  #df <- data.frame(x = seq(starting.index, ending.index), y = frontierObject$balance[starting.index:ending.index])
    df <- data.frame(x = nrow(dataset)-frontierObject$samplesize[starting.index:ending.index], 
                     y = frontierObject$balance[starting.index:ending.index])
  
    p1 <- ggplot(df, aes(x=x, y=y)) +
      geom_line() + geom_point() +
      xlab("Number of Observations Dropped") +
      ylab("Imbalance") +
      ggtitle("Imbalance Frontier")
  }
  return(p1)
}

plotEffects <- function(frontierObject, dataset, frontierEstObject=NULL, zoom = NULL, drop=NULL){
  if(frontierObject$metric == 'L1'){   
    starting.index <- 1
    ending.index <- length(frontierObject$drops)
    if(!is.null(zoom)){
      starting.index <- zoom[1]
      ending.index <- tail(zoom, 1)
    }
    if(!is.null(frontierEstObject)){
      q <- frontierEstObject[starting.index:ending.index,]
      eb <- aes(ymax = mean + sd * 1.96, ymin = mean - sd * 1.96)
      
      p2 <- ggplot(data = q, aes(x = x, y = mean)) + 
        geom_line(size = 1) + geom_point() +
        geom_ribbon(eb, alpha = 0.5) +
        xlab("Number of Observations Dropped") +
        ylab("Effect Size and SEs")
    }
  }
  if(frontierObject$metric=="Mahal" | frontierObject$metric=="Mahalj2k" | frontierObject$metric=="L1w"){
    
    starting.index <- 1
    ending.index <- length(frontierObject$balance)
    if(!is.null(zoom)){
      ## I made a warning for zoom
      if(zoom[1]<0 | tail(zoom, 1) > length(frontierObject$drops)){stop(paste("zoom must be between 0 and ",length(frontierObject$drops)," for this data  set.",sep=""))}
      ## get the index corresponding with the requested number of obs to remove for the start of the zoom
      starting.index <- which(abs((frontierObject$samplesize[1] - frontierObject$samplesize)-zoom[1]) == min(abs((frontierObject$samplesize[1] -  frontierObject$samplesize)-zoom[1])))[1]
      ## get the index corresponding with the requested number of obs to remove for the end of the zoom
      ending.index <- tail( which(abs((frontierObject$samplesize[1] - frontierObject$samplesize)-tail(zoom, 1)) == min(abs((frontierObject$samplesize[1]  - frontierObject$samplesize)-tail(zoom, 1)))), 1)
    }
      # Causal Effects
    if(!is.null(frontierEstObject)){
      q <- frontierEstObject[starting.index:ending.index,]
      eb <- aes(ymax = mean + sd * 1.96, ymin = mean - sd * 1.96)
      
      p2 <- ggplot(data = q, aes(x = x, y = mean)) + 
        geom_line(size = 1) + geom_point() +
        geom_ribbon(eb, alpha = 0.5) +
        xlab("Number of Observations Dropped") +
        ylab("Effect Size and SEs")
    }
  }
  return(p2)
}

plotMeans <- function(frontierObject, dataset, frontierEstObject=NULL, zoom = NULL, drop=NULL){
  if(frontierObject$metric == 'L1'){   
    starting.index <- 1
    ending.index <- length(frontierObject$drops)
    if(!is.null(zoom)){
      starting.index <- zoom[1]
      ending.index <- tail(zoom, 1)
    }
    covs <- colnames(dataset)[!(colnames(dataset) %in% drop)]
    
    for(col in covs){
      dataset[,colnames(dataset) == col] <- range01(dataset[,colnames(dataset) == col])
    }
    
    covs.mat <- matrix(nrow = 0, ncol = length(covs), byrow = FALSE)
    colnames(covs.mat) <- covs
    
    for(i in 1:length(frontierObject$drops)){
      iter.dat <- dataset[!(rownames(dataset) %in% frontierObject$drops[1:i]),]
      new.row <- c()
      for(c in colnames(covs.mat)){
        new.row <- c(new.row, mean(iter.dat[,c]))
      }
      covs.mat <- rbind(covs.mat, new.row)
    }
    rownames(covs.mat) <- seq(nrow(covs.mat))
    
    data.long <- melt(covs.mat[starting.index:ending.index,])
    
    p3 <- ggplot(data=data.long,
                 aes(x=Var1, y=value, colour=Var2)) +
                 geom_line() +
                 xlab("Number of Observations Pruned") +
                 ylab("Standardized Mean Value") +
                 opts(legend.position="bottom") +
                 theme(legend.title=element_blank())
  }

  if(frontierObject$metric=="Mahal" | frontierObject$metric=="Mahalj2k" | frontierObject$metric=="L1w"){
    
    starting.index <- 1
    ending.index <- length(frontierObject$balance)
    if(!is.null(zoom)){
      ## I made a warning for zoom
      if(zoom[1]<0 | tail(zoom, 1) > length(frontierObject$drops)){stop(paste("zoom must be between 0 and ",length(frontierObject$drops)," for this data  set.",sep=""))}
      ## get the index corresponding with the requested number of obs to remove for the start of the zoom
      starting.index <- which(abs((frontierObject$samplesize[1] - frontierObject$samplesize)-zoom[1]) == min(abs((frontierObject$samplesize[1] -  frontierObject$samplesize)-zoom[1])))[1]
      ## get the index corresponding with the requested number of obs to remove for the end of the zoom
      ending.index <- tail( which(abs((frontierObject$samplesize[1] - frontierObject$samplesize)-tail(zoom, 1)) == min(abs((frontierObject$samplesize[1]  - frontierObject$samplesize)-tail(zoom, 1)))), 1)
    }
    covs <- colnames(dataset)[!(colnames(dataset) %in% drop)]
    
    for(col in covs){
      dataset[,colnames(dataset) == col] <- range01(dataset[,colnames(dataset) == col])
    }
    
    covs.mat <- matrix(nrow = 0, ncol = length(covs), byrow = FALSE)
    colnames(covs.mat) <- covs
    
    for(i in 1:length(frontierObject$balance)){
      ## how far through drops do we go?
      dropseq <- (nrow(dataset)-frontierObject$samplesize[1]):(nrow(dataset)-frontierObject$samplesize[i])
      iter.dat <- dataset[!(rownames(dataset) %in% frontierObject$drops[dropseq]),]
      new.row <- c()
      for(c in colnames(covs.mat)){
        new.row <- c(new.row, mean(iter.dat[,c]))
      }
      covs.mat <- rbind(covs.mat, new.row)
    }
                                        #rownames(covs.mat) <- seq(nrow(covs.mat))
    rownames(covs.mat) <- nrow(dataset) - frontierObject$samplesize

    data.long <- melt(covs.mat[starting.index:ending.index,])
    
    p3 <- ggplot(data=data.long,
                 aes(x=Var1, y=value, colour=Var2)) +
                   geom_line() + 
                     xlab("Number of Observations Dropped") +
                       ylab("Standardized Mean Value") +
                         opts(legend.position="bottom") +
                           theme(legend.title=element_blank())    
  }
  return(p3)
}

frontierMultiPlot <- function(frontierObject, dataset, frontierEstObject=NULL, zoom = NULL, drop=NULL){
  p1 <- plotFrontier(frontierObject, dataset, frontierEstObject, zoom, drop)
  p2 <- plotEffects(frontierObject, dataset, frontierEstObject, zoom, drop)
  p3 <- plotMeans(frontierObject, dataset, frontierEstObject, zoom, drop)
  p.final <- multiplot(p1, p2, p3, cols=1)
  return(p.final)
}
  
######################
# FRONTIER FUNCTIONS #
######################

myMH <- function(Tnms, Cnms, inv.cov, dataset) {
 stopifnot(!is.null(dimnames(inv.cov)[[1]]), dim(inv.cov)[1] >
 1, all.equal(dimnames(inv.cov)[[1]], dimnames(inv.cov)[[2]]),
 all(dimnames(inv.cov)[[1]] %in% names(dataset)))
 covars <- dimnames(inv.cov)[[1]]
 xdiffs <- as.matrix(dataset[Tnms, covars])
 xdiffs <- xdiffs - as.matrix(dataset[Cnms, covars])
 rowSums((xdiffs %*% inv.cov) * xdiffs)
}

checkMatrix <- function(mdist, dataset, treatment){
    if(!is.matrix(mdist)){stop("the mdist provided is not a matrix")}
    ## are all the rownames of mdist treated units in the data?
    if(sum(rownames(mdist) %in% rownames(dataset[dataset[[treatment]]==1,])) != nrow(mdist)){stop("the rownames of mdist do not match the names of the treated units in the data.")}
    ## are all the treated units in the data listed as rows in mdist?
    if(sum(rownames(dataset[dataset[[treatment]]==1,]) %in% rownames(mdist)) != nrow(mdist)){stop("the rownames of mdist do not match the names of the treated units in the data.")}
    ## are all the colnames of mdist control units in the data?
    if(sum(colnames(mdist) %in% rownames(dataset[dataset[[treatment]]==0,])) != ncol(mdist)){stop("the colnames of mdist do not match the names of the control units in the data.")}  
    ## are all the control units in the data listed as columns in mdist?
    if(sum(rownames(dataset[dataset[[treatment]]==0,]) %in% colnames(mdist)) != ncol(mdist)){stop("the colnames of mdist do not match the names of the control units in the data.")}
}

calculateMdist <- function(dataset, treatment, matchVars){
    ## calculate the inverse covariance matrix
    icv <- solve(cov(dataset[, matchVars]))
    ## get the names of the treated
    trtnms <- row.names(dataset)[as.logical(dataset[[treatment]])]
    ## and the names of the control units
    ctlnms <- row.names(dataset)[!as.logical(dataset[[treatment]])]
    ## calculate the mahalanobis distances if not specified
    mdist <- outer(trtnms, ctlnms, FUN = myMH, inv.cov = icv, data = dataset)
    dimnames(mdist) <- list(trtnms, ctlnms)
    return(mdist)
}

frontierLoop <- function(dataset, treatment, distvec, minlist, mdist, strataList){
    ## Series of holders to store info about the frontier
    outholder <- c();dropped <- c();imbalance <- c(); matchedSampleSize <- c();wList <- list()

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

        ## fill in 0s for all units that are out
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
        if(sum(W != 0, na.rm=TRUE) < 400) {break}
    }
    return(list(balance = imbalance, drops = dropped, samplesize = matchedSampleSize, metric="Mahal", weights=wList))
}

makeStrata <- function(mdist, dataset, minlist){
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
    return(strataList)
}

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

######################
# END OF STUFF FOR MAHALANOBIS - BEGINNING OF L1
######################

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

## the original reduce.var from cem
reduceVar <- function(x, breaks=NULL){
	if(is.numeric(x) | is.integer(x)){
	 if(is.null(breaks)){
	  breaks <- "sturges"
	  }
	 if(is.character(breaks)){
       breaks <- match.arg(tolower(breaks), c("sturges", 
                "fd", "scott", "ss"))
            breaks <- switch(breaks, sturges = nclass.Sturges(x), 
                 fd = nclass.FD(x), 
				 scott = nclass.scott(x), 
				 ss = nclass.ss(x),
                stop("unknown 'breaks' algorithm"))
        }
	 if(length(breaks) > 0){
		if(length(breaks)==1){
			rg <- range(x, na.rm=TRUE)
			breaks <- seq(rg[1],rg[2], length = breaks)
		}
		breaks <- unique(breaks)
		if(length(breaks)>1)
	     x <- cut(x, breaks=breaks, include.lowest = TRUE, labels = FALSE)
		else 	
		 x <- as.numeric(x) 
	 }
	} else {
	  x <- as.numeric(x) 
	}
	return(list(x=x, breaks=breaks)) 
}



# Takes a dataframe and returns a vector of length nrow(data), where
# element i is strata for observation i. 
stratify <- function (dataset){
  xx <- apply(dataset, 1, function(x) paste(x, collapse = "\r"))
  tab <- table(xx)
  st <- names(tab)
  strata <- match(xx,st)
  return(strata)
}

L1 <- function(strataholder){
  L1 <- 0
  num.treated <- 0
  num.control <- 0
  for(strat in strataholder){
    num.treated <- num.treated + sum((names(strat) == 1))
    num.control <- num.control + sum((names(strat) == 0))
  }
  for(strat in strataholder){
      strat.imb <- (sum(names(strat) == 1))/num.treated - (sum(names(strat) == 0))/num.control
      L1 <- L1 + abs(strat.imb)
  }
  return(L1 * .5)
}


L1FrontierCEM <- function(treatment, dataset, drop, breaks=NULL){
  gs <- getStrata(treatment, dataset, drop, breaks=breaks)
  strata <- gs$strata
  mycut <- gs$mycut
  names(strata) <- dataset[,which(colnames(dataset) == treatment)]
  unique.strata <- unique(strata)
  strataholder <- list()
  for(i in 1:length(unique.strata)){
    strataholder[[i]] <- which(strata==unique.strata[i])
  }
  drops <- c()
  L1s <- c()
  samplesize <- c()
  wList <- list()

  ## calculate the L1 of the whole dataset
  imb <- imbalance(group=dataset[[treatment]], data=dataset, drop=drop, breaks = breaks)
  ## save the breaks that were randomly generated by the imbalance function
  if(is.null(breaks)){breaks <- imb$L1$breaks}
  L1s <- c(L1s, imb$L1$L1)
  samplesize <- c(samplesize, nrow(dataset))
  W <- rep(1,nrow(dataset))
  names(W) <- rownames(dataset)
  wList[[1]] <- W
  ## use cem to identify the singletons
  cem1 <- cem(treatment=treatment,data=dataset,cutpoints=breaks,L1.breaks=breaks,eval.imbalance=T, drop=drop)
  L1s <- c(L1s, cem1$imbalance$L1$L1)
  samplesize <- c(samplesize, sum(cem1$tab["Matched",]))
  W <- cem1$w
  names(W) <- rownames(dataset)
  wList[[2]] <- W
  
  #return(list(balance = L1s, drops = unname(drops), samplesize=samplesize, metric="L1-2", breaks=breaks))
  return(list(balance = L1s, samplesize=samplesize, metric="L1w", breaks=mycut, weights=wList))
}

L1FrontierSATT <- function(treatment, dataset, drop, breaks=NULL){
    gs <- getStrata(treatment, dataset, drop, breaks=breaks)
    strata <- gs$strata
    mycut <- gs$mycut
    names(strata) <- dataset[,which(colnames(dataset) == treatment)]
    unique.strata <- unique(strata)
    strataholder <- list()
    for(i in 1:length(unique.strata)){
        strataholder[[i]] <- which(strata==unique.strata[i])
    }

    drops <- c()
    L1s <- c(L1(strataholder))
    samplesize <- c()
    # Remove obs from imbalanced strata
    while(1){
    # get differences
        difference.vec <- c()
     
        treated.vec <- c()
        control.vec <- c()
        for(strat in strataholder){
            treated.vec <- c(treated.vec, sum((names(strat) == 1)))
            control.vec <- c(control.vec, sum((names(strat) == 0)))
        }
        difference.vec <- treated.vec/sum(treated.vec) - control.vec/sum(control.vec)

        difference.vec[difference.vec > 0] <- 0
       
        drop <- which(abs(difference.vec) == max(abs(difference.vec)))[1]
        drop.obs <- which(names(strataholder[[drop]]) == 0)[1]
        dropped <- strataholder[[drop]][drop.obs]
        drops <- c(drops, rownames(dataset)[dropped])
        samplesize <- c(samplesize, nrow(dataset)-length(drops))
        strataholder[[drop]] <- strataholder[[drop]][-drop.obs]

        L1s <- c(L1s, L1(strataholder))
        if(length(L1s) %% 1000 == 0){
          print(length(L1s))
          print(L1s[length(L1s)])
        }
        if(L1s[length(L1s)] > L1s[length(L1s) - 1]){break}
    }
    return(list(balance = L1s[1:length(L1s) - 1], drops = unname(drops)[1:length(drops) - 1],
                samplesize=samplesize[1:length(samplesize) - 1], metric="L1", breaks=mycut))
}
