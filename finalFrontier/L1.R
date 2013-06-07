# This creates the L1 frontier

get.strata <- function(treatment, dataset, drop, breaks=NULL){

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
    tmp <- reduce.var(dataset[[i]], breaks[[vnames[i]]])
    dataset[[i]] <- tmp$x
    mycut[[vnames[i]]] <- tmp$breaks
  }

  # Calculate strata
  strata <- stratify(dataset)
  return(list(strata=strata, mycut=mycut))
}


# Takes a column of data and uses Sturges' formula to bin the values. 
#reduce.var <- function(x){
#
#  # Calculate breaks
#  breaks <- nclass.Sturges(x) 
#  if(length(breaks) > 0){
#
#    # If there is only one break, instead
#    # set the breaks at the min and max values
#    if(length(breaks)==1){ 
#      rg <- range(x, na.rm=TRUE)
#      breaks <- seq(rg[1],rg[2], length = breaks)
#    }
#
#    # If there are more than one break,
#    # cut x according to the breaks
#    if(length(breaks)>1)
#      x <- cut(x, breaks=breaks, include.lowest = TRUE, labels = FALSE)
#  }
#
#  return(x)
#}

## the original reduce.var from cem
reduce.var <- function(x, breaks=NULL){
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

L1Frontier <- function(treatment, dataset, drop, breaks=NULL){
  gs <- get.strata(treatment, dataset, drop, breaks=breaks)
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

# Remove strata w/ one obs
  while(sum(lapply(strataholder, length) == 1) > 0){
    dropped.strata <- which(lapply(strataholder, length) == 1)[1]
    drop <- strataholder[[dropped.strata]]
#    drops <- c(drops, drop)
    drops <- c(drops, rownames(dataset)[drop])
    samplesize <- c(samplesize, nrow(dataset)-length(drops))
    strataholder[dropped.strata] <- NULL
    L1s <- c(L1s, L1(strataholder))
  }  

# Remove obs from imbalanced strata
  while(1){
    #get differences
    difference.vec <- c()

    for(s in 1:length(strataholder)){
      d <- sum(names(strataholder[[s]]) == '1') - sum(names(strataholder[[s]]) == '0')
      difference.vec <- c(difference.vec, d)
    }
    
    if(max(abs(difference.vec)) == 0){break}
    
    drop <- which(abs(difference.vec) == max(abs(difference.vec)))[1]
    sign <- sum(names(strataholder[[drop]]) == '1') - sum(names(strataholder[[drop]]) == '0')
    
    if(sign < 0){
      drop.obs <- which(names(strataholder[[drop]]) == 0)[1]
      dropped <- strataholder[[drop]][drop.obs]
#      drops <- c(drops, dropped)
      drops <- c(drops, rownames(dataset)[dropped])
      samplesize <- c(samplesize, nrow(dataset)-length(drops))
      strataholder[[drop]] <- strataholder[[drop]][-drop.obs]
    }

    if(sign > 0){
      drop.obs <- which(names(strataholder[[drop]]) == 1)[1]
      dropped <- strataholder[[drop]][drop.obs]
#      drops <- c(drops, dropped)
      drops <- c(drops, rownames(dataset)[dropped])
      samplesize <- c(samplesize, nrow(dataset)-length(drops))
      strataholder[[drop]] <- strataholder[[drop]][-drop.obs]
    }
  
    L1s <- c(L1s, L1(strataholder))
  }
  return(list(balance = L1s, drops = unname(drops), samplesize=samplesize, metric="L1", breaks=mycut))
}


L1 <- function(strataholder){
  L1 <- 0
  for(strat in strataholder){
    strat.imb <- abs(sum(names(strat) == '1') - sum(names(strat) == '0')) /
      Reduce('+', lapply(strataholder, length))
    L1 <- L1 + strat.imb
  }
  return(L1)
}

