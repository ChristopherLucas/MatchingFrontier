# This creates the L1 frontier

get.strata <- function(treatment, dataset, drop){

  # Remove dropped observations
  dropped <- match(drop, colnames(dataset))
  if(length(dropped) > 0){
    dataset <- dataset[-dropped]
  }

  # Bin data
  for (i in 1:dim(dataset)[2]) { # For each column
    dataset[[i]] <- reduce.var(dataset[[i]]) # Collapse with Sturges'
  }

  # Calculate strata
  strata <- stratify(dataset)
  return(strata)
}


# Takes a column of data and uses Sturges' formula to bin the values. 
reduce.var <- function(x){

  # Calculate breaks
  breaks <- nclass.Sturges(x) 
  if(length(breaks) > 0){

    # If there is only one break, instead
    # set the breaks at the min and max values
    if(length(breaks)==1){ 
      rg <- range(x, na.rm=TRUE)
      breaks <- seq(rg[1],rg[2], length = breaks)
    }

    # If there are more than one break,
    # cut x according to the breaks
    if(length(breaks)>1)
      x <- cut(x, breaks=breaks, include.lowest = TRUE, labels = FALSE)
  }

  return(x)
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

L1Frontier <- function(treatment, dataset, drop){
  strata <- get.strata(treatment, dataset, drop)
  names(strata) <- dataset[,which(colnames(dataset) == treatment)]
  unique.strata <- unique(strata)
  strataholder <- list()
  for(i in 1:length(unique.strata)){
    strataholder[[i]] <- which(strata==unique.strata[i])
  }
  drops <- c()
  L1s <- c()

# Remove strata w/ one obs
  while(sum(lapply(strataholder, length) == 1) > 0){
    dropped.strata <- which(lapply(strataholder, length) == 1)[1]
    drop <- strataholder[[dropped.strata]]
    drops <- c(drops, drop)
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
      drops <- c(drops, dropped)
      strataholder[[drop]] <- strataholder[[drop]][-drop.obs]
    }

    if(sign > 0){
      drop.obs <- which(names(strataholder[[drop]]) == 1)[1]
      dropped <- strataholder[[drop]][drop.obs]
      drops <- c(drops, dropped)
      strataholder[[drop]] <- strataholder[[drop]][-drop.obs]
    }
  
    L1s <- c(L1s, L1(strataholder))
  }
  return(list(balance = L1s, drops = unname(drops)))
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

