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
                 ylab("Rescaled Mean Value") +
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
