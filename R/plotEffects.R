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
