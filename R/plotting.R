
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

