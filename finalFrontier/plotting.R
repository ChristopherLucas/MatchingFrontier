# Make some pretty pictures

library(reshape) # To convert dataframe to long format
library(MASS) # For the moment, stealing their parallel plot

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


frontierPlotL1 <- function(frontierObject, dataset, frontierEstObject=NULL, zoom = NULL, drop=NULL){

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

  # Causal Effects
  if(!is.null(frontierEstObject)){
    q <- frontierEstObject[starting.index:ending.index,]
    eb <- aes(ymax = mean + sd, ymin = mean - sd)

    p2 <- ggplot(data = q, aes(x = x, y = mean)) + 
      geom_line(size = 1) + geom_point() +
      geom_ribbon(eb, alpha = 0.5) +
      xlab("Number of Observations Dropped") +
      ylab("Effect Size and SEs")
  } else {
    p2 <- NULL
  }

  # Overall Means
  drop <- c('treated', 're78')
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
               aes(x=X1, y=value, colour=X2)) +
    geom_line() +
    xlab("Number of Observations Dropped") +
    ylab("Standardized Mean Value") +
    opts(legend.position="bottom") +
    theme(legend.title=element_blank())

  # Join
  if(!is.null(p2)){
    p.final <- multiplot(p1, p2, p3, cols=1)
  } else {
    p.final <- multiplot(p1, p3, cols=1)
  }

  return(list(p.final,covs.mat))
}




frontierPlotMahal <- function(frontierObject, dataset, frontierEstObject=NULL, zoom = NULL, drop=NULL){

  starting.index <- 1
  #ending.index <- length(frontierObject$drops)
  ending.index <- length(frontierObject$balance)
  if(!is.null(zoom)){
    ## I made a warning for zoom
    if(zoom[1]<0 | tail(zoom, 1) > length(frontierObject$drops)){stop(paste("zoom must be between 0 and ",length(frontierObject$drops)," for this data set.",sep=""))}
    ## get the index corresponding with the requested number of obs to remove for the start of the zoom
    starting.index <- which(abs((frontierObject$samplesize[1] - frontierObject$samplesize)-zoom[1]) == min(abs((frontierObject$samplesize[1] - frontierObject$samplesize)-zoom[1])))[1]
    ## get the index corresponding with the requested number of obs to remove for the end of the zoom
    ending.index <- tail( which(abs((frontierObject$samplesize[1] - frontierObject$samplesize)-tail(zoom, 1)) == min(abs((frontierObject$samplesize[1] - frontierObject$samplesize)-tail(zoom, 1)))), 1)
  }
  
  # Frontier
  #df <- data.frame(x = seq(starting.index, ending.index), y = frontierObject$balance[starting.index:ending.index])
  df <- data.frame(x = frontierObject$samplesize[starting.index]-frontierObject$samplesize[starting.index:ending.index], 
                   y = frontierObject$balance[starting.index:ending.index])
  
  p1 <- ggplot(df, aes(x=x, y=y)) +
      geom_line() + geom_point() +
      xlab("Number of Observations Dropped") +
      ylab("Imbalance") +
      ggtitle("Imbalance Frontier")


  # Causal Effects
  if(!is.null(frontierEstObject)){
    q <- frontierEstObject[starting.index:ending.index,]
    eb <- aes(ymax = mean + sd, ymin = mean - sd)

    p2 <- ggplot(data = q, aes(x = x, y = mean)) + 
      geom_line(size = 1) + geom_point() +
      geom_ribbon(eb, alpha = 0.5) +
      xlab("Number of Observations Dropped") +
      ylab("Effect Size and SEs")
  } else {
    p2 <- NULL
  }


  # Overall Means
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
               aes(x=X1, y=value, colour=X2)) +
    geom_line() + 
    xlab("Number of Observations Dropped") +
    ylab("Standardized Mean Value") +
    opts(legend.position="bottom") +
    theme(legend.title=element_blank())

  # Join
  if(!is.null(p2)){
    p.final <- multiplot(p1, p2, p3, cols=1)
  } else {
    p.final <- multiplot(p1, p3, cols=1)
  }

  p.final
  #return(list(p.final,covs.mat))
  return(invisible(list(covs.mat=covs.mat)))
}




# Function for user
frontierPlot <- function(frontierObject, dataset, frontierEstObject=NULL, zoom = NULL, drop=NULL){
  ## Plot L1 frontier
  if(frontierObject$metric=="L1"){
    frontierPlotL1(frontierObject=frontierObject, dataset=dataset, frontierEstObject=frontierEstObject, zoom=zoom, drop=drop)
  }
  ## Plot Mahal frontier
  if(frontierObject$metric=="Mahal"){
    frontierPlotMahal(frontierObject=frontierObject, dataset=dataset, frontierEstObject=frontierEstObject, zoom=zoom, drop=drop)
  }
}
  




