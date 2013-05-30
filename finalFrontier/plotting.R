# Make some pretty pictures

library(reshape) # To convert dataframe to long format
library(MASS) # For the moment, stealing their parallel plot

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

# Function for user
frontierPlot <- function(frontierObject, dataset, myform, zoom = NULL){

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
  effectholder <- c()
  seholder <- c() 

  for(i in 1:length(frontierObject$drops)){
    m1 <- lm(myform,
           data=dataset[!(seq(nrow(dataset)) %in% frontierObject$drops[1:i]),])
    effectholder <- c(effectholder, summary(m1)$coeff["treated",1])
    seholder <- c(seholder, summary(m1)$coeff["treated",2])
  }

  q <- data.frame(x = seq(starting.index, ending.index))

  q$mean <- effectholder[starting.index:ending.index]
  q$sd <- seholder[starting.index:ending.index]

  eb <- aes(ymax = mean + sd, ymin = mean - sd)

  p2 <- ggplot(data = q, aes(x = x, y = mean)) + 
    geom_line(size = 2) + 
    geom_ribbon(eb, alpha = 0.5) +
    xlab("Number of Observations Dropped") +
    ylab("Effect Size and SEs")

  # Overall Means
  drop <- c('treated', 're78')
  covs <- colnames(dataset)[!(colnames(dataset) %in% drop)]

  for(col in covs){
      dataset[,colnames(dataset) == col] <- range01(dataset[,colnames(dataset) == col])
    }

  covs.mat <- matrix(nrow = 0, ncol = length(covs), byrow = FALSE)
  colnames(covs.mat) <- covs

  for(i in 1:length(frontierObject$drops)){
    iter.dat <- dataset[!(seq(nrow(dataset)) %in% frontierObject$drops[1:i]),]
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
  p.final <- multiplot(p1, p2, p3, cols=1)

  return(p.final)
}
