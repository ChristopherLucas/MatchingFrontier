plotTreatedProp <-
function(frontier.object,
         main = 'Proportion of Treated Units',
         xlim = c(1,max(frontier.object$frontier$Xs)),
         ylim = c(0, 1),
         xlab = 'Number of Observations Pruned',
         ylab = 'Treated Proportion',
         col = 'red',
         ...){
  x <- frontier.object$frontier$Xs
  N <- sum(lalonde.frontier$dataset[, lalonde.frontier$treatment])
  pruned.treat <- c()
  for(i in 1: length(x)){
    pruned.treat <- c(pruned.treat, sum(frontier.object$frontier$drop.order[[i]] <= N))
  }
  pruned.treat <- cumsum(pruned.treat)
  
  # make the plot
  plot(1, type='n', main = main, xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab)
  this.y <- c()
  for(i in 1:length(x)){
       this.y[i] <- (N - pruned.treat[i])/ (nrow(frontier.object$dataset) - x[i])
  }
  points(x, this.y, type = 'l', col = col)
}



