library(ggplot2)

# These are the functions that users are supposed to call. 

# This gets the frontier
finalFrontier <- function(treatment, dataset, drop, metric, mdist = NULL){

  # get frontier
  if(metric == 'L1'){
    frontier <- L1Frontier(treatment, dataset, drop)
  }

    if(metric == 'Mahal'){
    frontier <- MahalFrontier(treatment, dataset, drop, mdist)
  }

  return(frontier)
}

plotFrontier <- function(finalFrontierObject, dataset, zoom = NULL){
  p1 <- ggplot(finalFrontierObject$balance, aes(x=Time, y=weight, colour=Diet, group=Chick)) +
    geom_line() +
    ggtitle("Growth curve for individual chicks")
}

generateDataset <- function(finalFrontierObject, dataset, number.dropped){
  keep <- !(seq(nrow(dataset)) %in% finalFrontierObject$drops[1:number.dropped])
  return(dataset[keep,])
}
