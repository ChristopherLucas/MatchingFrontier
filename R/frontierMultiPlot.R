frontierMultiPlot <- function(frontierObject, dataset, frontierEstObject=NULL, zoom = NULL, drop=NULL){
  p1 <- plotFrontier(frontierObject, dataset, frontierEstObject, zoom, drop)
  p2 <- plotEffects(frontierObject, dataset, frontierEstObject, zoom, drop)
  p3 <- plotMeans(frontierObject, dataset, frontierEstObject, zoom, drop)
  p.final <- multiPlot(p1, p2, p3, cols=1)
  return(p.final)
}
