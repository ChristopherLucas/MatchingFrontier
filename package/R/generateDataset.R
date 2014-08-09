generateDataset <-
function(frontier.object, number.to.prune){
    X <- nrow(frontier.object$dataset) - frontier.object$frontier$Xs
    ind <- which(abs(X - number.to.prune) == min(abs(X - number.to.prune)))
    this.dat.inds <- unlist(frontier.object$frontier$drop.order[ind:length(frontier.object$frontier$drop.order)])
    dataset <- frontier.object$dataset[this.dat.inds,]

    # Add weights
    w <- makeWeights(dataset, frontier.object$treatment)
    dataset$weights <- w
    return(dataset)
}
