parallelPlot <-
function(frontier.object, number.to.prune, variables, treated.col = 'grey', control.col = 'black'){
    X <- nrow(frontier.object$dataset) - frontier.object$frontier$Xs
    ind <- which(abs(X - number.to.prune) == min(abs(X - number.to.prune)))
    this.dat.inds <- unlist(frontier.object$frontier$drop.order[ind:length(frontier.object$frontier$drop.order)])

    dataset <- frontier.object$dataset[this.dat.inds,]
    
    col <- rep(NA, nrow(dataset))
    col[dataset[[frontier.object$treatment]] == 1] <- treated.col
    col[dataset[[frontier.object$treatment]] == 0] <- control.col

    dataset <- dataset[,variables]
    parcoord(dataset, col = col)
}
