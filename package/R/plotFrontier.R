plotFrontier <-
function(frontier.object,
                         xlab = 'Number of Observations Pruned',
                         ylab = 'AMI',
                         main = 'Frontier Plot',
                         ...){

    plot(nrow(frontier.object$dataset) - frontier.object$frontier$Xs, frontier.object$frontier$Ys,
         xlab = xlab,
         ylab = ylab,
         ...)
}
