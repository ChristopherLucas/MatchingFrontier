plotFrontier <-
function(frontier.object,
                         xlab = 'Number of Observations Pruned',
                         ylab = 'AMD',
                         main = 'Frontier Plot',
                         ...){

    plot(nrow(frontier.object$dataset) - frontier.object$frontier$Xs, frontier.object$frontier$Ys,
         xlab = xlab,
         ylab = ylab,
         ...)
}
