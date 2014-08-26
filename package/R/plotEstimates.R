plotEstimates <-
function(frontier.object,
                          estimates.object,
                          xlab = 'Number of Observations Pruned',
                          ylab = 'Estimate',
                          main = 'Effects Plot',
                          ...){
    X <- frontier.object$frontier$Xs
    plot(X, estimates.object$coefs, xlab = xlab, ylab = ylab, main = main, ...)
    segments(X, unlist(lapply(estimates.object$CIs, function(x) x[1])),
           X, unlist(lapply(estimates.object$CIs, function(x) x[2])),
           col = 'gray75')
    points(X, estimates.object$coefs, ...)
}
