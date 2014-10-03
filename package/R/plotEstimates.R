plotEstimates <-
function(estimates.object,
         xlab = 'Number of Observations Pruned',
         ylab = 'Estimate',
         main = 'Effects Plot',
         ...){

    # Plot estimates
    X <- estimates.object$Xs
    plot(X, estimates.object$coefs, xlab = xlab, ylab = ylab, main = main, ...)
    segments(X, unlist(lapply(estimates.object$CIs, function(x) x[1])),
           X, unlist(lapply(estimates.object$CIs, function(x) x[2])),
           col = 'gray75')
    points(X, estimates.object$coefs, ...)

    # Plot model dependence
    for(i in 1:length(estimates.object$mod.dependence)){
        x.base <- as.integer(names(estimates.object$mod.dependence[i]))
        print(x.base)
#        return(list(x = x.base + density(estimates.object$mod.dependence[[i]])$y * 100000, y = density(mahal.estimates$mod.dependence[[1]])$x))

        Xs <- x.base + density(estimates.object$mod.dependence[[i]])$y * 100000
        Ys <- density(estimates.object$mod.dependence[[i]])$x

        print(range(Xs))
        print(range(Ys))

        points(Xs, Ys, type = 'l', lwd = 2)
    }
}
