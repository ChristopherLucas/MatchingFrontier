plotEstimates <-
function(estimates.object,
         xlab = 'Number of Observations Pruned',
         ylab = 'Estimate',
         main = 'Effects Plot',
         ...){

    plot(estimates.object$Xs, estimates.object$coefs, xlab = xlab, ylab = ylab, main = main, ...)
    
    # Plot model dependence
    Xs <- c()
    Ys <- list()

    
    for(i in 1:length(estimates.object$mod.dependence)){
        x.base <- as.integer(names(estimates.object$mod.dependence[i]))
        Xs <- c(Xs, x.base)
        Ys[[length(Ys) + 1]] <- quantile(estimates.object$mod.dependence[[i]], probs = c(0, 1))
    }
    
    Xs <- c(Xs, rev(Xs))
    Ys <- c(unlist(lapply(Ys, function(x) x[1])),
              rev(unlist(lapply(Ys, function(x) x[2]))))
    
    polygon(Xs,
            Ys,
            col = rgb(1, 0, 0, 0.4), lty = 2, lwd = 1, border = "red")

    
    # Plot estimates
    points(estimates.object$Xs, estimates.object$coefs, type = 'l')

    
    points(estimates.object$Xs, unlist(lapply(estimates.object$CIs, function(x) x[1])), type = 'l')
    points(estimates.object$Xs, unlist(lapply(estimates.object$CIs, function(x) x[2])), type = 'l')    
    
    ## polygon(c(estimates.object$Xs, rev(estimates.object$Xs)),
    ##         c(unlist(lapply(estimates.object$CIs, function(x) x[1])),
    ##           rev(unlist(lapply(estimates.object$CIs, function(x) x[2])))),
    ##         col = rgb(0, 0, 1, .5),
    ##         lty = 2, lwd = 1, border = "blue")
            
    ## segments(estimates.object$Xs, unlist(lapply(estimates.object$CIs, function(x) x[1])),
    ##          estimates.object$Xs, unlist(lapply(estimates.object$CIs, function(x) x[2])),
    ##          col = 'black')
    points(estimates.object$Xs, estimates.object$coefs, ...)
}
