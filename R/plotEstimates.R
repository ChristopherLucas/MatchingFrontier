plotEstimates <-
function(estimates.object,
         xlab = 'Number of Observations Pruned',
         ylab = 'Estimate',
         main = 'Effects Plot',
         ...){

    plot(estimates.object$Xs, estimates.object$coefs, xlab = xlab, ylab = ylab, main = main, ...)
    points(estimates.object$Xs, unlist(lapply(estimates.object$CIs, function(x) x[1])), type = 'l')
    points(estimates.object$Xs, unlist(lapply(estimates.object$CIs, function(x) x[2])), type = 'l')    
    
    # Plot model dependence
    par(new = TRUE)
    plot(estimates.object$Xs, estimates.object$modelDependence, type = "l", axes = FALSE, bty = "n", xlab = "", ylab = "")
    axis(side=4, at = pretty(range(estimates.object$modelDependence)))
    mtext("Model Dependence (expression(hat(sigma)[theta]))", side=4, line=3)
}
