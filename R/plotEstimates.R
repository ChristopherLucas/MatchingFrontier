plotEstimates <-
function(estimates.object,
         xlab = 'Number of Observations Pruned',
         ylab = 'Estimate',
         main = 'Effects Plot',
         ...){

    par(mar=c(5,4,4,5)+.1)
    plot(estimates.object$Xs, estimates.object$coefs, xlab = xlab, ylab = ylab, main = main, ...)
    points(estimates.object$Xs, unlist(lapply(estimates.object$CIs, function(x) x[1])), type = 'l')
    points(estimates.object$Xs, unlist(lapply(estimates.object$CIs, function(x) x[2])), type = 'l')    
    
    # Plot model dependence
    par(new = TRUE)
    plot(estimates.object$Xs,
         estimates.object$mod.dependence,
         type = "l",
         axes = FALSE,
         bty = "n",
         xlab = "",
         ylab = "",
         col = 'red')
    axis(side=4, at = pretty(range(estimates.object$mod.dependence, na.rm = TRUE)),
         col = 'red',
         col.ticks = 'red',
         col.lab = "red",
         col.axis = "red")
    mtext(expression(paste("Model Dependence ", hat(sigma)[theta])), side=4, line = 3, col="red")
}
