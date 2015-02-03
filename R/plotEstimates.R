plotEstimates <-
function(estimates.object,
         xlab = 'Number of Observations Pruned',
         ylab = 'Estimate',
         main = 'Effects Plot',
         ...){

    par(mar=c(5,4,4,5)+.1)
    plot(estimates.object$Xs, estimates.object$coefs, xlab = xlab, ylab = ylab, main = main, ...)
    polygon(c(estimates.object$Xs, rev(estimates.object$Xs)),
            c(estimates.object$coefs - estimates.object$mod.dependence, rev(estimates.object$coefs + estimates.object$mod.dependence)), col="thistle",border=NA)
        
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
    mtext(expression(paste("Model Dependence (", hat(sigma)[theta], ")")), side=4, line = 3, col="red")
}
