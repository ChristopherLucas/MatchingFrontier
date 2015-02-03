plotEstimates <-
function(estimates.object,
         xlab = 'Number of Observations Pruned',
         ylab = 'Estimate',
         main = 'Effects Plot',
         mod.dependence.col = 'cornflowerblue',
         ...){

    plot(1, type="n", main, xlab=xlab, ylab=ylab,
         xlim = c(0, max(estimates.object$Xs)),
         ylim = c(0, max(estimates.object$coefs + estimates.object$mod.dependence)))
    polygon(c(estimates.object$Xs, rev(estimates.object$Xs)),
            c(estimates.object$coefs - estimates.object$mod.dependence,
              rev(estimates.object$coefs + estimates.object$mod.dependence)),
            col=mod.dependence.col,border=NA)
    lines(estimates.object$Xs, estimates.object$coefs, type = 'l')

        
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
