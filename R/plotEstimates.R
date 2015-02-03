plotEstimates <-
function(estimates.object,
         xlab = 'Number of Observations Pruned',
         ylab = 'Estimate',
         main = 'Effects Plot',
         mod.dependence.col = rgb(255,0,0,127, maxColorValue=255),
         mod.dependence.border.col = rgb(255,0,0,200, maxColorValue=255),
         line.col = rgb(255,0,0,255, maxColorValue=255),
         ...){

    plot(1, type="n", main, xlab=xlab, ylab=ylab,
         xlim = c(0, max(estimates.object$Xs)),
         ylim = c(min(estimates.object$coefs - estimates.object$mod.dependence),
             max(estimates.object$coefs + estimates.object$mod.dependence)))
    polygon(c(estimates.object$Xs, rev(estimates.object$Xs)),
            c(estimates.object$coefs - estimates.object$mod.dependence,
              rev(estimates.object$coefs + estimates.object$mod.dependence)),
            col = mod.dependence.col,
            border = mod.dependence.border.col)
    lines(estimates.object$Xs, estimates.object$coefs, type = 'l', col = line.col)
}
