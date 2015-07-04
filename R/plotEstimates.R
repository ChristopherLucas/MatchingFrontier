plotEstimates <-
function(estimates.object,
         xlab = 'Number of Observations Pruned',
         ylab = 'Estimate',
         main = 'Effects Plot',
         xlim = NULL,
         ylim = NULL,
         mod.dependence.col = rgb(255,0,0,127, maxColorValue=255),
         mod.dependence.border.col = rgb(255,0,0,200, maxColorValue=255),
         line.col = rgb(102,0,0,255, maxColorValue=255),
         ...){

    mod.dependence.mins <- lapply(estimates.object$mod.dependence, min)
    mod.dependence.maxs <- lapply(estimates.object$mod.dependence, max)
    
    if(is.null(xlim)){
        xlim <- c(0, max(estimates.object$Xs, na.rm = TRUE))
    }
    if(is.null(ylim)){
        ylim <- c(min(mod.dependence.mins), max(mod.dependence.maxs))
    }
    print(ylim)
    plot(1, type="n", main, xlab=xlab, ylab=ylab,
         xlim = xlim,
         ylim = ylim,
         ...)

    x0 <- rev(estimates.object$Xs)
    x1 <- estimates.object$Xs
    y0 <- rev(mod.dependence.mins)
    y1 <- mod.dependence.maxs

    remove <- (is.na(x0) | is.na(x1) | is.na(y0) | is.na(y1))
    polygon(c(x0, x1),
            c(y0, y1),
            col = mod.dependence.col,
            border = mod.dependence.border.col)
    lines(estimates.object$Xs, estimates.object$coefs, type = 'l', col = line.col)
}
