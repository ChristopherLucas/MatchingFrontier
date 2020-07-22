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

    mod.dependence.mins <- unlist(lapply(estimates.object$mod.dependence, function(x) x[1]))
    mod.dependence.maxs <- unlist(lapply(estimates.object$mod.dependence, function(x) x[2]))
    method <- estimates.object$method
    
    if(is.null(xlim)){
        xlim <- c(0, max(estimates.object$Xs, na.rm = TRUE))
    }
    if(is.null(ylim)){
        ylim <- c(min(mod.dependence.mins, na.rm = TRUE), max(mod.dependence.maxs, na.rm = TRUE))
    }
    print(ylim)
    plot(1, type="n", main=main, xlab=xlab, ylab=ylab,
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
            if(method == "simulated AME"){
    lines(estimates.object$Xs, estimates.object$AMEs, type = 'l', col = line.col)
            }else{
    lines(estimates.object$Xs, estimates.object$coefs, type = 'l', col = line.col)
            }
}
