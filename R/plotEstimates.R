plotEstimates <-
function(estimates.object,
         CI == TRUE,
         mod.dependence == TRUE,
         xlab = 'Number of Observations Pruned',
         ylab = 'Estimate',
         main = 'Effects Plot',
         xlim = NULL,
         ylim = NULL,
         CI.col = rgb(102,178,255,255, maxColorValue=255),
         CI.border.col = rgb(0,102,204,255, maxColorValue=255),     
         mod.dependence.col = rgb(255,0,0,127, maxColorValue=255),
         mod.dependence.border.col = rgb(255,0,0,200, maxColorValue=255),
         line.col = rgb(102,0,0,255, maxColorValue=255),
         ...){
    
    method <- estimates.object$method
    mod.dependence.mins <- unlist(lapply(estimates.object$mod.dependence, function(x) x[1]))
    mod.dependence.maxs <- unlist(lapply(estimates.object$mod.dependence, function(x) x[2]))
    CI.mins <- unlist(lapply(estimates.object$CIs, function(x) x[1]))
    CI.maxs <- unlist(lapply(estimates.object$CIs, function(x) x[2]))
    
    if(method == "simulated AME"){
        min.effect <- min(estimates.object$AMEs, na.rm = TRUE)
        max.effect <- max(estimates.object$AMEs, na.rm = TRUE)}else{
            min.effect <- min(estimates.object$coefs, na.rm = TRUE)
            max.effect <- max(estimates.object$coefs, na.rm = TRUE)
        }
    
    
    if(is.null(xlim)){
        xlim <- c(0, max(estimates.object$Xs, na.rm = TRUE))
    }
    if(is.null(ylim)){
        min <- min(min(mod.dependence.mins, na.rm = TRUE), min(CI.mins, na.rm = TRUE), min.effect)
        max <- max(max(mod.dependence.maxs, na.rm = TRUE), max(CI.mins, na.rm = TRUE), max.effect)
        ylim <- c(min, max)
    }
    print(ylim)
    plot(1, type="n", main=main, xlab=xlab, ylab=ylab,
         xlim = xlim,
         ylim = ylim,
         ...)
    
    if(mod.dependence == T){
        mod.x0 <- rev(estimates.object$Xs)
        mod.x1 <- estimates.object$Xs
        mod.y0 <- rev(mod.dependence.mins)
        mod.y1 <- mod.dependence.maxs
        remove <- (is.na(mod.x0) | is.na(mod.x1) | is.na(mod.y0) | is.na(mod.y1))
        polygon(c(mod.x0, mod.x1),
                c(mod.y0, mod.y1),
                col = mod.dependence.col,
                border = mod.dependence.border.col)
    }
    
    if(CI == T){
        CI.x0 <- rev(estimates.object$Xs)
        CI.x1 <- estimates.object$Xs
        CI.y0 <- rev(CI.mins)
        CI.y1 <- CI.maxs
        remove <- (is.na(CI.x0) | is.na(CI.x1) | is.na(CI.y0) | is.na(CI.y1))
        polygon(c(CI.x0, CI.x1),
                c(CI.y0, CI.y1),
                col = CI.col,
                border = CI.border.col)
    }
   
    if(method == "simulated AME"){
    lines(estimates.object$Xs, estimates.object$AMEs, type = 'l', col = line.col)
            }else{
    lines(estimates.object$Xs, estimates.object$coefs, type = 'l', col = line.col)
            }
}
