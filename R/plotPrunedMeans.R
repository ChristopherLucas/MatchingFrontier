plotPrunedMeans <-
function(frontier.object,
         xlab = 'Number of Observations Pruned',
         main = 'Means Plot',
         xlim = c(1,max(frontier.object$frontier$Xs)),
         ylim = c(0, 1),
         cols = rainbow(length(frontier.object$match.on)),
         diff.in.means = FALSE,
         ...){
    
    covs.mat <- matrix(nrow = 0, ncol = length(frontier.object$match.on), byrow = FALSE)
    colnames(covs.mat) <- frontier.object$match.on

    for(col in frontier.object$match.on){
        frontier.object$dataset[,colnames(frontier.object$dataset) == col] <- range01(
                             frontier.object$dataset[,colnames(frontier.object$dataset) == col])
    }
    
    # Calculate means
    for(i in 1:length(frontier.object$frontier$Xs)){        
        this.dat.inds <- unlist(frontier.object$frontier$drop.order[1:i]) # changed for removed obs
        dataset <- frontier.object$dataset[this.dat.inds,]       
        new.row <- c()
        for(col in colnames(covs.mat)){
            if(diff.in.means){
                treated.inds <- dataset[[frontier.object$treatment]] == 1
                treated.mean <- mean(dataset[treated.inds, col])
                
                control.inds <- dataset[[frontier.object$treatment]] == 0
                control.mean <- mean(dataset[control.inds, col])
                
                new.row <- c(new.row, abs(treated.mean - control.mean))
            }else{
                new.row <- c(new.row, mean(dataset[,col]))
            }
            
        }
        covs.mat <- rbind(covs.mat, new.row)            
    }

    if(!(exists('ylab'))){
        if(diff.in.means){
            ylab <- 'Difference in scaled means'
        }else{
            ylab <- 'Scaled means'
        }
    }
    
    # make plot
    par(oma=c(0, 0, 0, 5))
    x <- frontier.object$frontier$Xs
    plot(1, type='n', main=main, xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab)
    for(i in 1:ncol(covs.mat)){
      this.y <- covs.mat[, colnames(covs.mat)[i]]
      points(x, this.y, type = 'l', col = cols[i])
    }
    
    legend(par('usr')[2], par('usr')[4], bty='n', xpd=NA, lty=c(1,1), legend=colnames(covs.mat), col = cols, title="Group")
}
