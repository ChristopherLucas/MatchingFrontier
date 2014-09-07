estimateEffects <-
function(frontier.object, formula, prop.estimated = 1, PoSI = TRUE){

    point.inds <- sort(sample(1:length(frontier.object$frontier$Xs),
                              round(length(frontier.object$frontier$Xs) * prop.estimated)))
    
    coefs <- vector(mode="list", length = length(point.inds))
    CIs <- vector(mode="list", length= length(point.inds))

    treatment <- frontier.object$treatment

    pb <- txtProgressBar(min = 1, max = length(point.inds), style = 3)
    for(i in 1:length(point.inds)){        
        this.dat.inds <- unlist(frontier.object$frontier$drop.order[point.inds[i]:length(frontier.object$frontier$drop.order)])
        dataset <- frontier.object$dataset[this.dat.inds,]

        if(frontier.object$ratio == 'variable'){
            w <- makeWeights(dataset, treatment)
            dataset$w <- w            
            results <- lm(formula, dataset, weights = w)
        } else {
            results <- lm(formula, dataset)
        }

        coefs[i] <- coef(results)[frontier.object$treatment]
        CIs[[i]] <- confint(results)[frontier.object$treatment,]
        if(PoSI == TRUE){
            multiplier <- PoSIMultiplier(frontier.object$dataset[, frontier.object$match.on])
            print(coefs[i])
            print(CIs[[i]])
            new.error <- (abs(coefs[[i]] - CIs[[i]][1])) * multiplier
            print(new.error)
            CIs[[i]] <- c(coefs[[i]] - new.error, coefs[i] + new.error)
            print(CIs[[i]])
        }
        setTxtProgressBar(pb, i)
    }
    close(pb)
    return(list(Xs = frontier.object$frontier$Xs[point.inds], coefs = coefs, CIs = CIs))
}

