estimateEffects <-
function(frontier.object, formula, prop.estimated = 1, mod.dependence.formula, seed = 1){

    set.seed(seed)
    
    # These are the points that we'll estimate
    point.inds <- sort(sample(1:length(frontier.object$frontier$Xs),
                              round(length(frontier.object$frontier$Xs) * prop.estimated)))

    coefs <- vector(mode="list", length = length(point.inds))
    CIs <- vector(mode="list", length= length(point.inds))
    mod.dependence <- vector(mode="list", length= length(point.inds))
    
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
        tryCatch(this.sig.hat <- modelDependence(dataset, treatment, mod.dependence.formula, verbose = FALSE)$sigma.hat.theta,
                 error = function(e) this.sig.hat <- NA)

        coefs[i] <- coef(results)[frontier.object$treatment]
        CIs[[i]] <- confint(results)[frontier.object$treatment,]       
        mod.dependence[i] <- this.sig.hat
        
        setTxtProgressBar(pb, i)
    }
    close(pb)
    
    return(list(Xs = frontier.object$frontier$Xs[point.inds], coefs = coefs, CIs = CIs, mod.dependence = mod.dependence))
}

