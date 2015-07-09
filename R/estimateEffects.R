estimateEffects <-
function(frontier.object,
         formula,
         prop.estimated = 1,
         seed = 1,
         model.dependence.ests = 100){
    
    set.seed(seed)
    
    # These are the points that we'll estimate
    point.inds <- sort(sample(1:length(frontier.object$frontier$Xs),
                              round(length(frontier.object$frontier$Xs) * prop.estimated)))
    coefs <- vector(mode="list", length = length(point.inds))
    CIs <- vector(mode="list", length= length(point.inds))
    mod.dependence <- vector(mode="list", length= length(point.inds))

    treatment <- frontier.object$treatment
    outcome <- frontier.object$outcome
    covs <- frontier.object$match.on

    specifications <- getSpecifications(covs, treatment, outcome, dataset, model.dependence.ests)

    pb <- txtProgressBar(min = 1, max = length(point.inds), style = 3)
    for(i in 1:length(point.inds)){
        this.dat.inds <- unlist(frontier.object$frontier$drop.order[point.inds[i]:length(frontier.object$frontier$drop.order)])
        dataset <- frontier.object$dataset[this.dat.inds,]

        this.mod.dependence <- modelDependence(dataset,
                                               treatment,
                                               outcome,
                                               covs,
                                               model.dependence.ests,
                                               verbose = FALSE,
                                               frontier.object$ratio,
                                               specifications = specifications)

        if(frontier.object$ratio == 'variable'){
            w <- makeWeights(dataset, treatment)
            dataset$w <- w            
            results <- lm(formula, dataset, weights = w)
        } else {
            results <- lm(formula, dataset)
        }
                
        coefs[i] <- coef(results)[frontier.object$treatment]
        CIs[[i]] <- confint(results)[frontier.object$treatment,]       
        mod.dependence[[i]] <- this.mod.dependence
        
        setTxtProgressBar(pb, i)
    }
    close(pb)
    
    return(list(Xs = frontier.object$frontier$Xs[point.inds], coefs = unlist(coefs), CIs = CIs, mod.dependence = mod.dependence))
}

