estimateEffects <-
function(frontier.object, formula){
    coefs <- vector(mode="list", length=length(frontier.object$frontier$drop.order))
    CIs <- vector(mode="list", length=length(frontier.object$frontier$drop.order))

    treatment <- frontier.object$treatment

    pb <- txtProgressBar(min = 0, max = length(frontier.object$frontier$drop.order), style = 3)
    for(i in 1:length(frontier.object$frontier$drop.order)){        
        this.dat.inds <- unlist(frontier.object$frontier$drop.order[i:length(frontier.object$frontier$drop.order)])
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
        setTxtProgressBar(pb, i)
    }
    close(pb)
    return(list(coefs = coefs, CIs = CIs))
}
