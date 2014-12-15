estimateEffects <-
function(frontier.object, formula, prop.estimated = 1, model.dependence.points = 4, model.dependence.ests = 100){

    point.inds <- sort(sample(1:length(frontier.object$frontier$Xs),
                              round(length(frontier.object$frontier$Xs) * prop.estimated)))
    
    coefs <- vector(mode="list", length = length(point.inds))
    CIs <- vector(mode="list", length= length(point.inds))

    treatment <- frontier.object$treatment

    ################################
    cat('Estimating effects...\n') #
    ################################
    
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
        setTxtProgressBar(pb, i)
    }
    close(pb)

    #########################################
    cat('Estimating model dependence...\n') #
    #########################################
    
    # Get points to estimate
    depend.point.inds <- c(point.inds[1], point.inds[round((2:(model.dependence.points - 1) * length(point.inds) * (1 / model.dependence.points)))], point.inds[length(point.inds)])

    mod.dependence <- list()
    for(i in 1:length(depend.point.inds)){
        this.dat.inds <- unlist(frontier.object$frontier$drop.order[depend.point.inds[i]:length(frontier.object$frontier$drop.order)])
        dataset <- frontier.object$dataset[this.dat.inds,]

        coef.dist <- c()
        for(k in 1:model.dependence.ests){
            # Select model
            covs <- sample(frontier.object$match.on, sample(1:length(frontier.object$match.on), 1))
            
            # Make mod formula
            cov.polys <- c()
            for(cov in covs){
                if(length(unique(dataset[[cov]])) <= 3){
                    cov.polys <- c(cov.polys, cov)
                    next
                }
                cov.polys <- c(cov.polys, paste('poly(', cov, ',', sample(1:3, 1), ')', sep = ''))
            }

            # Double interactions
            if(length(covs) > 1){
                possible.interactions <- combn(covs, 2, simplify = FALSE)
                cov.cols <- sample(1:length(possible.interactions), sample(1:length(possible.interactions), 1))
                cov.interactions <- c()
                for(i in 1:length(cov.cols)){
                    this.interaction <- paste(possible.interactions[[i]], collapse = ':')
                    cov.interactions <- c(cov.interactions, this.interaction)
                }
            
                formula <- paste(frontier.object$outcome,
                                 '~',
                                 paste(frontier.object$treatment, '+'),
                                 paste(paste(cov.polys, collapse = ' + ')),
                                 '+',
                                 paste(paste(cov.interactions, collapse = ' + '))
                                 )
            }else{formula <- paste(frontier.object$outcome,
                                   '~',
                                   paste(frontier.object$treatment, '+'),
                                   paste(paste(cov.polys, collapse = ' + '))
                                   )
              }
            
            if(k == 1){
                formula <- paste(frontier.object$outcome, '~',  frontier.object$treatment)
            }
            print(formula)
            # run model
            if(frontier.object$ratio == 'variable'){
                w <- makeWeights(dataset, treatment)
                dataset$w <- w            
                results <- lm(formula, dataset, weights = w)
            } else {
                results <- lm(formula, dataset)
            }
            coef.dist <- c(coef.dist, coef(results)[frontier.object$treatment])
        }
        mod.dependence[[as.character(frontier.object$frontier$Xs[depend.point.inds[i]])]] <- coef.dist
    }
    
    return(list(Xs = frontier.object$frontier$Xs[point.inds], coefs = coefs, CIs = CIs, mod.dependence = mod.dependence))
}

