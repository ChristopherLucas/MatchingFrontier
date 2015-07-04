modelDependence <-
function(dataset,
         treatment,
         outcome,
         covariates = NULL,
         model.dependence.ests = 100,
         verbose = TRUE,
         ratio = 'fixed'){
    if(is.null(covariates)){
        covariates <- colnames(dataset)[!(colnames(dataset) %in% c(treatment, outcome, 'matched.to'))]
    }
    set.seed(1)
    # Get points to estimate

    coef.dist <- c()
    for(k in 1:model.dependence.ests){
        covs <- sample(covariates, sample(1:length(covariates), 1))
        
        cov.polys <- c()
        for(cov in covs){
            if(length(unique(dataset[[cov]])) <= 3){
                cov.polys <- c(cov.polys, cov)
                next
            }
            cov.polys <- c(cov.polys, paste('poly(', cov, ',', sample(1:3, 1), ', raw = TRUE)', sep = ''))
        }
        
        # Double interactions
        if(length(covs) > 1){
            possible.interactions <- combn(covs, 2, simplify = FALSE)
            cov.cols <- sample(1:length(possible.interactions), sample(1:length(possible.interactions), 1))
            cov.interactions <- c()
            for(cov.ind in 1:length(cov.cols)){
                this.interaction <- paste(possible.interactions[[cov.ind]], collapse = ':')
                cov.interactions <- c(cov.interactions, this.interaction)
            }
            formula <- paste(outcome,
                             '~',
                             paste(treatment, '+'),
                             paste(paste(cov.polys, collapse = ' + ')),
                             '+',
                             paste(paste(cov.interactions, collapse = ' + '))
                             )
        }else{
            formula <- paste(outcome,
                             '~',
                             paste(treatment, '+'),
                             paste(paste(cov.polys, collapse = ' + '))
                             )
        }
        if(k == 1){
            formula <- paste(outcome, '~',  treatment)
        }
        # run model
        if(ratio == 'variable'){
            w <- makeWeights(dataset, treatment)
            dataset$w <- w            
            results <- lm(formula, dataset, weights = w)
        } else {
            results <- lm(formula, dataset)
        }
        coef.dist <- c(coef.dist, coef(results)[treatment])
    }
    return(range(coef.dist))
}
