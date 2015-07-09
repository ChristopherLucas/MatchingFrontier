modelDependence <-
function(dataset,
         treatment,
         outcome,
         covariates = NULL,
         model.dependence.ests = NULL,
         verbose = TRUE,
         ratio = 'fixed',
         specifications = NULL){
    if(is.null(covariates)){
        covariates <- colnames(dataset)[!(colnames(dataset) %in% c(treatment, outcome, 'matched.to'))]
    }
    set.seed(1)

    if(is.null(mod.dependence.ests) & is.null(specifications)){
        stop("Must specify either 'specifications' or 'mod.dependence.ests'.")
    }
    
    if(is.null(specifications)){
        specifications <- getSpecifications(covariates, treatment, outcome, dataset, model.dependence.ests)
    }    
    
    if(length(specifications) != model.dependence.ests){
        stop("'model.dependence.ests' must equal the length of 'specifications'.")
    }
    
    coef.dist <- c()
    for(k in 1:model.dependence.ests){
        if(k == 10){ print(k) }
        formula <- specifications[k]
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
