glm.modelDependence <-
  function(dataset,
           treatment,
           outcome = NULL,
           covariates = NULL,
           model.dependence.ests = NULL,
           verbose = TRUE,
           ratio = 'fixed',
           specifications = specifications,
           glm.family = glm.family,
           base.form = NULL,
           marginal = FALSE,
           seed = 02139){
    set.seed(seed)
    
    if(marginal == FALSE){ 
      if(is.null(covariates)){
      covariates <- colnames(dataset)[!(colnames(dataset) %in% c(treatment, outcome, 'matched.to'))]
    }
      
      if(is.null(model.dependence.ests) & is.null(specifications)){
        stop("Must specify either 'specifications' or 'model.dependence.ests'.")
      }
      
      if(is.null(specifications)){
        specifications <- getSpecifications(covariates, treatment, outcome, dataset, model.dependence.ests)
      }    
      
      if(is.null(model.dependence.ests)){
        model.dependence.ests <- length(specifications)
      }    
      
      if(length(specifications) != model.dependence.ests){
        stop("'model.dependence.ests' must equal the length of 'specifications'.")
      }
      
      coef.dist <- c()
      for(k in 1:model.dependence.ests){
        formula <- specifications[k]
        # run model
        if(ratio == 'variable'){
          w <- makeWeights(dataset, treatment)
          dataset$w <- w            
          results <- glm(formula, dataset, weights = w, family = glm.family)
        } else {
          results <- glm(formula, dataset, family = glm.family)
        }
        coef.dist <- c(coef.dist, coef(results)[treatment])
      }
      return(range(coef.dist))
    }else {
      
      if(is.null(covariates)){
        covariates <- colnames(dataset)[!(colnames(dataset) %in% c(treatment, outcome, 'matched.to'))]
      }
      
      if(is.null(model.dependence.ests) & is.null(specifications)){
        stop("Must specify either 'specifications' or 'model.dependence.ests'.")
      }
      
      if(is.null(specifications)){
        specifications <- getSpecifications(covariates, treatment, outcome, dataset, model.dependence.ests)
      }    
      
      if(is.null(model.dependence.ests)){
        model.dependence.ests <- length(specifications)
      }    
      
      if(length(specifications) != model.dependence.ests){
        stop("'model.dependence.ests' must equal the length of 'specifications'.")
      }
      
        
        coef.all <- c()
        for(k in 1:model.dependence.ests){
          formula <- specifications[k]
          # get coefs
          if(ratio == 'variable'){
            w <- makeWeights(dataset, treatment)
            dataset$w <- w            
            result <- glm(formula, dataset, weights = w, family = glm.family)
            
          } else {
            result <- glm(formula, dataset, family = glm.family)
          }
          coef.all <- c(coef.all, coef(result)[treatment])
        }
       
    max.mod <- specifications[which.max(coef.all)]
    min.mod <- specifications[which.min(coef.all)]
    
    if(ratio == 'variable'){
      w <- makeWeights(dataset, treatment)
      dataset$w <- w 
    AME.max <- summary(margins(glm(max.mod, dataset, weights = w, family = glm.family), variables = treatment))$AME
    AME.min <- summary(margins(glm(min.mod, dataset, weights = w, family = glm.family), variables = treatment))$AME
    }else{
      AME.max <- summary(margins(glm(max.mod, dataset, family = glm.family), variables = treatment))$AME
      AME.min <- summary(margins(glm(min.mod, dataset, family = glm.family), variables = treatment))$AME
    }
    return(c(AME.min, AME.max))
    }
  }
