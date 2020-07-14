estimateEffects <-
function(frontier.object,
         my.form,
         glm.family = NULL,
         prop.estimated = 1,
         mod.dependence.formula = NULL,
         continuous.vars = NA,
         seed = 1,
         model.dependence.ests = 100,
         means.as.cutpoints = TRUE,
         Athey.Imbens = FALSE,
         alpha=0.95){
    
    set.seed(seed)
    
    if(is.null(glm.family)){
  # Check if specify a base specification model when choosing to estimate the Athey-Imbens intervals   
    if(Athey.Imbens = TRUE & mod.dependence.formula = NULL){
      msg <- c("please specify `model.dependence.formula` if `Athey.Imbens` is set to be TRUE.")
      customStop(msg, 'estimateEffects()')
    } 
    
    if(Athey.Imbens = FALSE & is.null(mod.dependence.formula) = FALSE){
      msg <- c("please don't specify `model.dependence.formula` if `Athey.Imbens` is set to be FALSE.")
      customStop(msg, 'estimateEffects()')
    } 
  
  # These are the points that we'll estimate
    point.inds <- sort(sample(1:length(frontier.object$frontier$Xs),
                              round(length(frontier.object$frontier$Xs) * prop.estimated)))
    coefs <- vector(mode="list", length = length(point.inds))
    CIs <- vector(mode="list", length= length(point.inds))
    mod.dependence <- vector(mode="list", length= length(point.inds))

    outcome <- all.vars(as.formula(my.form))[1]
    treatment <- frontier.object$treatment
  
    if(is.null(mod.dependence.formula)){
        covs <- frontier.object$match.on
        specifications <- getSpecifications(covs,
                                            treatment,
                                            outcome,
                                            frontier.object$dataset,
                                            model.dependence.ests)
        
        pb <- txtProgressBar(min = 1, max = length(point.inds), style = 3)
        for(i in 1:length(point.inds)){
            this.dat.inds <- unlist(frontier.object$frontier$drop.order[point.inds[i]:length(frontier.object$frontier$drop.order)])
            dataset <- frontier.object$dataset[this.dat.inds,]
            
            this.mod.dependence <- modelDependence(dataset = dataset,
                                                   treatment = treatment,
                                                   outcome = outcome,
                                                   covariates = covs,
                                                   model.dependence.ests = model.dependence.ests,
                                                   verbose = FALSE,
                                                   ratio = frontier.object$ratio,
                                                   specifications = specifications)
            
            if(frontier.object$ratio == 'variable'){
                w <- makeWeights(dataset, treatment)
                dataset$w <- w            
                results <- lm(my.form, dataset, weights = w)
            } else {
                results <- lm(my.form, dataset)
            }
            
            coefs[i] <- coef(results)[frontier.object$treatment]
            CIs[[i]] <- confint(results, level = alpha)[frontier.object$treatment,]       
            mod.dependence[[i]] <- this.mod.dependence
            
            setTxtProgressBar(pb, i)
        }
        close(pb)
        
        return(list(Xs = frontier.object$frontier$Xs[point.inds], coefs = unlist(coefs), CIs = CIs, mod.dependence = mod.dependence, method = "simulated intervals"))
        
    } else {
        if(!is.na(continuous.vars[1])){
            if(means.as.cutpoints){
                cutpoints <- lapply(continuous.vars, function(x) mean(frontier.object$dataset[[x]]))
                names(cutpoints) <- continuous.vars
            } else {
                cutpoints <- getCutpointList(frontier.object$dataset, mod.dependence.formula, continuous.vars)
            }
        } else{ cutpoints <- NA }
        
        covs <- strsplit(as.character(mod.dependence.formula[3]), '\\+')
        covs <- unlist(lapply(covs, trim))
        covs <- covs[!(covs %in% treatment)]
        
        pb <- txtProgressBar(min = 1, max = length(point.inds), style = 3)
        
        for(i in 1:length(point.inds)){
            this.dat.inds <- unlist(frontier.object$frontier$drop.order[point.inds[i]:length(frontier.object$frontier$drop.order)])
            dataset <- frontier.object$dataset[this.dat.inds,]
            
            if(frontier.object$ratio == 'variable'){
                w <- makeWeights(dataset, treatment)
                dataset$w <- w            
                results <- lm(my.form, dataset, weights = w)
            } else {
                results <- lm(my.form, dataset)
            }
            
            this.mod.dependence <- tryCatch(modelDependence(dataset = dataset,
                                                            treatment = treatment,
                                                            base.form = mod.dependence.formula,
                                                            verbose = FALSE,
                                                            cutpoints = cutpoints),
                                            error = function(e) NA)
           
            if(!is.na(this.mod.dependence[1])){           
                this.sig.hat <- this.mod.dependence
                
            } else{
                this.sig.hat <- NA
            }
            
            
            coefs[i] <- coef(results)[frontier.object$treatment]
            CIs[[i]] <- confint(results, level = alpha)[frontier.object$treatment,]
            mod.dependence[[i]] <- c(coefs[[i]] - this.sig.hat, coefs[[i]] + this.sig.hat)
            setTxtProgressBar(pb, i)
        }
        close(pb)
        frontierEstimates <- list(Xs = frontier.object$frontier$Xs[point.inds], coefs = unlist(coefs), CIs = CIs, mod.dependence = mod.dependence, method = "Athey-Imbens intervals")
        class(frontierEstimates) <- "frontierEstimates"
        return(frontierEstimates)        
    }} else{
      # Warning that Athey-Imbens intervals don't work for GLM
      if(Athey.Imbens == TRUE){
        msg <- c("please change 'Athey.Imbens' to be FALSE since the Athey-Imbens model-dependence intervals don't work for GLM.")
        customStop(msg, 'estimateEffects()')
      }
      
      
      # These are the points that we'll estimate
      point.inds <- sort(sample(1:length(frontier.object$frontier$Xs),
                                round(length(frontier.object$frontier$Xs) * prop.estimated)))
      coefs <- vector(mode="list", length = length(point.inds))
      CIs <- vector(mode="list", length= length(point.inds))
      mod.dependence <- vector(mode="list", length= length(point.inds))
      
      outcome <- all.vars(as.formula(my.form))[1]
      treatment <- frontier.object$treatment
      
      covs <- frontier.object$match.on
      specifications <- getSpecifications(covs,
                                          treatment,
                                          outcome,
                                          frontier.object$dataset,
                                          model.dependence.ests)
      
      pb <- txtProgressBar(min = 1, max = length(point.inds), style = 3)
      for(i in 1:length(point.inds)){
        this.dat.inds <- unlist(frontier.object$frontier$drop.order[point.inds[i]:length(frontier.object$frontier$drop.order)])
        dataset <- frontier.object$dataset[this.dat.inds,]
      
      
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
      
      
      if(frontier.object$ratio == 'variable'){
        w <- makeWeights(dataset, treatment)
        dataset$w <- w            
        results <- lm(my.form, dataset, weights = w, family = glm.family)
      } else {
        results <- lm(my.form, dataset, family = glm.family)
      }
      
      coefs[i] <- coef(results)[frontier.object$treatment]
      CIs[[i]] <- confint(results, level = alpha)[frontier.object$treatment,]       
      mod.dependence[[i]] <- range(coef.dist)
      
      setTxtProgressBar(pb, i)
    }
  close(pb)
  
  return(list(Xs = frontier.object$frontier$Xs[point.inds], coefs = unlist(coefs), CIs = CIs, mod.dependence = mod.dependence, method = "simulated intervals"))
    }
  }

