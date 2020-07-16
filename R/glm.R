library(margins)
library(MatchingFrontier)

data(lalonde)
lalonde.match.on <- colnames(lalonde)[!(colnames(lalonde) %in% c('re78', 'treat'))]
frontier.object <- makeFrontier(dataset = lalonde,
                                treatment = 'treat',
                                match.on = lalonde.match.on,
                                QOI = 'FSATT', metric = 'Mahal', ratio = 'fixed')

glm.estimate(frontier.object = frontier.object, my.form = 're78 ~ treat', glm.family = gaussian(), marginal = F) 




getSpecifications <- function(covariates, treatment, outcome, dataset, N){
  
  specifications <- c()
  
  for(i in 1:N){
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
    if(i == 1){
      formula <- paste(outcome, '~',  treatment)
    }
    specifications <- c(specifications, formula)
  }
  return(specifications)
}



glm.estimate <- function(frontier.object,
                         my.form,
                         glm.family = NULL,
                         prop.estimated = 1,
                         mod.dependence.formula = NULL,
                         continuous.vars = NA,
                         seed = 1,
                         model.dependence.ests = 100,
                         means.as.cutpoints = TRUE,
                         Athey.Imbens = FALSE,
                         alpha=0.95, 
                         covariates = frontier.object$match.on,
                         ratio = frontier.object$ratio,
                         marginal = FALSE){
  if(marginal == FALSE){
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

return(list(Xs = frontier.object$frontier$Xs[point.inds], 
            coefs = unlist(coefs), CIs = CIs, 
            mod.dependence = mod.dependence, 
            method = "treatment coefficient intervals"))
  }else{
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
          results <- summary(margins(glm(formula, dataset, weights = w, family = glm.family), variables = treatment))
          
        } else {
          results <- summary(margins(glm(formula, dataset, family = glm.family), variables = treatment))
        }
        coef.dist <- c(coef.dist, results$AME)
      }
      return(range(coef.dist))
      
      
      if(frontier.object$ratio == 'variable'){
        w <- makeWeights(dataset, treatment)
        dataset$w <- w            
        results <- summary(margins(glm(my.form, dataset, weights = w, family = glm.family), variables = treatment))
      } else {
        results <- summary(margins(glm(my.form, dataset, family = glm.family), variables = treatment))
      }
      
      coefs[i] <- results$AME
      CIs[[i]] <- results[, c("lower", "upper")]     
      mod.dependence[[i]] <- range(coef.dist)
      
      setTxtProgressBar(pb, i)
    }
    close(pb)
    
    return(list(Xs = frontier.object$frontier$Xs[point.inds], 
                coefs = unlist(coefs), CIs = CIs, 
                mod.dependence = mod.dependence, 
                method = "average marginal treatment effects intervals"))
}
}
