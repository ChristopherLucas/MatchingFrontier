modelDependence <-
function(dataset,
         treatment,
         outcome = NULL,
         covariates = NULL,
         model.dependence.ests = NULL,
         verbose = TRUE,
         ratio = 'fixed',
         specifications = NULL,
         base.form = NULL,
         cutpoints = NULL,
         median = TRUE){
    set.seed(1)
    
    if(!is.null(base.form)){
        base.form <- as.formula(base.form)
        
        covs <- strsplit(as.character(base.form[3]), '\\+')
        covs <- unlist(lapply(covs, trim))
        
        base.theta <- lm(base.form, data = dataset)$coefficients[[treatment]]
        
        if(verbose){
            cat(paste('Estimate from base model:', round(base.theta, 2), '\n'))
        }
        
        N <- nrow(dataset)
        # estimate theta_p

        theta.Ps <- c()
        
        for(cov in covs){
            if(cov == treatment){next}
            
            # Formula for this iteration
            this.form <- paste(as.character(base.form[2]),
                               as.character(base.form[1]),
                               paste(covs[!(covs %in% cov)], collapse = ' + '))
            
            base.mod <- lm(base.form, data = dataset)
            
            # Split data
            if(length(unique(dataset[[cov]])) == 2){
                split.inds <- dataset[[cov]] == unique(dataset[[cov]])[1]            
                dat1 <- dataset[split.inds,]
                dat2 <- dataset[!split.inds,]
            }else{
                if(cov %in% names(cutpoints)){
                    cutpoint <- cutpoints[names(cutpoints) == cov]
                }else{
                    cutpoint <- getCutpoint(dataset, base.form, cov, median)
                }
                split.inds <- dataset[[cov]] < cutpoint
                dat1 <- dataset[split.inds,]
                dat2 <- dataset[!split.inds,]
            }

            # Get theta_ps
            dat1.est <- lm(this.form, data = dat1)$coefficients[[treatment]]
            dat2.est <- lm(this.form, data = dat2)$coefficients[[treatment]]
            
            this.theta.p <- dat1.est * (nrow(dat1) / N) + dat2.est * (nrow(dat2) / N)        
            
            if(verbose){
                cat(paste('Estimate from', cov, 'partition:', round(this.theta.p, 2), '\n'))
            }
            theta.Ps <- c(theta.Ps, this.theta.p)      
        }
        
        covs <- covs[!(covs %in% treatment)]
        failed.covs <-covs[is.na(theta.Ps)]
        
        theta.Ps <- theta.Ps[!is.na(theta.Ps)]
        
        sigma.hat.theta <- sqrt(sum((theta.Ps - base.theta) ^ 2) / length(theta.Ps))
        
        return(sigma.hat.theta)
    } else {
        
        if(is.null(covariates)){
            covariates <- colnames(dataset)[!(colnames(dataset) %in% c(treatment, outcome, 'matched.to'))]
        }
        
        if(is.null(mod.dependence.ests) & is.null(specifications)){
            stop("Must specify either 'specifications' or 'mod.dependence.ests'.")
        }
        
        if(is.null(specifications)){
            specifications <- getSpecifications(covariates, treatment, outcome, dataset, mod.dependence.ests)
        }    
        
        if(is.null(mod.dependence.ests)){
            mod.dependence.ests <- length(specifications)
        }    
        
        if(length(specifications) != mod.dependence.ests){
            stop("'mod.dependence.ests' must equal the length of 'specifications'.")
        }
        
        coef.dist <- c()
        for(k in 1:mod.dependence.ests){
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
}
