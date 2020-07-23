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
            cov.polys <- c(cov.polys, paste('stats::poly(', cov, ',', sample(1:3, 1), ', raw = TRUE)', sep = ''))
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
