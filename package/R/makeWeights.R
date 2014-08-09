makeWeights <-
function(dataset, treatment){
    strata <- getMahalStrata(rownames(dataset), dataset$matched.to)
    w <- rep(NA, nrow(dataset))
    w[dataset[[treatment]] == 1] <- 1
    for(s in unique(strata)){
        num.treated <- sum(strata == s & dataset[[treatment]] == 1)
        num.control <- sum(strata == s & dataset[[treatment]] == 0)
        w[strata == s & dataset[[treatment]] == 0] <- num.treated / num.control
    }
    return(w)
}
