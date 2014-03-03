L1FrontierSATT <- function(treatment, dataset, drop, breaks=NULL){
    gs <- getStrata(treatment, dataset, drop, breaks=breaks)
    strata <- gs$strata
    mycut <- gs$mycut
    names(strata) <- dataset[,which(colnames(dataset) == treatment)]
    unique.strata <- unique(strata)
    strataholder <- list()
    for(i in 1:length(unique.strata)){
        strataholder[[i]] <- which(strata==unique.strata[i])
    }

    drops <- c()
    L1s <- c(L1(strataholder))
    samplesize <- c()
    # Remove obs from imbalanced strata
    while(1){
    # get differences
        difference.vec <- c()
     
        treated.vec <- c()
        control.vec <- c()
        for(strat in strataholder){
            treated.vec <- c(treated.vec, sum((names(strat) == 1)))
            control.vec <- c(control.vec, sum((names(strat) == 0)))
        }
        difference.vec <- treated.vec/sum(treated.vec) - control.vec/sum(control.vec)

        difference.vec[difference.vec > 0] <- 0
       
        drop <- which(abs(difference.vec) == max(abs(difference.vec)))[1]
        drop.obs <- which(names(strataholder[[drop]]) == 0)[1]
        dropped <- strataholder[[drop]][drop.obs]
        drops <- c(drops, rownames(dataset)[dropped])
        samplesize <- c(samplesize, nrow(dataset)-length(drops))
        strataholder[[drop]] <- strataholder[[drop]][-drop.obs]

        L1s <- c(L1s, L1(strataholder))
        if(length(L1s) %% 1000 == 0){
          print(length(L1s))
          print(L1s[length(L1s)])
        }
        if(L1s[length(L1s)] > L1s[length(L1s) - 1]){break}
    }
    return(list(balance = L1s[1:length(L1s) - 1], drops = unname(drops)[1:length(drops) - 1],
                samplesize=samplesize[1:length(samplesize) - 1], metric="L1", breaks=mycut))
}
