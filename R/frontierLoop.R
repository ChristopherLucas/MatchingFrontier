frontierLoop <- function(dataset, treatment, distvec, minlist, mdist, strataList){
    ## Series of holders to store info about the frontier
    outholder <- c();dropped <- c();imbalance <- c(); matchedSampleSize <- c();wList <- list()

    pb <- txtProgressBar(min=1,max=nrow(dataset),initial = 1, style = 3)
    ## Start a loop over the number of unique minimum distances
    for(i in 1:length(distvec)){
        setTxtProgressBar(pb, i)
        ## We keep all units that have minimum distances <= this value
        currentThreshold <- distvec[i]
        ## This is the subset of minimum units that remain
        remainingMinimums <- minlist[minlist <= currentThreshold]
        ## we need the matched sample size
        matchedSampleSize <- c(matchedSampleSize, length(remainingMinimums))
        
        this.drop <- rownames(dataset)[!(rownames(dataset) %in% c(names(remainingMinimums), dropped))]
        dropped <- c(dropped, this.drop)
        
        ## calculate the Avg of all the minimum Mahalanobis discrepancies
        imbalance <- c(imbalance, mean(remainingMinimums))
        
        ## calcalate a weights vector for this drop
        ## note, the code depends on whether there are any units in "dropped" yet
        if(length(dropped)==0){
            m_T <- table(dataset[[treatment]])[["1"]]
            m_C <- table(dataset[[treatment]])[["0"]]
        } else {
            m_T <- table(dataset[-which(rownames(dataset) %in% dropped),][[treatment]])[["1"]]
            m_C <- table(dataset[-which(rownames(dataset) %in% dropped),][[treatment]])[["0"]]
        }
        
        W <- rep(NA, nrow(dataset))
        names(W) <- rownames(dataset)
        ## fill in 1s for treated units that are still in
        W[(names(W) %in% rownames(mdist)) & (names(W) %in% names(remainingMinimums))] <- 1
        
        ## fill in 0s for all units that are out
        W[!(names(W) %in% names(remainingMinimums))] <- 0
        ## fill in weights for the rest
        cLeft <- W[(names(W) %in% colnames(mdist)) & (names(W) %in% names(remainingMinimums))]
        for(j in 1:length(unique(strataList))){
            currentStrata <- names(strataList)[strataList==unique(strataList)[j]][!names(strataList)[strataList==unique(strataList)[j]] %in% dropped]
            sTvec <- dataset[currentStrata,][[treatment]]
            sTab <- table(sTvec)
            m_T_s <- sTab["1"]
            m_C_s <- sTab["0"]
            cLeft[currentStrata[sTvec==0]] <- (m_C/m_T)*(m_T_s/m_C_s)
        }
        W[names(cLeft)] <- cLeft
        
        wList[[i]] <- W
    #if(sum(W != 0, na.rm=TRUE) < 400) {break}
    }
    close(pb)
    return(list(balance = imbalance, drops = dropped, samplesize = matchedSampleSize, metric="Mahal", weights=wList))
}
