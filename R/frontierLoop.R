frontierLoop <- function(dataset, treatment, distvec, minlist, mdist, strataList){
    ## Series of holders to store info about the frontier
    outholder <- c();dropped <- c();imbalance <- c(); matchedSampleSize <- c();wList <- list()

    ## Start a loop over the number of unique minimum distances
     for(i in 1:length(distvec)){
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
        
        ## NEW -- calcalate a weights vector for this drop
        m_T <- table(dataset[[treatment]])[["1"]]
        m_C <- table(dataset[[treatment]])[["0"]]
        
        W <- rep(NA, nrow(dataset))
        names(W) <- rownames(dataset)
        ## fill in 1s for treated units that are still in
        W[(names(W) %in% rownames(mdist)) & (names(W) %in% names(remainingMinimums))] <- 1

        ## fill in 0s for all units that are out
        W[!(names(W) %in% names(remainingMinimums))] <- 0
        ## fill in weights for the rest
        cLeft <- W[(names(W) %in% colnames(mdist)) & (names(W) %in% names(remainingMinimums))]
        for(j in 1:length(cLeft)){
            sTab <- table(dataset[ names(strataList)[ strataList == strataList[names(cLeft[j])] ], ][[treatment]])
            m_T_s <- sTab["1"]
            m_C_s <- sTab["0"]
            W[names(cLeft[j])] <- (m_C/m_T)*(m_T_s/m_C_s)
        }
        wList[[i]] <- W
        if(sum(W != 0, na.rm=TRUE) < 400) {break}
    }
    return(list(balance = imbalance, drops = dropped, samplesize = matchedSampleSize, metric="Mahal", weights=wList))
}
