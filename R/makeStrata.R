makeStrata <- function(mdist, dataset, minlist){
    ##NEW -- try to calculate the parts for cemweights
    ## identify all obs that are mutually minimum to each other and call those strata
    ss <- list()
    for(i in 1:nrow(mdist)){
        ss[[length(ss)+1]] <- c(rownames(mdist)[i], names(which(mdist[i,] == minlist[rownames(mdist)[i]])))
    }
    for(i in 1:ncol(mdist)){
        ss[[length(ss)+1]] <- c(colnames(mdist)[i], names(which(mdist[,i] == minlist[colnames(mdist)[i]])))
    }
    ## Then combine all of them so that
    names(ss) <- as.character(1:length(ss))
    ss.names <- names(ss)
    for(i in 2:length(ss.names)){
        tmp <- ss[[ss.names[i]]]
        whichshare <- lapply(lapply(ss[1:(i-1)], function(x){tmp %in% x}), sum)
        ## if there is only one prior strata to combine with...
        if(sum(unlist(whichshare)) == 1){
            ss[[names(whichshare)[whichshare >0]]] <- unique(c(ss[[names(whichshare)[whichshare >0]]], tmp))
            ss[[ss.names[i]]] <- ""
        }
        ## if there is MORE THAN one prior strata to combine with...
        if(sum(unlist(whichshare)) > 1){
            ss[[ names(whichshare)[whichshare >0][1] ]] <- unique(c(unlist(ss[ names(whichshare)[whichshare >0] ]), tmp))
            ss[[ss.names[i]]] <- ""
            for(j in 2:sum(unlist(whichshare))){
                ss[[ names(whichshare)[whichshare >0][j] ]] <- ""
            }
        }
    }
    ## remove the empty holders
    ss <- ss[lapply(ss,function(x){sum(x=="")})==0]
    if( length(unlist(ss))!=nrow(dataset)){stop("The internal calculation of strata weights has messed up.")}
    ## then make an observation list with strata
    strataList <- rep(NA, nrow(dataset))
    names(strataList) <- rownames(dataset)
    for(i in 1:length(strataList)){
        strataList[i] <- names(ss)[unlist(lapply(ss, function(x){rownames(dataset)[i] %in% x}))]
    }
    return(strataList)
}
