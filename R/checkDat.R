checkDat <-
function(dataset, treatment, match.on){    
       
    # Make sure user isn't trying to match on the treatment or the outcome
    if(treatment %in% match.on){
        customStop("the treatment is in 'match.on'. You shouldn't match on the treatment, that's bad.", 'makeFrontier()')
    }

    # Check treatment
    if(sum(!(dataset[,treatment] %in% c(0,1))) != 0){
        customStop('the treatment must be either 0/1 (integers) or "TRUE"/"FALSE" (logical).', 'makeFrontier()')
    }

    # Check for missing values
    if(sum(is.na(dataset)) != 0){
        customStop("missing values in the data; remove them (or impute) and try again.", 'makeFrontier()')
    }
    
    # Check if a variable name is repeated twice in match.on
    if (length(match.on) != length(unique(match.on))){
        customStop("repeated variables in match.on; remove them and try again.", 'makeFrontier()')
    } 
    
    rownames(dataset) <- 1:nrow(dataset)
    return(dataset)

}
