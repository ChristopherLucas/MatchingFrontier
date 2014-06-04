#################################
# Version 1 of MatchingFrontier #
#################################

.onAttach <- function(lib, pkg){
    dcf <- read.dcf(file.path(lib, pkg, "DESCRIPTION"))

    version.msg <- paste('\n', 'Loading MatchingFrontier Version ', dcf[, 'Version'], sep = '')

    cite.msg <- "King, Gary, Christopher Lucas, and Richard Nielsen. 2014. \"Optimizing Balance and Sample Size in Matching Methods for Causal Inference.\" Working paper."
    cite.msg <- paste(strwrap(cite.msg), collapse = "\n")

    bib.msg <- "@article{King14,\n\ttitle={Optimizing Balance and Sample Size in Matching Methods for Causal Inference},\n\tauthor={King, Gary and Lucas, Christopher and Nielsen, Richard},\n\tjournal={Working Paper},\n\tyear={2014}\n}\n"
        
    cite.msg <- paste('## Citation ##\n', cite.msg, sep = '')

    bib.msg <- paste('## BibTeX ##\n', bib.msg, sep = '')
    msg <- paste(version.msg, cite.msg, bib.msg, sep = '\n\n')
    packageStartupMessage(msg)
}

makeFrontier <- function(dataset, treatment, outcome, match.on, QOI, metric, ratio){

    # Check data and trim to suff we need
    dataset <- checkDat(dataset, treatment, outcome, match.on)

    # Check the frontier arguments 
    checkArgs(QOI, metric, ratio)
    
    if(QOI == 'FSATT' & metric == 'Mahal' & ratio == 'variable'){
        return(MahalFrontierFSATT(treatment=treatment, dataset, drop, mdist))
    }
    if(QOI == 'SATT' & metric == 'L1' & ratio == 'fixed'){
        return(L1FrontierSATT(treatment=treatment, dataset, drop))
    }
    if(QOI == 'FSATT' & metric == 'L1' & ratio == 'variable'){
        return(L1FrontierCEM(treatment=treatment, dataset, drop))
    }
    else{
        msg <- paste('The', ratio, 'ratio', metric, 'theoretical frontier is not calculable.', sep = ' ')


        customStop(msg, 'makeFrontier()')
}

checkDat <- function(dataset, treatment, outcome, match.on){
    keep.columns <- c(treatment, outcome, match.on)
   
    # Check if all the variables are in the data
    if(sum(!(keep.columns %in% colnames(dataset))) > 0){ 
        missing.cols <- keep.columns[!(keep.columns %in% colnames(dataset))]
        error.msg <- paste('the following columns are not in the data: ',
                           paste(missing.cols, collapse = '\n'), sep = '\n'
                           )
        customStop(error.msg, 'makeFrontier()')
    }
    
    # Make sure user isn't trying to match on the treatment or the outcome
    if(treatment %in% match.on){
        customStop("the treatment is in 'match.on'. Don't match on the treatment, it's bad.", 'makeFrontier()')
    }
    if(outcome %in% match.on){
        customStop("the outcome is in 'match.on'. Don't match on the outcome, it's bad.", 'makeFrontier()')
    }
    
    # Trim the dataset to the stuff we need
    dataset <- dataset[c(treatment, outcome, match.on)]

    # Check for missing values
    if(sum(is.na(dataset)) != 0){
        customStop("missing values in the data; remove them (or impute) and try again.", 'makeFrontier()')
    }

    return(dataset)

}

checkArgs <- function(QOI, metric, ratio){
    if(!(QOI %in% c('FSATT', 'SATT'))){
        customStop("QOI must be either 'FSATT' or 'SATT'.", 'makeFrontier()')
    }

    if(!(metric %in% c('L1', 'Mahal'))){
        customStop("metric must be either 'L1' or 'Mahal'.", 'makeFrontier()')
    }

    if(!(ratio %in% c('fixed', 'variable'))){
        customStop("ratio must be either 'fixed' or 'variable'.", 'makeFrontier()')
    }
}

customStop <- function(msg, func){
    custom.msg <- paste('In ', func, ', ', msg, sep = '')
    stop(custom.msg, call. = FALSE)
}
