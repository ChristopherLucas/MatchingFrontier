#################################
# Version 1 of MatchingFrontier #
#################################

.onAttach <- function(lib, pkg){
    dcf <- read.dcf(file.path(lib, pkg, "DESCRIPTION"))
    version.msg <- paste('\n', 'Loading MatchingFrontier Version ', dcf[, 'Version'], sep = '')
    cite.msg <- "To cite:\n\nKing, Gary, Christopher Lucas, and Richard Nielsen. 2014. \"Optimizing Balance and Sample Size in Matching Methods for Causal Inference.\" Working paper."
    msg <- strwrap(paste(version.msg, cite.msg), 1 * getOption("width"))
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
}

checkDat <- function(dataset, treatment, outcome, match.on){
    keep.columns <- c(treatment, outcome, match.on)
   
    # Check if all the variables are in the data
    if(sum(!(keep.columns %in% colnames(dataset))) > 0){ 
        missing.cols <- keep.columns[!(keep.columns %in% colnames(dataset))]
        error.msg <- paste('The following columns are not in the data: ',
                           paste(missing.cols, collapse = ', '),
                           '.', sep = ''
                           )
        stop(error.msg, call. = FALSE)
    }
    
    # Make sure user isn't trying to match on the treatment or the outcome
    if(treatment %in% match.on){
        stop("The treatment is in 'match.on'. Don't match on the treatment, it's bad.", call. = FALSE)
    }
    if(outcome %in% match.on){
        stop("The outcome is in 'match.on'. Don't match on the outcome, 's bad.", call. = FALSE)
    }
    
    # Trim the dataset to the stuff we need
    dataset <- dataset[c(treatment, outcome, match.on)]

    # Check for missing values
    if(sum(is.na(dataset)) != 0){
        stop("Missing values in the data; remove them (or impute) and try again.", call. = FALSE)
    }

    return(dataset)

}

checkArgs <- function(QOI, metric, ratio){
    if(!(QOI %in% c('FSATT', 'SATT'))){
        stop("QOI must be either 'FSATT' or 'SATT'.", call. = FALSE)
    }

    if(!(metric %in% c('L1', 'Mahal'))){
        stop("metric must be either 'L1' or 'Mahal'.", call. = FALSE)
    }

    if(!(ratio %in% c('fixed', 'variable'))){
        stop("ratio must be either 'fixed' or 'variable'.", call. = FALSE)
    }
}
