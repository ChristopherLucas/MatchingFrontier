##################
# USER FUNCTIONS #
##################

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

    # Check the frontier arguments 
    checkArgs(QOI, metric, ratio)
    
    # Check data and trim to suff we need
    dataset <- checkDat(dataset, treatment, outcome, match.on)
    
    if(QOI == 'FSATT' & metric == 'Mahal' & ratio == 'variable'){
        return(MahalFrontierFSATT(treatment = treatment, outcome = outcome, dataset = dataset))
    }
    if(QOI == 'SATT' & metric == 'L1' & ratio == 'fixed'){
        return(L1FrontierSATT(treatment = treatment, outcome = outcome, dataset = dataset))
    }
    if(QOI == 'FSATT' & metric == 'L1' & ratio == 'variable'){
        return(L1FrontierCEM(treatment = treatment, outcome = outcome, dataset = dataset))
    }
    else{
        msg <- paste('the ', ratio, '-ratio ', metric, ' theoretical frontier is not presently calculable.', sep = '')
        customStop(msg, 'makeFrontier()')
    }
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
        customStop("the treatment is in 'match.on'. You shouldn't match on the treatment, that's bad.", 'makeFrontier()')
    }
    if(outcome %in% match.on){
        customStop("the outcome is in 'match.on'. You shouldn't match on the treatment, that's bad.", 'makeFrontier()')
    }

    # Check treatment
    if(sum(!(dataset[,treatment] %in% c(0,1))) != 0){
        customStop("the treatment must be either 0/1 (integers) or TRUE/FALSE (logical).", 'makeFrontier()')
    }
    
    # Trim the dataset to the stuff we need
    dataset <- dataset[c(treatment, outcome, match.on)]

    # Check for missing values
    if(sum(is.na(dataset)) != 0){
        customStop("missing values in the data; remove them (or impute) and try again.", 'makeFrontier()')
    }
    
    rownames(dataset) <- 1:nrow(dataset)
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

######################
# FRONTIER FUNCTIONS #
######################

#################################
#################################
#################################
#################################
# L1 SATT
#################################
#################################
#################################
#################################
print.L1SATTClass <- function(x){
    msg <- paste('An imbalance frontier with', as.character(length(x$frontier)), 'points.\n', sep = ' ')
    cat(msg)
}

L1FrontierSATT <- function(treatment, outcome, dataset){    
    match.on <- colnames(dataset)[!(colnames(dataset) %in% c(treatment, outcome))]
    binnings <- getBins(dataset, treatment, match.on)
    


    out <- list(
        frontier = frontier,
        treatment = treatment,
        outcome = outcome,
        QOI = 'SATT',
        metric = 'L1',
        ratio = 'fixed',
        dataset = dataset
        )
    class(out) <- 'L1SATTClass'
    return(out)
}

getBins <- function(dataset, treatment, match.on){
    
}





















#################################
#################################
#################################
#################################
# MAHAL FSATT
#################################
#################################
#################################
#################################

print.MahalFSATTClass <- function(x){
    msg <- paste('An imbalance frontier with', as.character(length(x$frontier$Xs)), 'points.\n', sep = ' ')
    cat(msg)
}

MahalFrontierFSATT <- function(treatment, outcome, dataset){    
    match.on <- colnames(dataset)[!(colnames(dataset) %in% c(treatment, outcome))]
    distance.mat <- calculateMdist(dataset, treatment, match.on)
    frontier <- distToFrontier(distance.mat)
    out <- list(
        frontier = frontier,
        treatment = treatment,
        outcome = outcome,
        QOI = 'FSATT',
        metric = 'Mahal',
        ratio = 'variable',
        dataset = dataset
        )
    class(out) <- 'MahalFSATTClass'
    return(out)
}

calculateMdist <- function(dataset, treatment, matchVars){
    cat("Calculating Mahalanobis distances...\n")
    ## calculate the inverse covariance matrix
    icv <- solve(cov(dataset[, matchVars]))
    ## get the names of the treated
    trtnms <- row.names(dataset)[as.logical(dataset[[treatment]])]
    ## and the names of the control units
    ctlnms <- row.names(dataset)[!as.logical(dataset[[treatment]])]
    ## calculate the mahalanobis distances if not specified
    mdist <- outer(trtnms, ctlnms, FUN = mahalDist, inv.cov = icv, data = dataset)
    dimnames(mdist) <- list(trtnms, ctlnms)
    return(mdist)
}

mahalDist <- function(Tnms, Cnms, inv.cov, dataset) {
    stopifnot(!is.null(dimnames(inv.cov)[[1]]), dim(inv.cov)[1] >
              1, all.equal(dimnames(inv.cov)[[1]], dimnames(inv.cov)[[2]]),
              all(dimnames(inv.cov)[[1]] %in% names(dataset)))
    covars <- dimnames(inv.cov)[[1]]
    xdiffs <- as.matrix(dataset[Tnms, covars])
    xdiffs <- xdiffs - as.matrix(dataset[Cnms, covars])
    rowSums((xdiffs %*% inv.cov) * xdiffs)
}

distToFrontier <- function(distance.mat){
    cat("Calculating theoretical frontier...\n")
    row.mins <- apply(distance.mat, 1, function(x) min(x))
    col.mins <- apply(distance.mat, 2, function(x) min(x))
    minimums <- c(row.mins, col.mins)
    sorted.minimums <- sort(unique(c(row.mins, col.mins)), decreasing = TRUE)
    drop.order <- lapply(sorted.minimums, function(x) as.integer(names(minimums[minimums == x])))
    cat("Calculating information for plotting the frontier...\n")
    weighted.vals <- unlist(lapply(drop.order, function(x) length(x))) * sorted.minimums
    Xs <- rev(cumsum(rev(lapply(drop.order, function(x) length(x)))))
    Ys <- rev(cumsum(rev(weighted.vals))) / Xs
    return(list(drop.order = drop.order, Xs = Xs, Ys = Ys))
}

## load('../data/lalonde.RData')
## lalonde <- lalonde[, !(colnames(lalonde) %in% c('data_id'))]

## match.on <- colnames(lalonde)[!(colnames(lalonde) %in% c('re78', 'treat'))]

## front <- MahalFrontierFSATT('treat', 're78', lalonde)

## my.frontier <- makeFrontier(dataset = lalonde, treatment = 'treat', outcome = 're78', match.on = match.on,
##                             QOI = 'FSATT', metric = 'Mahal', ratio = 'variable')


