calculateEdist <-
function(dataset, treatment, matchVars){
    cat("Calculating Euclidean distances...\n")
    ## get the names of the treated
    trtnms <- row.names(dataset)[as.logical(dataset[[treatment]])]
    ## and the names of the control units
    ctlnms <- row.names(dataset)[!as.logical(dataset[[treatment]])]
    ## calculate the mahalanobis distances if not specified
    edist <- euclidDist(trtnms, ctlnms, dataset, matchVars)
    dimnames(edist) <- list(trtnms, ctlnms)
    return(edist)
}
