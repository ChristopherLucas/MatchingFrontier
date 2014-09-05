L1FrontierSATT <-
function(treatment, outcome, dataset, breaks){
    cat("Calculating L1 binnings...\n")
    match.on <- colnames(dataset)[!(colnames(dataset) %in% c(treatment, outcome))]
    binnings <- getBins(dataset, treatment, match.on, breaks)
    cat("Calculating L1 frontier... This may take a few minutes...\n")
    frontier <- binsToFrontier(binnings)
    out <- list(
        frontier = frontier,
        cuts = binnings$cuts,
        treatment = treatment,
        outcome = outcome,
        QOI = 'SATT',
        metric = 'L1',
        ratio = 'fixed',
        dataset = dataset
        )
    return(out)
}
