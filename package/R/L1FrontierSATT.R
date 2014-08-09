L1FrontierSATT <-
function(treatment, outcome, dataset, breaks){    
    match.on <- colnames(dataset)[!(colnames(dataset) %in% c(treatment, outcome))]
    binnings <- getBins(dataset, treatment, match.on, breaks)
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
    class(out) <- 'L1SATTClass'
    return(out)
}
