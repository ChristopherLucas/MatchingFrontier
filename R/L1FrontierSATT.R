L1FrontierSATT <-
function(treatment, dataset, breaks, match.on, keep.vars){
    cat("Calculating L1 binnings...\n")
    binnings <- getBins(dataset, treatment, match.on, breaks)
    cat("Calculating L1 frontier... This may take a few minutes...\n")
    frontier <- binsToFrontier(binnings)
    out <- list(
        frontier = frontier,
        cuts = binnings$cuts,
        treatment = treatment,
        QOI = 'SATT',
        metric = 'L1',
        ratio = 'fixed',
        dataset = dataset,
        match.on = match.on,
        keep.vars = keep.vars
        )
    return(out)
}
