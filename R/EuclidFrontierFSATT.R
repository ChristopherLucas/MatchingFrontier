EuclidFrontierFSATT <-
function(treatment, dataset, ratio, match.on, keep.vars){    
    distance.mat <- calculateEdist(dataset, treatment, match.on)
    frontier <- distToFrontier(distance.mat)
    dataset$matched.to <- frontier$matched.to
    frontier$matched.to <- NULL
    out <- list(
        frontier = frontier,
        treatment = treatment,
        QOI = 'FSATT',
        metric = 'Mahal',
        ratio = ratio,
        dataset = dataset,
        match.on = match.on,
        keep.vars = keep.vars
        )
    return(out)
}
