makeFrontier <-
    function(dataset, treatment, match.on, keep.vars = NULL,
             QOI = 'FSATT', metric = 'Mahal', ratio = 'fixed',
             breaks = NULL, distance.mat = NULL){

    # Check the frontier arguments 
    checkArgs(QOI, metric, ratio)
    
    # Check data and trim to suff we need
    dataset <- checkDat(dataset, treatment, match.on, keep.vars)

    if(QOI == 'FSATT' & metric == 'Custom' & ratio == 'variable' &
       is.null(distance.mat) == FALSE){
        frontier <- CustomFrontierFSATT(treatment = treatment,
                                        dataset = dataset,
                                        ratio = ratio,
                                        match.on = match.on,
                                        keep.vars = keep.vars,
                                        distance.mat = distance.mat)
        class(frontier) <- "MahalFSATTClass"
        return(frontier)        
    }

    if(QOI == 'FSATT' & metric == 'Custom' & ratio == 'fixed' &
       is.null(distance.mat) == FALSE){
        frontier <- CustomFrontierFSATT(treatment = treatment,
                                        dataset = dataset,
                                        ratio = ratio,
                                        match.on = match.on,
                                        keep.vars = keep.vars,
                                        distance.mat = distance.mat)
        class(frontier) <- "MahalFSATTClass"
        return(frontier)        
    }

    if(QOI == 'FSATT' & metric == 'Mahal' & ratio == 'variable'){
        frontier <- MahalFrontierFSATT(treatment = treatment,
                                       dataset = dataset,
                                       ratio = ratio,
                                       match.on = match.on,
                                       keep.vars = keep.vars)
        class(frontier) <- "MahalFSATTClass"
        return(frontier)        
    }
    if(QOI == 'FSATT' & metric == 'Mahal' & ratio == 'fixed'){
        frontier <- MahalFrontierFSATT(treatment = treatment,
                                       dataset = dataset,
                                       ratio = ratio,
                                       match.on = match.on,
                                       keep.vars = keep.vars)
        class(frontier) <- "MahalFSATTClass"
        return(frontier)
    }
    if(QOI == 'FSATT' & metric == 'Euclid' & ratio == 'variable'){
        frontier <- EuclidFrontierFSATT(treatment = treatment,
                                       dataset = dataset,
                                       ratio = ratio,
                                       match.on = match.on,
                                       keep.vars = keep.vars)
        class(frontier) <- "MahalFSATTClass"
        return(frontier)        
    }
    if(QOI == 'FSATT' & metric == 'Euclid' & ratio == 'fixed'){
        frontier <- EuclidFrontierFSATT(treatment = treatment,
                                       dataset = dataset,
                                       ratio = ratio,
                                       match.on = match.on,
                                       keep.vars = keep.vars)
        class(frontier) <- "MahalFSATTClass"
        return(frontier)
    }
    if(QOI == 'SATT' & metric == 'L1' & ratio == 'fixed'){
        frontier <- L1FrontierSATT(treatment = treatment,
                                   dataset = dataset,
                                   breaks = breaks,
                                   match.on = match.on,
                                   keep.vars = keep.vars)
        class(frontier) <- "L1SATTClass"
        return(frontier)
    }
    if(QOI == 'FSATT' & metric == 'L1' & ratio == 'variable'){
        print("See the 'cem' package")
        #return(L1FrontierCEM(treatment = treatment, outcome = outcome, dataset = dataset, breaks = breaks))
    }
    else{
        msg <- paste('the ', ratio, '-ratio ', metric, ' theoretical frontier is not presently calculable.', sep = '')
        customStop(msg, 'makeFrontier()')
    }
}
