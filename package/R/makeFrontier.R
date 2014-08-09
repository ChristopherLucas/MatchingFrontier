makeFrontier <-
function(dataset, treatment, outcome, match.on, QOI = 'FSATT', metric = 'Mahal',
                         ratio = 'variable', breaks = NULL){

    # Check the frontier arguments 
    checkArgs(QOI, metric, ratio)
    
    # Check data and trim to suff we need
    dataset <- checkDat(dataset, treatment, outcome, match.on)
    
    if(QOI == 'FSATT' & metric == 'Mahal' & ratio == 'variable'){
        return(MahalFrontierFSATT(treatment = treatment, outcome = outcome, dataset = dataset, ratio = ratio))
    }
    if(QOI == 'FSATT' & metric == 'Mahal' & ratio == 'fixed'){
        return(MahalFrontierFSATT(treatment = treatment, outcome = outcome, dataset = dataset, ratio = ratio))
    }
    if(QOI == 'SATT' & metric == 'L1' & ratio == 'fixed'){
        return(L1FrontierSATT(treatment = treatment, outcome = outcome, dataset = dataset, breaks = breaks))
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
