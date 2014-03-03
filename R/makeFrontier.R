# This gets the frontier
makeFrontier <- function(treatment, dataset, drop, mdist = NULL, QOI, metric, ratio){
    if(sum(is.na(dataset)) > 0) stop('ERROR: Dataframe has missing values')
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
