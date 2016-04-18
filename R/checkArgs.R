checkArgs <-
function(QOI, metric, ratio){
    if(!(QOI %in% c("FSATT", "SATT"))){
        customStop("QOI must be either 'FSATT' or 'SATT'.", 'makeFrontier()')
    }

    if(!(metric %in% c('L1', 'Mahal', 'Euclid', "Custom"))){
        customStop("metric must be either 'L1', 'Mahal', 'Euclid', or 'Custom'.", 'makeFrontier()')
    }

    if(!(ratio %in% c('fixed', 'variable'))){
        customStop("ratio must be either 'fixed' or 'variable'.", 'makeFrontier()')
    }
}
