CustomFrontierFSATT <-
function(treatment, dataset, ratio, match.on, 
         distance.mat){
    original.rownames <- as.numeric(rownames(distance.mat))
    original.colnames <- as.numeric(colnames(distance.mat))
    row.max = max(original.rownames)
    col.max = max(original.colnames)
    if((nrow(distance.mat)+ncol(distance.mat)) != max(row.max, col.max)){
        warning('The data index has changed, please see index in the function output.')
        original.index <- sort(c(original.rownames, original.colnames))
        index.df <- data.frame(original.index)
        index.df$new.index <- rownames(index.df)
        new.row.index <- index.df[which(original.index%in%rownames(distance.mat)), ]
        new.col.index <- index.df[which(original.index%in%colnames(distance.mat)), ] 
        rownames(distance.mat) <- new.row.index[, "new.index"]
        colnames(distance.mat) <- new.col.index[, "new.index"]
        frontier <- distToFrontier(distance.mat)
        dataset$matched.to <- frontier$matched.to
        frontier$matched.to <- NULL
        out <- list(
            frontier = frontier,
            treatment = treatment,
            index = index.df
            QOI = 'FSATT',
            metric = 'Custom',
            ratio = ratio,
            dataset = dataset,
            match.on = match.on
        )
        return(out)
    }else{
    frontier <- distToFrontier(distance.mat)
    dataset$matched.to <- frontier$matched.to
    frontier$matched.to <- NULL
    out <- list(
        frontier = frontier,
        treatment = treatment,
        QOI = 'FSATT',
        metric = 'Custom',
        ratio = ratio,
        dataset = dataset,
        match.on = match.on
        )
    return(out)
    }
}


