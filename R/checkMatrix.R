checkMatrix <- function(mdist, dataset, treatment){
    if(!is.matrix(mdist)){stop("the mdist provided is not a matrix")}
    ## are all the rownames of mdist treated units in the data?
    if(sum(rownames(mdist) %in% rownames(dataset[dataset[[treatment]]==1,])) != nrow(mdist)){stop("the rownames of mdist do not match the names of the treated units in the data.")}
    ## are all the treated units in the data listed as rows in mdist?
    if(sum(rownames(dataset[dataset[[treatment]]==1,]) %in% rownames(mdist)) != nrow(mdist)){stop("the rownames of mdist do not match the names of the treated units in the data.")}
    ## are all the colnames of mdist control units in the data?
    if(sum(colnames(mdist) %in% rownames(dataset[dataset[[treatment]]==0,])) != ncol(mdist)){stop("the colnames of mdist do not match the names of the control units in the data.")}  
    ## are all the control units in the data listed as columns in mdist?
    if(sum(rownames(dataset[dataset[[treatment]]==0,]) %in% colnames(mdist)) != ncol(mdist)){stop("the colnames of mdist do not match the names of the control units in the data.")}
}
