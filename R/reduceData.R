reduceData <- function(data, breaks=NULL, collapse=FALSE){
  if (!is.data.frame(data))
        stop("Data must be a dataframe", call. = FALSE)
 vnames <- colnames(data)
 nv <- length(vnames)
 new.breaks <- vector(dim(data)[2], mode="list")
 names(new.breaks) <- vnames
 for (i in 1:nv){
   tmp <- reduce.var(data[[i]], breaks[[vnames[i]]] )
   new.breaks[[vnames[i]]] <- tmp$breaks
   data[[i]] <- tmp$x
  }
 if(collapse)
  return(list(data=collapseData(data), breaks=new.breaks))
 
 return(list(data=data, breaks=new.breaks))
}

collapseData <- function(data){
  apply(data,1, function(x) paste(x, collapse="\r"))	
}
