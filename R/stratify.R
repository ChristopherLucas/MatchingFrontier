# Takes a dataframe and returns a vector of length nrow(data), where
# element i is strata for observation i. 
stratify <- function (dataset){
  xx <- apply(dataset, 1, function(x) paste(x, collapse = "\r"))
  tab <- table(xx)
  st <- names(tab)
  strata <- match(xx,st)
  return(strata)
}
