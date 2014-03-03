generateDataset <- function(finalFrontierObject, dataset, number.dropped){
  ## If L1 frontier
  if(finalFrontierObject$metric=="L1"){
    keep <- !(nrow(dataset) %in% finalFrontierObject$drops[0:number.dropped])
    return(dataset[keep,])
  } 
  ## If Mahalanobis frontier
  if(finalFrontierObject$metric=="Mahal" | finalFrontierObject$metric=="Mahalj2k"){
    if(number.dropped > length(finalFrontierObject$drops)){stop(paste("number.dropped must be less than ",length(finalFrontierObject $drops),".",sep=""))}
    if(number.dropped %in% (nrow(dataset)-finalFrontierObject$samplesize)){
      dataset$w = finalFrontierObject$weights[[which((nrow(dataset)-finalFrontierObject$samplesize) == number.dropped)]]
      keep <- !(rownames(dataset) %in% finalFrontierObject$drops[0:number.dropped])
      return(dataset[keep,])
    } else {
      ## calculate the nearest two options
      min2 <- sort(abs((nrow(dataset)-finalFrontierObject$samplesize) - number.dropped))[1]
      suggestions <- (nrow(dataset)-finalFrontierObject$samplesize)[which(abs((nrow(dataset)-finalFrontierObject$samplesize) - number.dropped) %in%  min2)]
      stop(paste("There is no dataset on the frontier with ",number.dropped," dropped observations.\n  The closest options for number.dropped  are",paste(paste(suggestions[1:(length(suggestions)-1)],collapse=", ")," or ",tail(suggestions,1),".",sep="")))
    }
  }
}
