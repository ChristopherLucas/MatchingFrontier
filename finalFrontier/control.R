library(ggplot2)

# These are the functions that users are supposed to call. 

# This gets the frontier
finalFrontier <- function(treatment, dataset, drop, metric, mdist = NULL, breaks=NULL, j=NULL, k=NULL){

  # get frontier
  if(metric == 'L1'){
    frontier <- L1Frontier(treatment, dataset, drop, breaks)
  }
  if(metric == 'L1j2k'){
    frontier <- L1Frontier2(treatment, dataset, drop, breaks, j, k)
  }
  if(metric == 'Mahal'){
    frontier <- MahalFrontier(treatment, dataset, drop, mdist)
  }
  if(metric == 'Mahalj2k'){
    frontier <- MahalFrontier2(treatment, dataset, drop, mdist, j, k)
  }
  return(frontier)
}



## A function to estimate causal effects
frontierEst <- function(frontierObject, dataset, myform=NULL, treatment=NULL, estCall=NULL, drop=NULL){

  ## Mahal
  if(frontierObject$metric=="Mahal" | frontierObject$metric=="Mahalj2k" ){
    # Causal Effects
    effectholder <- c()
    seholder <- c() 

    cat("Calculating estimates along the frontier\n")
    pb <- txtProgressBar(min=1,max=length(frontierObject$balance),initial = 1, style = 3)

    ## replace the name DATASET with the subsetted data
## I've broken my thing for putting any function you want into the mahalanobis estimation.
#    if(!is.null(estCall)){
#      estCall <- gsub("DATASET","dataset[!(rownames(dataset) %in% frontierObject$drops[dropseq]),]",estCall, fixed=T)
#    } else { 
    ## sub in thing for weights now in the user-provided data
    if(!is.null(estCall)){
      estCall <- gsub("WEIGHTS","frontierObject$weights[[i]][rownames(dataset)]",estCall, fixed=T)
    } else { 
    ## some warnings 
      if(is.null(treatment)){stop("\"treatment\" must be specified (as a string).")}
      if(is.null(myform)){stop("\"myform\" must be specified (as a formula).")}
    }
    for(i in 1:length(frontierObject$balance)){
      setTxtProgressBar(pb, i)
      ## how far through drops do we go?
      dropseq <- (nrow(dataset)-frontierObject$samplesize[1]):(nrow(dataset)-frontierObject$samplesize[i])
      ## If there isn't a model specified, we just do linear regression with the formula
      if(is.null(estCall)){
        dataset$myw <- frontierObject$weights[[i]][rownames(dataset)]
#        m1 <- lm(myform, data=dataset[!(rownames(dataset) %in% frontierObject$drops[dropseq]),])
        m1 <- lm(myform, data=dataset, weights=myw)
        effectholder <- c(effectholder, summary(m1)$coeff[treatment,1])
        seholder <- c(seholder, summary(m1)$coeff[treatment,2])
      }
      ## if estCall is specified, then we just take whatever quantity of interest it is
      if(!is.null(estCall)){
        est <- eval(parse(text=estCall))
        effectholder <- c(effectholder, est[1])
        seholder <- c(seholder, est[2])
      }
    }
    close(pb)

    q <- data.frame(x = nrow(dataset)-frontierObject$samplesize)
    q$mean <- effectholder
    q$sd <- seholder
  }

  ## Mahal2 -- DELETE THIS VERSION IF THE WEIGHTS END UP WORKING ABOVE
  if(frontierObject$metric=="Mahalj2kXXXXXXXXXXX"){
    # Causal Effects
    effectholder <- c()
    seholder <- c() 

    cat("Calculating estimates along the frontier\n")
    pb <- txtProgressBar(min=1,max=length(frontierObject$balance),initial = 1, style = 3)

    ## replace the name DATASET with the subsetted data
    if(!is.null(estCall)){
      estCall <- gsub("DATASET","dataset[!(rownames(dataset) %in% frontierObject$drops[dropseq]),]",estCall, fixed=T)
    } else { 
    ## some warnings 
      if(is.null(treatment)){stop("\"treatment\" must be specified (as a string).")}
      if(is.null(myform)){stop("\"myform\" must be specified (as a formula).")}
    }
    for(i in 1:length(frontierObject$balance)){
      setTxtProgressBar(pb, i)
      ## how far through drops do we go?
      dropseq <- (nrow(dataset)-frontierObject$samplesize[1]):(nrow(dataset)-frontierObject$samplesize[i])
      ## If there isn't a model specified, we just do linear regression with the formula
      if(is.null(estCall)){
        m1 <- lm(myform, data=dataset[!(rownames(dataset) %in% frontierObject$drops[dropseq]),])
        effectholder <- c(effectholder, summary(m1)$coeff[treatment,1])
        seholder <- c(seholder, summary(m1)$coeff[treatment,2])
      }
      ## if estCall is specified, then we just take whatever quantity of interest it is
      if(!is.null(estCall)){
        est <- eval(parse(text=estCall))
        effectholder <- c(effectholder, est[1])
        seholder <- c(seholder, est[2])
      }
    }
    close(pb)

    q <- data.frame(x = nrow(dataset)-frontierObject$samplesize)
    q$mean <- effectholder
    q$sd <- seholder
  }

  ## L1
  if(frontierObject$metric=="L1"){
    # Causal Effects
    effectholder <- c()
    seholder <- c() 

    ## replace the name DATASET with the subsetted data
    if(!is.null(estCall)){
      estCall <- gsub("DATASET","dataset[!(rownames(dataset)  %in% frontierObject$drops[1:i]),]",estCall, fixed=T)
    } else { 
    ## some warnings 
      if(is.null(treatment)){stop("\"treatment\" must be specified (as a string).")}
      if(is.null(myform)){stop("\"myform\" must be specified (as a formula).")}
    }

    cat("Calculating estimates along the frontier\n")
    pb <- txtProgressBar(min=1,max=length(frontierObject$balance),initial = 1, style = 3)

    for(i in 1:length(frontierObject$drops)){
      setTxtProgressBar(pb, i)
      if(is.null(estCall)){
        m1 <- lm(myform, data=dataset[!(rownames(dataset)  %in% frontierObject$drops[1:i]),], )
        effectholder <- c(effectholder, summary(m1)$coeff["treated",1])
        seholder <- c(seholder, summary(m1)$coeff["treated",2])
      }
      if(!is.null(estCall)){
        est <- eval(parse(text=estCall))
        effectholder <- c(effectholder, est[1])
        seholder <- c(seholder, est[2])
      }     
    }
    close(pb)

    q <- data.frame(x = seq(1, length(frontierObject$drops)))

    q$mean <- effectholder
    q$sd <- seholder

  }
  return(q)
}



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
#    ## If Mahalanobis 2 frontier
#  if(finalFrontierObject$metric=="Mahalj2kXXXXX"){
#    if(number.dropped > length(finalFrontierObject$drops)){stop(paste("number.dropped must be less than ",length(finalFrontierObject $drops),".",sep=""))}
#    if(number.dropped %in% (nrow(dataset)-finalFrontierObject$samplesize)){
#      keep <- !(rownames(dataset) %in% finalFrontierObject$drops[0:number.dropped])
#      return(dataset[keep,])
#    } else {
#      ## calculate the nearest two options
#      min2 <- sort(abs((nrow(dataset)-finalFrontierObject$samplesize) - number.dropped))[1]
#      suggestions <- (nrow(dataset)-finalFrontierObject$samplesize)[which(abs((nrow(dataset)-finalFrontierObject$samplesize) - number.dropped) %in%  min2)]
#      stop(paste("There is no dataset on the frontier with ",number.dropped," dropped observations.\n  The closest options for number.dropped  are",paste(paste(suggestions[1:(length(suggestions)-1)],collapse=", ")," or ",tail(suggestions,1),".",sep="")))
#    }
#  }
}


