## A function to estimate causal effects
frontierEst <- function(frontierObject, dataset, myform=NULL, treatment=NULL, estCall=NULL){

  ## Mahal
  if(frontierObject$metric=="Mahal" | frontierObject$metric=="Mahalj2k" | frontierObject$metric=="L1w"){
    # Causal Effects
    effectholder <- c()
    seholder <- c() 

    cat("Calculating estimates along the frontier\n")
    pb <- txtProgressBar(min=1,max=length(frontierObject$balance),initial = 1, style = 3)
    ## some warnings 
    if(is.null(treatment)){stop("\"treatment\" must be specified (as a string).")}
        
    for(i in 1:length(frontierObject$balance)){
      setTxtProgressBar(pb, i)
      ## how far through drops do we go?
#      dropseq <- (nrow(dataset)-frontierObject$samplesize[1]):(nrow(dataset)-frontierObject$samplesize[i])
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
        est <- estCall(dataset=dataset, weights=frontierObject$weights[[i]][rownames(dataset)])
        effectholder <- c(effectholder, est$effect)
        seholder <- c(seholder, est$se)
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

    cat("Calculating estimates along the frontier\n")
    pb <- txtProgressBar(min=1,max=length(frontierObject$balance),initial = 1, style = 3)

    for(i in 1:length(frontierObject$drops)){
      setTxtProgressBar(pb, i)
      if(is.null(estCall)){
        m1 <- lm(myform, data=dataset[!(rownames(dataset)  %in% frontierObject$drops[1:i]),], )
        effectholder <- c(effectholder, summary(m1)$coeff[treatment,1])
        seholder <- c(seholder, summary(m1)$coeff[treatment,2])
      }
      if(!is.null(estCall)){
        est <- estCall(data=dataset[!(rownames(dataset)  %in% frontierObject$drops[1:i]),], weights=NULL)
        effectholder <- c(effectholder, est$effect)
        seholder <- c(seholder, est$se)
      }     
    }
    close(pb)

    q <- data.frame(x = seq(1, length(frontierObject$drops)))

    q$mean <- effectholder
    q$sd <- seholder

  }
  return(q)
}

