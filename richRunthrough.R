## Examples of how to use finalFrontier

## Assume that the files are in the working directory
rm(list=ls())

# Function for loading all the files in a subdirectory
sourceDir <- function(path, trace = TRUE, ...) {
   for (nm in list.files(path, pattern = "\\.[RrSsQq]$")) {
      if(trace) cat(nm,":")          
      source(file.path(path, nm), ...)
      if(trace) cat("\n")
   }
}

# Load files in subdirectory
setwd("C:\\Users\\Rich\\finalFrontier\\finalFrontier")
sourceDir(getwd())


#########################################################################
## DATA 
#########################################################################

## Lalonde

library(cem)
data(LL)
detach(package:cem)

## These are all pieces we'll want
mytreatment <- "treated"
mydataset <- LL
mydrop <- c("treated","re78")
myform <- as.formula(re78 ~ treated +age + education + black + married + nodegree
                     + re74 + re75 + hispanic + u74 + u75)
mypsform <- as.formula(treated~age+education+black+married+nodegree+re74+re75+hispanic+u74+u75)
mybreaks <- list(age=seq(-.5,100.5,1),education=seq(-.5,100.5,1),black=seq(-.5,1.5,1),
                 married=seq(-.5,1.5,1),nodegree=seq(-.5,1.5,1),
                 re74=seq(range(LL$re74)[1],range(LL$re74)[2],length.out=100),
                 re75=seq(range(LL$re75)[1],range(LL$re75)[2],length.out=100),
                 hispanic=seq(-.5,1.5,1),u74=seq(-.5,1.5,1),u75=seq(-.5,1.5,1))

#################
## Mahal frontier
#################
## calculate mahalanobis frontier
step1m <- finalFrontier(treatment=mytreatment, dataset=mydataset, drop=mydrop, metric = 'Mahal')
## calculate ATE using default lm()
step2m <- frontierEst(step1m,dataset=mydataset, myform=myform, treatment=mytreatment, drop=mydrop)
## recalculate ATE using user-specified function
## NOTE: the data MUST be called "dataset" and the weights must be specified as
##       "weights = WEIGHTS".  The result of the user-specified call
##       MUST be a two-element numeric vector that is c(estimate, standardError).
##       The call can be as many lines as you want.  All quotes must be backslashed.
myEstCall <- "summary(lm(re78 ~ treated +age + education + black + married + nodegree
                     + re74 + re75 + hispanic + u74 + u75, data=dataset, weights=WEIGHTS))$coeff[2,1:2]"
step2m <- frontierEst(step1m,dataset=mydataset, myform=myform, treatment=mytreatment, drop=mydrop, estCall=myEstCall)
## plot the frontier
frontierPlot(step1m, mydataset, step2m, drop=mydrop)
## Note that you can get the covariate mean information out of the plot if you want it
covStuff <- frontierPlot(step1m, mydataset, step2m, drop=mydrop)
head(covStuff$covs.mat)
## make a data set
mdat <- generateDataset(step1m, mydataset, number.dropped=100)
## and estimate a model with the data set we generated, using the weights
summary(lm(re78~treated,mdat,weights=w))
## Note that if you try to specify a number.dropped for which there isn't a point
##  on the mahalanobis frontier, you get an error with some suggestions.
generateDataset(step1m, mydataset, number.dropped=500)

#################
## Mahal frontier: j-to-k matching option
#################
## use j=1, k=1
step1m2 <- finalFrontier(treatment=mytreatment, dataset=mydataset, drop=mydrop, metric = 'Mahalj2k', j=1, k=1)
step1m2$metric
## calculate ATE using default lm()
step2m2 <- frontierEst(step1m2,dataset=mydataset, myform=myform, treatment=mytreatment, drop=mydrop)
## plot the frontier
frontierPlot(step1m2, mydataset, step2m2, drop=mydrop)
## make a data set
mdat <- generateDataset(step1m2, mydataset, number.dropped=100)
mdat <- generateDataset(step1m2, mydataset, number.dropped=470)
## and estimate a model with the data set we generated, using the weights
summary(lm(re78~treated,mdat,weights=w))
## try j=2, k=3
step1m2 <- finalFrontier(treatment=mytreatment, dataset=mydataset, drop=mydrop, metric = 'Mahalj2k', j=2, k=3)
## calculate ATE using default lm()
step2m2 <- frontierEst(step1m2,dataset=mydataset, myform=myform, treatment=mytreatment, drop=mydrop)
## plot the frontier
frontierPlot(step1m2, mydataset, step2m2, drop=mydrop)


##############
## L1 frontier
##############
## calculate L1 frontier
step1L <- finalFrontier(treatment=mytreatment, dataset=mydataset, drop=mydrop, metric = 'L1')
## recalculate L1 frontier with my own L1 breaks specified
step1L <- finalFrontier(treatment=mytreatment, dataset=mydataset, drop=mydrop, metric = 'L1', breaks=mybreaks)
## calculate the ATEs
step2L <- frontierEst(step1L,dataset=mydataset, myform=myform, treatment=mytreatment, drop=mydrop)
## plot the frontier
frontierPlot(step1L, mydataset, step2L, drop=mydrop)


##############
## L1w frontier (with weights)
##############
## calculate L1 frontier
step1L <- finalFrontier(treatment=mytreatment, dataset=mydataset, drop=mydrop, metric = 'L1w')
## calculate the ATEs
step2L <- frontierEst(step1L,dataset=mydataset, myform=myform, treatment=mytreatment, drop=mydrop)
## plot the frontier
frontierPlot(step1L, mydataset, step2L, drop=mydrop)


##################
## Pscore Frontier
##################
## Pscore distance matrix
psm1 <- glm(mypsform,mydataset,family=binomial(link = "logit"))
## make a distance matrix
trtnms <- rownames(mydataset[mydataset[[mytreatment]]==1,])
ctlnms <- rownames(mydataset[mydataset[[mytreatment]]==0,])
psmdist <- matrix(NA,length(trtnms),length(ctlnms))
rownames(psmdist) <- trtnms
colnames(psmdist) <- ctlnms
for(i in 1:nrow(psmdist)){
    psmdist[i,] <- abs(psm1$fitted.values[rownames(psmdist)[i]] - psm1$fitted.values[colnames(psmdist)])
}

## calculate Pscore frontier
step1p <- finalFrontier(treatment=mytreatment, dataset=mydataset, drop=mydrop, metric = 'Mahal', mdist=psmdist)
## calculate ATEs
step2p <- frontierEst(step1p,dataset=mydataset, myform=myform, treatment=mytreatment, drop=mydrop)
## plot the frontier
frontierPlot(step1p, mydataset, step2p, drop=mydrop)



