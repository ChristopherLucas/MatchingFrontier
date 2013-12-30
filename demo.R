#Using the Lalonde data from the CEM package, the following examples demonstrate how to 
# generate each of the three frontiers we've implemented. 

library(cem)
data(LL)
detach(package:cem)
library(finalFrontier)
library(foreign)

# LALONDE ILLUSTRATION

# S=1 is fixed ratio matching
# Label stuff we'll use for each of the frontiers
mytreatment <- "treated"
mydataset <- LL
mydrop <- c("treated","re78")
myform <- as.formula(re78 ~ treated +age + education + black + married + nodegree
                     + re74 + re75 + hispanic + u74 + u75)

# Mahalanobis FSATT Frontier
step1m <- makeFrontier(treatment="treated", dataset=LL, drop=c("re78", "treated"), QOI = 'FSATT', metric = 'Mahal', S = 0)
## calculate ATE using default lm()
step2m <- frontierEst(step1m,dataset=mydataset, myform=myform, treatment=mytreatment, drop=mydrop)
frontierMultiPlot(step1m, mydataset, step2m, drop=mydrop)
mdat <- generateDataset(step1m, mydataset, number.dropped=100)
summary(lm(re78~treated,mdat,weights=w))

# L1 SATT Frontier
step1m <- makeFrontier(treatment="treated", dataset=LL, drop=c("re78", "treated"), QOI = 'SATT', metric = 'L1', S = 1)
## calculate ATE using default lm()
step2m <- frontierEst(step1m,dataset=mydataset, myform=myform, treatment=mytreatment, drop=mydrop)
frontierPlot(step1m, mydataset, step2m, drop=mydrop)
mdat <- generateDataset(step1m, mydataset, number.dropped=100)
summary(lm(re78~treated,mdat))

# L1 FSATT S = 0 Frontier
step1m <- makeFrontier(treatment="treated", dataset=LL, drop=c("re78", "treated"), QOI = 'FSATT', metric = 'L1', S = 0)
## calculate ATE using default lm()
step2m <- frontierEst(step1m,dataset=mydataset, myform=myform, treatment=mytreatment, drop=mydrop)
frontierPlot(step1m, mydataset, step2m, drop=mydrop)
mdat <- generateDataset(step1m, mydataset, number.dropped=100)
summary(lm(re78~treated,mdat,weights=w))



# NIELSEN ILLUSTRATION
mydataset <- read.dta("./data/cwdata.dta",convert.factors=F)
mydataset <- mydataset[mydataset$inmysample==1,]
mydataset$coldwar <- NULL
## fix underscores
names(mydataset) <- tolower(gsub("_","",names(mydataset),fixed=T))

myform <- prio~aidshock11+aidshock11pos+lptsavefilled+lassassinbanks+lriotsbanks+lstrikesbanks+ldemonstrationsbanks+linfantmort+lnciv+lpartautocracy+lpartdemocracy+lfactionaldemoc+lfulldemocracy+llnrgdpc+llnpopulation+loil+linstab+ethfrac+relfrac+ncontig+logmtn+coldwar+spline1+spline2+spline3

## keep only these variables
mvars <- c("lptsavefilled","lassassinbanks",
           "lriotsbanks","lstrikesbanks","ldemonstrationsbanks","linfantmort",
           "lnciv","lpartautocracy","lpartdemocracy","lfactionaldemoc",
           "lfulldemocracy","llnrgdpc","llnpopulation","loil","linstab",
           "ethfrac","relfrac","ncontig","logmtn","coldwar","spline1",
           "spline2","spline3")

mytreatment <- "aidshock11"
outcome <- "prio"
leavin <- c("year", "countryname","aidshock11pos","countrynum")
mydataset <- mydataset[,c(mvars,mytreatment,outcome,leavin)]
mydrop <- colnames(mydataset)[!(colnames(mydataset) %in% c(mvars))]

estCall <- function(dataset, weights){
    myform <- (prio ~ aidshock11 + aidshock11pos + lptsavefilled
              + lassassinbanks + lriotsbanks + lstrikesbanks
              + ldemonstrationsbanks + linfantmort + lnciv + lpartautocracy
              + lpartdemocracy + lfactionaldemoc + lfulldemocracy
              + llnrgdpc + llnpopulation + loil + linstab + ethfrac
              + relfrac + ncontig + logmtn + coldwar + spline1 + spline2 + spline3)
    m2<-glm(formula = myform, data = dataset, weights = weights, family = "binomial")
    return(list(effect=summary(m2)$coefficients[2,1], se=summary(m2)$coefficients[2,2]))
}



# FRONTIERS
# Mahalanobis FSATT Frontier
step1m <- makeFrontier(treatment=mytreatment, dataset=mydataset, drop=mydrop, QOI = 'FSATT', metric = 'Mahal', S = 0)
## calculate ATE using default lm()
step2m <- frontierEst(step1m,dataset=mydataset, treatment=mytreatment, drop=mydrop, estCall = estCall)

pdf('NielsenMahalanobisFSATTfrontier.pdf')
plotFrontier(step1m, mydataset, step2m, drop=mydrop)
dev.off()

pdf('NielsenMahalanobisFSATTEffects.pdf')
plotEfects(step1m, mydataset, step2m, drop=mydrop)
dev.off()

pdf('NielsenMahalanobisFSATTMeans.pdf')
plotMeans(step1m, mydataset, step2m, drop=mydrop)
dev.off()



mdat <- generateDataset(step1m, mydataset, number.dropped=100)
summary(lm(re78~treated,mdat,weights=w))

# COMMENTED OUT BECAUSE WE'RE USING THE NIELSEN EXAMPLE FOR THE TIME BEING
## estCall <- function(dataset, weights){
##     myform <- (prio ~ aidshock11 + aidshock11pos + lptsavefilled
##               + lassassinbanks + lriotsbanks + lstrikesbanks
##               + ldemonstrationsbanks + linfantmort + lnciv + lpartautocracy
##               + lpartdemocracy + lfactionaldemoc + lfulldemocracy
##               + llnrgdpc + llnpopulation + loil + linstab + ethfrac
##               + relfrac + ncontig + logmtn + coldwar + spline1 + spline2 + spline3)
##     m2<-glm(formula = myform, data = dataset, family = "binomial")
##     return(list(effect=summary(m2)$coefficients[2,1], se=summary(m2)$coefficients[2,2]))
## }


## # L1 SATT Frontier
## step1m <- makeFrontier(treatment=mytreatment, dataset=mydataset, drop=mydrop, QOI = 'SATT', metric = 'L1', S = 1)
## ## calculate ATE using default lm()
## step2m <- frontierEst(step1m,dataset=mydataset, treatment=mytreatment, drop=mydrop, estCall = estCall)
## frontierPlot(step1m, mydataset, step2m, drop=mydrop)
## mdat <- generateDataset(step1m, mydataset, number.dropped=100)

## # L1 FSATT S = 0 Frontier
## step1m <- makeFrontier(treatment=mytreatment, dataset=mydataset, drop=mydrop, QOI = 'FSATT', metric = 'L1', S = 0)
## ## calculate ATE using default lm()
## step2m <- frontierEst(step1m,dataset=mydataset, treatment=mytreatment, drop=mydrop, estCall = estCall)
## frontierPlot(step1m, mydataset, step2m, drop=mydrop)
## mdat <- generateDataset(step1m, mydataset, number.dropped=100)
## summary(lm(re78~treated,mdat,weights=w))
