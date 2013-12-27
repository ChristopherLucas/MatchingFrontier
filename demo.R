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
frontierPlot(step1m, mydataset, step2m, drop=mydrop)
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
summary(lm(re78~treated,mdat,weights=w))</code>


# NIELSEN ILLUSTRATION
 
# Label stuff we'll use for each of the frontiers    
mydataset <- read.dta("./data/cwdata.dta", convert.underscore=T)
mydataset <- na.omit(mydataset)
mytreatment <- "aidshock11oecd"
myform <- (prio ~ aidshock11oecd + lPTSave.filled + lassassinbanks + lriotsbanks + lstrikesbanks
           + ldemonstrationsbanks + linfantmort + lnciv 
           + lpartautocracy + lpartdemocracy + lfactionaldemoc
           + lfulldemocracy 
           + lln.rgdpc + lln.population + loil + linstab 
           + ethfrac + relfrac + ncontig + logmtn + ColdWar
           + .spline1 + .spline2 + .spline3 
           + year  ## year is in here so that we have it later
           )
match.on <- c('prio', 'aidshock11oecd', 'lPTSave.filled', 'lassassinbanks', 'lriotsbanks', 'lstrikesbanks',
           'ldemonstrationsbanks', 'linfantmort', 'lnciv', 
           'lpartautocracy', 'lpartdemocracy', 'lfactionaldemoc',
           'lfulldemocracy', 'lln.rgdpc', 'lln.population', 'loil', 'linstab', 
           'ethfrac', 'relfrac', 'ncontig', 'logmtn', 'ColdWar',
           '.spline1', '.spline2', '.spline3', 'year')
mydrop <- colnames(mydataset)[!(colnames(mydataset) %in% match.on)]

estCall <- function(dataset, weights){
    form3 <- (prio ~ aidshock11oecd + lPTSave.filled + lassassinbanks + lriotsbanks + lstrikesbanks
             + ldemonstrationsbanks + linfantmort + lnciv 
             + lpartautocracy + lpartdemocracy + lfactionaldemoc
             + lfulldemocracy 
             + lln.rgdpc + lln.population + loil + linstab 
             + ethfrac + relfrac + ncontig + logmtn + ColdWar
             + .spline1 + .spline2 + .spline3 
             + year)    
    m2<-glm(formula = form3, data = dataset, weights = weights, family = "binomial")
    return(list(effect=summary(m2)$coefficients[2,1], se=summary(m2)$coefficients[2,2]))
}



# FRONTIERS
# Mahalanobis FSATT Frontier
step1m <- makeFrontier(treatment=mytreatment, dataset=mydataset, drop=mydrop, QOI = 'FSATT', metric = 'Mahal', S = 0)
## calculate ATE using default lm()
step2m <- frontierEst(step1m,dataset=mydataset, treatment=mytreatment, drop=mydrop, estCall = estCall)
frontierPlot(step1m, mydataset, step2m, drop=mydrop)
mdat <- generateDataset(step1m, mydataset, number.dropped=100)
summary(lm(re78~treated,mdat,weights=w))

# L1 SATT Frontier
step1m <- makeFrontier(treatment=mytreatment, dataset=mydataset, drop=mydrop, QOI = 'SATT', metric = 'L1', S = 1)
## calculate ATE using default lm()
step2m <- frontierEst(step1m,dataset=mydataset, treatment=mytreatment, drop=mydrop, estCall = estCall)
frontierPlot(step1m, mydataset, step2m, drop=mydrop)
mdat <- generateDataset(step1m, mydataset, number.dropped=100)
summary(lm(re78~treated,mdat))

# L1 FSATT S = 0 Frontier
step1m <- makeFrontier(treatment=mytreatment, dataset=mydataset, drop=mydrop, QOI = 'FSATT', metric = 'L1', S = 0)
## calculate ATE using default lm()
step2m <- frontierEst(step1m,dataset=mydataset, treatment=mytreatment, drop=mydrop, estCall = estCall)
frontierPlot(step1m, mydataset, step2m, drop=mydrop)
mdat <- generateDataset(step1m, mydataset, number.dropped=100)
summary(lm(re78~treated,mdat,weights=w))</code>
