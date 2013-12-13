#Using the Lalonde data from the CEM package, the following examples demonstrate how to 
# generate each of the three frontiers we've implemented. 

library(cem)
data(LL)
detach(package:cem)

# S=1 is fixed ratio matching
# Label stuff we'll use for each of the frontiers
mytreatment <- "treated"
mydataset <- LL
mydrop <- c("treated","re78")
myform <- as.formula(re78 ~ treated +age + education + black + married + nodegree
                     + re74 + re75 + hispanic + u74 + u75)
mypsform <- as.formula(treated~age+education+black+married+nodegree+re74+re75+hispanic+u74+u75)
</code>

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
