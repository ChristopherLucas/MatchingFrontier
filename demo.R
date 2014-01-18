#Using the Lalonde data from the CEM package, the following examples demonstrate how to 
# generate each of the three frontiers we've implemented. 
library(directlabels)
library(finalFrontier)
library(foreign)
library(sem)
library(stargazer)
require(ggplot2)
require(reshape2)

# ########################
# FUNCTIONS NOT IN PACKAGE
# ########################

# FUNCTION ADD POINT TO IMBALANCE PL
frontierEstBig <- function(frontierObject, dataset, myform=NULL, treatment=NULL, estCall=NULL, drop=NULL){

  ## L1
  if(frontierObject$metric=="L1"){
    # Causal Effects
    effectholder <- c()
    seholder <- c() 

    cat("Calculating estimates along the frontier\n")
    pb <- txtProgressBar(min=1,max=length(frontierObject$balance),initial = 1, style = 3)

    interval <- length(frontierObject$drops) / 5000
    for(i in seq(1, length(frontierObject$drops), interval)){
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


plotEffects1 <- function(frontierObject, dataset, frontierEstObject=NULL, zoom = NULL, drop=NULL){
  if(frontierObject$metric == 'L1'){   
    if(!is.null(frontierEstObject)){
      q <- frontierEstObject
      eb <- aes(ymax = mean + sd * 1.96, ymin = mean - sd * 1.96)
      
      p2 <- ggplot(data = q, aes(x = x, y = mean)) + 
        geom_line(size = 1) + geom_point() +
        geom_ribbon(eb, alpha = 0.5) +
        xlab("Number of Observations Pruned") +
        ylab("Effect Size and CIs")
    }
  }
  if(frontierObject$metric=="Mahal" | frontierObject$metric=="Mahalj2k" | frontierObject$metric=="L1w"){
    
    starting.index <- 1
    ending.index <- length(frontierObject$balance)
    if(!is.null(zoom)){
      ## I made a warning for zoom
      if(zoom[1]<0 | tail(zoom, 1) > length(frontierObject$drops)){stop(paste("zoom must be between 0 and ",length(frontierObject$drops)," for this data  set.",sep=""))}
      ## get the index corresponding with the requested number of obs to remove for the start of the zoom
      starting.index <- which(abs((frontierObject$samplesize[1] - frontierObject$samplesize)-zoom[1]) == min(abs((frontierObject$samplesize[1] -  frontierObject$samplesize)-zoom[1])))[1]
      ## get the index corresponding with the requested number of obs to remove for the end of the zoom
      ending.index <- tail( which(abs((frontierObject$samplesize[1] - frontierObject$samplesize)-tail(zoom, 1)) == min(abs((frontierObject$samplesize[1]  - frontierObject$samplesize)-tail(zoom, 1)))), 1)
    }
      # Causal Effects
    if(!is.null(frontierEstObject)){
      q <- frontierEstObject[starting.index:ending.index,]
      eb <- aes(ymax = mean + sd * 1.96, ymin = mean - sd * 1.96)
      
      p2 <- ggplot(data = q, aes(x = x, y = mean)) + 
        geom_line(size = 1) + geom_point() +
        geom_ribbon(eb, alpha = 0.5) +
        xlab("Number of Observations Dropped") +
        ylab("Effect Size and SEs")
    }
  }
  return(p2)
}


plotFrontier1 <- function(frontierObject, dataset, frontierEstObject=NULL,
                               zoom = NULL, drop=NULL){
  require(ggplot2)
  if(frontierObject$metric == 'L1'){   
    starting.index <- 1
    ending.index <- length(frontierObject$drops)
    if(!is.null(zoom)){
      starting.index <- zoom[1]
      ending.index <- tail(zoom, 1)
    }
    
    # Frontier
    df <- data.frame(x = frontierEstObject$x, y = frontierObject$balance[frontierEstObject$x])

    p1 <- ggplot(df, aes(x=x, y=y)) +
      geom_line() +
      xlab("Number of Observations Pruned") +
      ylab("L1") +
      ylim(-.1, 1)
    p1 <- p1 + + theme(legend.position="right")
  }

  if(frontierObject$metric=="Mahal" | frontierObject$metric=="Mahalj2k" | frontierObject$metric=="L1w"){
    
    starting.index <- 1
    ending.index <- length(frontierObject$balance)
    if(!is.null(zoom)){
      ## I made a warning for zoom
      if(zoom[1]<0 | tail(zoom, 1) > length(frontierObject$drops)){stop(paste("zoom must be between 0 and ",length(frontierObject$drops)," for this data  set.",sep=""))}
    ## get the index corresponding with the requested number of obs to remove for the start of the zoom
      starting.index <- which(abs((frontierObject$samplesize[1] - frontierObject$samplesize)-zoom[1]) == min(abs((frontierObject$samplesize[1] -  frontierObject$samplesize)-zoom[1])))[1]
      ## get the index corresponding with the requested number of obs to remove for the end of the zoom
      ending.index <- tail( which(abs((frontierObject$samplesize[1] - frontierObject$samplesize)-tail(zoom, 1)) == min(abs((frontierObject$samplesize[1]  - frontierObject$samplesize)-tail(zoom, 1)))), 1)
    }
  
  # Frontier
  #df <- data.frame(x = seq(starting.index, ending.index), y = frontierObject$balance[starting.index:ending.index])
  normal.df <- data.frame(x = nrow(dataset)-frontierObject$samplesize[starting.index:ending.index], 
                     y = frontierObject$balance[starting.index:ending.index])
  points.df <- data.frame(x = point.x, y = point.y)
  df <- rbind(normal.df, points.df)
  df$dataset <- c(rep("A", nrow(normal.df)), rep("B", nrow(points.df)))
    
    p1 <- ggplot(df, aes(x=x, y=y, col = dataset)) +
      geom_line() + geom_point(colour = 'black') +
      xlab("Number of Observations Pruned") +
      ylab("Average Mahalanobis Imbalance") +
      theme(legend.position="none") +
      ylim(-1, 12)
    p1 <- p1 + geom_text(data = NULL, x = point.x - 100, y = point.y + .5, label = point.label, colour = 'black')
  }
  return(p1)
}

# LALONDE ILLUSTRATION

# S=1 is fixed ratio matching
# Label stuff we'll use for each of the frontiers
nsw <- read.dta('./data/nsw_dw.dta')
step1m <- makeFrontier(treatment=mytreatment, dataset=nsw, drop=mydrop,
                       QOI = 'SATT', metric = 'L1', S = 1)


psid <- read.dta('./data/psid_controls.dta')
cps <- read.dta('./data/cps_controls.dta')

mydataset <- rbind(nsw, psid, cps)

mytreatment <- "treat"

mydrop <- c("treat","re78", "data_id")
myform <- as.formula(re78 ~ treat + age + education + black + married +
                     nodegree + hispanic + re74 + re75)

## # Mahalanobis FSATT Frontier
## step1m <- makeFrontier(treatment="treated", dataset=LL, drop=c("re78", "treated"), QOI = 'FSATT', metric = 'Mahal', S = 0)
## ## calculate ATE using default lm()
## step2m <- frontierEst(step1m,dataset=mydataset, myform=myform, treatment=mytreatment, drop=mydrop)
## frontierMultiPlot(step1m, mydataset, step2m, drop=mydrop)
## mdat <- generateDataset(step1m, mydataset, number.dropped=100)
## summary(lm(re78~treated,mdat,weights=w))

# L1 SATT Frontier
step1m <- makeFrontier(treatment=mytreatment, dataset=mydataset, drop=mydrop,
                       QOI = 'SATT', metric = 'L1', S = 1)
load('./Workspaces/LalondeFrontier.RData')

## calculate ATE using default lm()
step2m <- frontierEst(step1m,dataset=mydataset, myform=myform, treatment=mytreatment, drop=mydrop)

## pdf('LalondeL1SATTFrontier.pdf')
## plotFrontier1(step1m, mydataset, step2m, drop=mydrop)
## dev.off()

## pdf('LalondeL1SATTEffects.pdf')
## plotEffects1(step1m, mydataset, step2m, drop=mydrop)
## dev.off()

colnames(mydataset) <- c('data_id', 'treat', 'Age', 'Education', 'Black', 'Hispanic', 'Married', 'No College Degree', 'Income in 74', 'Income in 75', 're78')

test <- plotMeans1(step1m, mydataset, step2m, drop=mydrop, treatment=mytreatment)

angled.firstpoints <- list("first.bumpup",rot = 0, hjust = .9, vjust = -0.7)
pdf('LalondeL1SATTMeans.pdf')
direct.label(test, angled.firstpoints)
dev.off()

# Figure out why the results are so different
range01 <- function(x){
  if(min(x) == max(x)){
    x <- rep(0, length(x))
    return(x)
  }
  (x-min(x))/(max(x)-min(x))
}

### NSW
nsw <- nsw[order(nsw$treat),]
nsw$Condition <- nsw$treat
nsw$treat <- NULL
nsw$data_id <- NULL
for(col in colnames(nsw)){
  nsw[,colnames(nsw) == col] <- range01(nsw[,colnames(nsw) == col])
}

experiment.controls <- nsw[nsw$treat != 1,]
all.controls <- mydataset[mydataset$treat != 1,]

nsw$Condition <- c('Control', 'Treated')[nsw$Condition + 1]
nsw$ID <- 1:nrow(nsw)
nsw_m <- melt(nsw, id.vars = c('Condition', 'ID'))

ggplot(nsw_m) + 
  geom_line(aes(x = variable, y = value, group = ID, color = Condition),
            position=position_jitter(w=.05, h=.05), alpha = .4) +
  ylab("Standardized Value") +
  xlab("Variable Name")

# MYDATASET    
mydataset <- mydataset[order(mydataset$treat),]
mydataset$Condition <- mydataset$treat
mydataset$treat <- NULL
mydataset$data_id <- NULL
for(col in colnames(mydataset)){
  mydataset[,colnames(mydataset) == col] <- range01(mydataset[,colnames(mydataset) == col])
}

mydataset$Condition <- c('Control', 'Treated')[mydataset$Condition + 1]
mydataset$ID <- 1:nrow(mydataset) 

pdf('parallel_plot_for_everything.pdf')
mydata_m <- melt(mydataset, id.vars = c('Condition', 'ID'))
ggplot(mydata_m) + 
  geom_line(aes(x = variable, y = value, group = ID, color = Condition,
                order = sample(Condition)),
            position=position_jitter(w=.05, h=.05), alpha = .15) + 
  ylab("Standardized Value") +
  xlab("Variable Name")
dev.off()

# Balanced dat
keep <- !(1:nrow(mydataset) %in% step1m$drops)
balance.dat <- mydataset[keep,]
balance.dat <- balance.dat[order(balance.dat$treat),]
balance.dat$Condition <- balance.dat$treat
balance.dat$treat <- NULL
balance.dat$data_id <- NULL
for(col in colnames(balance.dat)){
  balance.dat[,colnames(balance.dat) == col] <- range01(balance.dat[,colnames(balance.dat) == col])
}

balance.dat$Condition <- c('Control', 'Treated')[balance.dat$Condition + 1]
balance.dat$ID <- 1:nrow(balance.dat) 

mydata_balanced <- melt(balance.dat, id.vars = c('Condition', 'ID'))

pdf('parallel_plot_for_balanced_dat.pdf')
ggplot(mydata_balanced) + 
  geom_line(aes(x = variable, y = value, group = ID, color = Condition,
                order = sample(Condition)),
            position=position_jitter(w=.05, h=.05), alpha = .15) + 
  ylab("Standardized Value") +
  xlab("Variable Name")
dev.off()


## # L1 FSATT S = 0 Frontier
## step1m <- makeFrontier(treatment="treated", dataset=LL, drop=c("re78", "treated"), QOI = 'FSATT', metric = 'L1', S = 0)
## ## calculate ATE using default lm()
## step2m <- frontierEst(step1m,dataset=mydataset, myform=myform, treatment=mytreatment, drop=mydrop)
## frontierMultiPlot(step1m, mydataset, step2m, drop=mydrop)
## mdat <- generateDataset(step1m, mydataset, number.dropped=100)
## summary(lm(re78~treated,mdat,weights=w))



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

## estCall <- function(dataset, weights){
##     myform <- (prio ~ aidshock11 + aidshock11pos + lptsavefilled
##               + lassassinbanks + lriotsbanks + lstrikesbanks
##               + ldemonstrationsbanks + linfantmort + lnciv + lpartautocracy
##               + lpartdemocracy + lfactionaldemoc + lfulldemocracy
##               + llnrgdpc + llnpopulation + loil + linstab + ethfrac
##               + relfrac + ncontig + logmtn + coldwar + spline1 + spline2 + spline3)
##     m2<-glm(formula = myform, data = dataset, weights = weights, family = "binomial")
##     return(list(effect=summary(m2)$coefficients[2,1], se=summary(m2)$coefficients[2,2]))
## }

## # FRONTIERS
## # Mahalanobis FSATT Frontier
## step1m <- makeFrontier(treatment=mytreatment, dataset=mydataset, drop=mydrop, QOI = 'FSATT', metric = 'Mahal', S = 0)
## ## calculate ATE using default lm()
## step2m <- frontierEst(step1m,dataset=mydataset, treatment=mytreatment, drop=mydrop, estCall = estCall)
pscore.matched.dat <- read.csv('psmatchdat.csv')
mydrop.pscore <- c(mydrop[mydrop %in% colnames(pscore.matched.dat)], colnames(pscore.matched.dat)[!(colnames(pscore.matched.dat) %in% colnames(mydataset))])
pscore.front <- makeFrontier(treatment = mytreatment, dataset = pscore.matched.dat, drop = mydrop.pscore, QOI = 'FSATT', metric = 'Mahal', S = 0)

load('Nielsen_Workspace.RData')

pdf('NielsenMahalanobisFSATTfrontier.pdf')
plotFrontierWPoint(step1m, mydataset, step2m, drop=mydrop, point.x = (nrow(mydataset) - pscore.front$samplesize[1]),
                   point.y = pscore.front$balance[1], point.label = 'AMI in Published Analysis')
dev.off()

#pdf('NielsenMahalanobisFSATTEffects.pdf')
#plotEffects1(step1m, mydataset, step2m, drop=mydrop, title = 'Nielsen Effect Estimates')
#dev.off()

pdf('NielsenMahalanobisFSATTMeans.pdf')
plotMeans(step1m, mydataset, step2m, drop=mydrop)
dev.off()



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



mydataset <- read.dta('./data/ArceneauxGerberGreen_PA_2006_IA_MI_merge040504.dta')
keep <- c('vote02', 'contact', 'state', 'comp_ia', 'comp_mi', 'persons', 'age', 'female2', 'newreg', 'vote00', 'vote98', 'fem_miss', 'treat_real')
mydataset <- mydataset[,colnames(mydataset) %in% keep]

iv.2 <- tsls(vote02 ~ contact + state + comp_ia + comp_mi + persons + age + female2 + newreg + vote00 + vote98 + fem_miss, ~ treat_real + state + comp_mi + comp_ia + persons + age + female2 + newreg + vote00 + vote98 + fem_miss, data=mydataset)
summary(iv.2)

ols.2 <- lm(vote02 ~ contact + state + comp_ia + comp_mi + persons + age + female2 + newreg + vote00 + vote98 + fem_miss, data=mydataset)
summary(ols.2)

mytreatment <- "contact"

mydrop <- colnames(mydataset)[!(colnames(mydataset) %in% c('state', 'comp_ia', 'comp_mi', 'persons', 'age', 'female2', 'newreg', 'vote00', 'vote98', 'fem_miss'))]
myform <- as.formula(vote02 ~ contact + state + comp_mi + comp_ia + persons + age + female2 + newreg + vote00 + vote98 + fem_miss)

## # Mahalanobis FSATT Frontier
## step1m <- makeFrontier(treatment="treated", dataset=LL, drop=c("re78", "treated"), QOI = 'FSATT', metric = 'Mahal', S = 0)
## ## calculate ATE using default lm()
## step2m <- frontierEst(step1m,dataset=mydataset, myform=myform, treatment=mytreatment, drop=mydrop)
## frontierMultiPlot(step1m, mydataset, step2m, drop=mydrop)
## mdat <- generateDataset(step1m, mydataset, number.dropped=100)
## summary(lm(re78~treated,mdat,weights=w))
mydataset <- na.omit(mydataset)
# L1 SATT Frontier

load('./Workspaces/GOTV_frontier.RData')
#step1m <- makeFrontier(treatment=mytreatment, dataset=mydataset, drop=mydrop,
#                       QOI = 'SATT', metric = 'L1', S = 1)

load('./Workspaces/GOTV_estimates.RData')

## calculate ATE using default lm()
#step2m <- frontierEstBig(step1m,dataset=mydataset, myform=myform, treatment=mytreatment, drop=mydrop)

pdf('AGGL1SATTFrontier.pdf')
plotFrontier1(step1m, mydataset, step2m, drop=mydrop)
dev.off()

pdf('AGGL1SATTEffects.pdf')
plotEffects1(step1m, mydataset, step2m, drop=mydrop)
dev.off()

colnames(mydataset) <- c('Household Size', 'Vote in 00', 'Vote in 98', 'Newly Registered Voter', 'Age',
                         'Vote in 02', 'Contact', 'Real Treatment', 'State', 'Competitive MI District', 'Competitive IA District',
                         'Missing Gender', 'Female')
mydrop <- c('Vote in 02', 'Contact', 'Real Treatment')
mydataset$State <- mydataset$State == 'Iowa'


######
#####
####
###
##
#

plotMeans1 <- function(frontierObject, dataset, frontierEstObject=NULL, zoom = NULL, drop=NULL, treatment){
  require(ggplot2)
  require(reshape2)
  if(frontierObject$metric == 'L1'){   
    covs <- colnames(dataset)[!(colnames(dataset) %in% drop)]

    for(col in covs){
      dataset[,colnames(dataset) == col] <- range01(dataset[,colnames(dataset) == col])
    }
    
    covs.mat <- matrix(nrow = 0, ncol = length(covs), byrow = FALSE)
    colnames(covs.mat) <- covs
    
    for(i in frontierEstObject$x[seq(1, 5000, 10)]){
      iter.dat <- dataset[!(rownames(dataset) %in% frontierObject$drops[1:i]),]
      treated.inds <- which(iter.dat[[treatment]] == 1)
      control.inds <- which(iter.dat[[treatment]] == 0)
      new.row <- c()
      print(i/tail(frontierEstObject$x, 1))
      for(col in colnames(covs.mat)){
        new.row <- c(new.row, mean(iter.dat[treated.inds,col]) - mean(iter.dat[control.inds,col]))
      }
      covs.mat <- rbind(covs.mat, new.row)
    }
    rownames(covs.mat) <- seq(nrow(covs.mat))
    
    data.long <- melt(covs.mat)
    data.long$x <- rep(frontierEstObject$x[seq(1, 5000, 10)], nrow(data.long) / length(frontierEstObject$x[seq(1, 5000, 10)]))
    return(data.long)
    p3 <- ggplot(data=data.long,
                 aes(x=x, y=value, colour=Var2)) +
                 geom_line() +
                 xlab("Number of Observations Pruned") +
                 ylab("Ratio of mean values for treated units against controls") +
                 opts(legend.position="none") +
                 xlim(-4000, 18000)
                 theme(legend.title=element_blank())
  }
  return(p3)
}

means.test <- plotMeans1(step1m, mydataset, step2m, drop=mydrop, treatment = 'Contact')

angled.firstpoints <- list("first.bumpup",rot = 0, hjust = 1, vjust = -0.7)
direct.label(means.test, angled.firstpoints)

pdf('AGGL1SATTMeans.pdf')
means.test
dev.off()

