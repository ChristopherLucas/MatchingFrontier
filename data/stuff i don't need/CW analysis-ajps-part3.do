/* Analysis for Aid Shocks

  This file does extra models with alternative sources of aid data

  Note that while most of this analysis is in Stata 9.2,
  some of it is in R 2.10.1.  The R code is in with the stata
  code, usually indicated with something like:
	********************
	** R code for blah blah
	*******************
  Just copy and paste it in to R and it should run.

  The files necessary to run this analysis are all in the folder ~\data.
  Set your directory to the main folder with this script in it.

	You'll want to enter the following directly from the command line: 
					set mem 500M
					set more off
					cd "your/directory/here"
					** Or whatever your directory is.
 	from the command line, so that the file can run all the way through.
*/

*********************************************
** Make a folder to hold some of the output
*********************************************

capture mkdir output
capture mkdir crap


****************************
** compare OECD and PLAID aid shocks
****************************
use cwdata, clear

sum year if aidshock11==1 & inmysample==1
sum year if aidshock11oecd1==1 & inmysample==1
sum year if aidshock11oecd2==1 & inmysample==1
sum year if aidshock11==1 & aidshock11oecd1==1 & inmysample==1

gen diffaidshock = 1 if (aidshock11==1 & aidshock11oecd2==0 & inmysample==1) | (aidshock11==0 & aidshock11oecd2==1 & inmysample==1)
  ** the number of non-overlapping aid shocks
sum diffaidshock if diffaidshock==1

  ** This number is surprisingly high!

*edit countryname year aidshock11 aidshock11oecd if diffaidshock==1

  ** compare OECD aid to plaid aid graphically

## in R
## Set working directory to the folder with the data

## Reading in the data
library(foreign)
library(Zelig)
data<-read.dta("cwdata.dta", convert.underscore=T)

data <- data[data$year<2008,]

holder <- unique(data$countryname)
clist <- c("Cambodia (Kampuchea)","Tajikistan","Cape Verde","Benin")

currentpath <- getwd()
outpath <- paste(currentpath,"/output",sep="")
setwd(outpath)

jpeg(filename = "Plaid vs OECD.jpg", width = 9, height = 6.8, res=200,
     units = "in")
par(mfrow=c(2,2))
for(i in 1:length(clist)){
  c <- clist[i]
  subdat <- data[data$countryname==c,]
  plot(x=subdat$year, y=(subdat$aid.usd.2000)/1000000, type="l", main=c,
       xlab="Year", ylab="Aid (in Millions of 2000 USD)")
  lines(x=subdat$year, y=(subdat$aid.usd.2000oecd)/1000000,lty=2)
  legend("topleft", legend=c("AidData","OECD"), lty=c(1,2),
         bty="n")
}
dev.off()

## reset the directory
setwd(currentpath)

################################




  ** compare commitment shocks to disbursment shocks
sum year if aidshock11==1 & inmysample==1
sum year if aidshock11disb1==1 & inmysample==1
sum year if aidshock11disb2==1 & inmysample==1
sum year if aidshock11==1 & aidshock11disb1==1 & inmysample==1

gen diffaidshock2 = 1 if (aidshock11==1 & aidshock11disb1==0 & inmysample==1) | (aidshock11==0 & aidshock11disb1==1 & inmysample==1)
  ** the number of non-overlapping aid shocks
sum diffaidshock2 if diffaidshock2==1

*edit countryname year aidshock11 aidshock11oecd if diffaidshock==1

  ** compare commitments to disbursements graphically

## in R

## Set working directory to the folder with the data

## Reading in the data
library(foreign)
library(Zelig)
data<-read.dta("cwdata.dta", convert.underscore=T)

data <- data[data$year<2008,]

holder <- unique(data$countryname)
clist <- c("Rwanda","Tajikistan","Cape Verde","Argentina", "Uganda",
           "Madagascar (Malagasy)","Laos","Myanmar (Burma)")

par(mfrow=c(2,4))
for(i in 1:length(clist)){
  c <- clist[i]
  subdat <- data[data$countryname==c,]
  plot(x=subdat$year, y=(subdat$aid.usd.2000)/1000000, type="l", main=c,
       xlab="Year", ylab="Aid (in Millions of 2000 USD)")
  lines(x=subdat$year, y=(subdat$aid.usd.2000disb)/1000000,lty=2)
  legend("topleft", legend=c("Commitments","Disbursements"), lty=c(1,2),
         bty="n")
}
###############################


*********************************************************
** Redo key models with OECD aid and DISBURSEMENTS
*********************************************************

****************************
** Main model
****************************

  ** This is part of making the table of results
capture erase "output/finalmodelsoecd.txt"
capture erase "output/finalmodelsoecd.rtf"


use cwdata, clear
  ** oecd2 uses the smaidchoecd 15th percentile cutoff
relogit prio aidshock11oecd2 aidshock11posoecd lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, cl(countrynum)
outreg2 using "output/finalmodelsoecd", word append

  ** make a var with only the agreeing aidshocks
gen aidshock11both2 = 1 if aidshock11 ==1 & aidshock11oecd2==1
replace aidshock11both2 = 0 if aidshock11both2!=1
relogit prio aidshock11both2 lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, cl(countrynum)


  ** disbursements
  ** all years
relogit prio aidshock11disb1 lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, cl(countrynum)
relogit prio aidshock11disb2 lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, cl(countrynum)

  ** after 2001
relogit prio aidshock11disb1 lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn         _spline1 _spline2 _spline3 if year>=2001, cl(countrynum)
capture gen samp=e(sample)
tab aidshock11disb1 if samp==1
sum prio if aidshock11disb1 ==1 & samp==1
sum prio if aidshock11disb1 ==0 & samp==1

relogit prio aidshock11disb2 lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn         _spline1 _spline2 _spline3 if year>=2001, cl(countrynum)


****************************
** Fixed/Random effects
****************************

xtlogit prio aidshock11oecd2 aidshock11posoecd lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, fe
outreg2 using "output/finalmodelsoecd", word append

  ** with only the agreeing aidshocks
xtlogit prio aidshock11both2 aidshock11posoecd lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, fe



*************************************************************
** Matching Models
*************************************************************

	********************
	** R code for matching
	********************
** Run the matching procedure by copying this code into R
/*

## Need to get the working directory
## Set the working directory to the location with the data

## Then grab the paths for later
currentpath <- getwd()
crappath <- paste(currentpath,"/crap",sep="")
outpath <- paste(currentpath,"/output",sep="")

## Reading in the data
library(foreign)
library(Zelig)
data<-read.dta("cwdata.dta", convert.underscore=T)

##################################################
## MATCHING

## The model (to create the data for matching)
form3<- (prio ~ aidshock11oecd + lPTSave.filled + lassassinbanks + lriotsbanks + lstrikesbanks
                  + ldemonstrationsbanks + linfantmort + lnciv 
                  + lpartautocracy + lpartdemocracy + lfactionaldemoc
                  + lfulldemocracy 
			+ lln.rgdpc + lln.population + loil + linstab 
                  + ethfrac + relfrac + ncontig + logmtn + ColdWar
			+ .spline1 + .spline2 + .spline3 
                  + year  ## year is in here so that we have it later
                  )

m2<-zelig(form3, data=data, model="relogit")
summary(m2)

nrow(m2$data)  ## should be 2627 -- same as main model

  ## get rid of the observations that aren't in the main model
data <- data[as.numeric(rownames(m2$data)),]
rownames(data) <- row(data)[,1]



## Make a figure of the distribution of aid change with AID SHOCKS in gray
hist(data$smaidchoecd)
plot(density(na.omit(data$smaidchoecd)),type="n", main="Distribution of Changes in Aid", xlim=c(-.05,.05), 
     xlab="Changes in Aid over/GDP", 
     ylab="Density
(Number of observations with this level of Aid Change)")
#abline(v=quantile(na.omit(data$smaidch), probs=.15), lty=2)
#legend(x="topright",c("  15th Percentile","(Aid Shock Cut-off)"), lty=c(1,0),box.lty=0)
legend(x="topright",c("Lowest 15th Percentile","(Coded as Aid Shocks)"), 
       pch=c(15,NA),box.lty=0,col="gray55")
xvals2 <- c(density(na.omit(data$smaidchoecd))$x,rev(density(na.omit(data$smaidchoecd))$x))
yvals2 <- c(density(na.omit(data$smaidchoecd))$y, rep(0,length(density(na.omit(data$smaidchoecd))$y)))
polygon( xvals2,yvals2, col="gray95", lty=0)
cutoff <- quantile(na.omit(data$smaidchoecd), probs=.15)
xvals <- c(density(na.omit(data$smaidchoecd))$x[density(na.omit(data$smaidchoecd))$x<cutoff],rev(density(na.omit(data$smaidchoecd))$x[density(na.omit(data$smaidchoecd))$x<cutoff]))
yvals <- c(density(na.omit(data$smaidchoecd))$y[density(na.omit(data$smaidchoecd))$x<cutoff], rep(0,length(density(na.omit(data$smaidchoecd))$y[density(na.omit(data$smaidchoecd))$x<cutoff])))
polygon( xvals,yvals, col="gray55", lty=0)
lines(density(na.omit(data$smaidchoecd)))


summary(data$smaidchoecd)

  ## we run it again
## The model (to create the data for matching)
form3<- (prio ~ aidshock11oecd + lPTSave.filled + lassassinbanks + lriotsbanks + lstrikesbanks
                  + ldemonstrationsbanks + linfantmort + lnciv 
                  + lpartautocracy + lpartdemocracy + lfactionaldemoc
                  + lfulldemocracy 
			+ lln.rgdpc + lln.population + loil + linstab 
                  + ethfrac + relfrac + ncontig + logmtn + ColdWar
			+ .spline1 + .spline2 + .spline3 
                  + year  ## year is in here so that we have it later
                  )

m2<-zelig(form3, data=data, model="relogit")
summary(m2)



## Matching Procedure
library(MatchIt)

head(m2$model[2:26])
m.dat2 <- cbind(m2$model[2:26],m2$y)

  ## with the additional covariates
match.form3<- (aidshock11oecd ~ lPTSave.filled + lassassinbanks + lriotsbanks + lstrikesbanks
                  + ldemonstrationsbanks + linfantmort + lnciv 
                  + lpartautocracy + lpartdemocracy + lfactionaldemoc
                  + lfulldemocracy 
			+ lln.rgdpc + lln.population + loil + linstab 
                  + ethfrac + relfrac + ncontig + logmtn + ColdWar
			+ .spline1 + .spline2 + .spline3 )

  ## Genetic WITH PITF vars -- takes 50 minutes on my machine
set.seed(1111)
out.m <- matchit(match.form3, data=m.dat2, 
		method="genetic")
alarm()

summary(out.m)

#plot(out.m)

head(match.data(out.m))
out<-cbind(data$countryname[as.numeric(rownames(match.data(out.m)))],
           data$year[as.numeric(rownames(match.data(out.m)))],
          (match.data(out.m)))

## out$cunit is the treatmentid for treated units and the 
## id of the matched treated unit for control units
out$cunit<-as.numeric(rownames(out))
for(i in 1:length(rownames(out.m$match.matrix))){
  unit<-rownames(out.m$match.matrix)[i]
  out$cunit[rownames(out)==unit]<- as.numeric(out.m$match.matrix[unit,1])
}

setwd(crappath)
write.csv(out, file="matched data.csv")
setwd(currentpath)
names(out)
names(data)

## LOVE PLOT FUNCTION ###########################################################
## Make a function to create love plots to assess balance

  ## Example inputs
#myformula <- t ~ age + education + black + hispanic + nodegree + married + re74 + re75
#dataset1 <- lalonde
#dataset2 <- match.data(m.out)

## RED is the first dataset, BLUE is the second

loveplot <- function(myformula, dataset1, dataset2, 
             main="Standardized Means Before and After Matching
RED is the first dataset, BLUE is the second", varnames=NULL) {
   ##  love.plot takes three arguments: (1) a formula where
   ##  treatment ~ covariate1 + covariate 2 ...
   ##  (2) an unmatched dataset, and (3) a matched dataset

   data1 <- model.frame(myformula,data=dataset1)
   data2 <- model.frame(myformula,data=dataset2)

   X1 <- model.matrix(myformula, dataset1)[,-1]
   X2 <- model.matrix(myformula, dataset2)[,-1]

   t1 <- model.extract(data1,"response")
   t2 <- model.extract(data2,"response")

   varnames1 <- colnames(X1)
   varnames2 <- colnames(X2)

   X1.t <- X1[t1==1,]
   X1.c <- X1[t1==0,]

   X2.t <- X2[t2==1,]
   X2.c <- X2[t2==0,]

   dif.means1 <- (apply(X1.t, MAR=2, FUN=mean) - apply(X1.c, MAR=2, FUN=mean))/apply(X1, MAR=2, FUN=sd)
   dif.means2 <- (apply(X2.t, MAR=2, FUN=mean) - apply(X2.c, MAR=2, FUN=mean))/apply(X2, MAR=2, FUN=sd)

     ## make the plot
   par(mar=c(5,9,5,2)+.1)
   plot(x=dif.means1, y = rev(seq(1,length(dif.means1),1)), type="n", 
        main=main,
        xlab = "(mean treated - mean control)/sd treated", ylab="", axes=F,
        xlim=c( min(c(dif.means1,dif.means2)),  max(c(dif.means1,dif.means2)))
        #xlim=c( min(c(dif.means1,dif.means2,-.02)),  max(c(dif.means1,dif.means2,.02)))
        )
   abline(v=0, lty=2)
   for(i in 1:length(dif.means1)){
      abline(h=i, col="gray80")
   }
   points(x=dif.means1, y = rev(seq(1,length(dif.means1),1)), col="red", pch=1)
   points(x=dif.means2, y = rev(seq(1,length(dif.means2),1)), col="blue", pch=1)
   one.is.better <- data.frame(cbind(rev(seq(1,length(dif.means1),1)),
                          abs(dif.means1) < abs(dif.means2),
                          dif.means1))
   points(x=one.is.better[,3][one.is.better[,2]==1],
          y=one.is.better[,1][one.is.better[,2]==1],
          col="red", pch=19)
   two.is.better <- data.frame(cbind(rev(seq(1,length(dif.means1),1)),
                          abs(dif.means1) > abs(dif.means2),
                          dif.means2))
   points(x=two.is.better[,3][two.is.better[,2]==1],
          y=two.is.better[,1][two.is.better[,2]==1],
          col="blue", pch=19) 
   #axis(1, at=rev(seq( round(min(dif.means1),.5)) , round(max(dif.means1),.5),length.out = 5), labels=T) 
   axis(1, at=seq(-100,100,.5), labels=T) 
   if(is.null(varnames)){
     varnames <- colnames(X1)
   }
   axis(2, at=rev(seq(1,length(dif.means1),1)), labels=varnames, las=2)
   
   
     ## add a key so I can remember
   #legend(x="topleft", legend=c("1","2"), bty="n",text.col=c(2,4), horiz=T)
}
## END LOVE PLOT FUNCTION ###################################################

form <- (aidshock11oecd ~ lPTSave.filled + lassassinbanks + lriotsbanks + lstrikesbanks
                  + ldemonstrationsbanks + linfantmort + lnciv 
                  + lpartautocracy + lpartdemocracy + lfactionaldemoc
                  + lfulldemocracy 
			+ lln.rgdpc + lln.population + loil + linstab 
                  + ethfrac + relfrac + ncontig + logmtn + ColdWar)
			#+ .spline1 + .spline2 + .spline3 )

loveplot(form,out,data,
         main="",
         varnames=c("Human Rights Abuse","Assassinations","Riots","Strikes","Demonstrations",
                    "Infant Mortality","Bad Neighborhood","Partial Autocracy",
                    "Partial Democracy","Factional Democracy","Full Democracy","GDP pc",
                    "Population","Oil","Instability","Ethnic Frac.","Religious Frac.",
                    "Noncontiguous","Mountains","Cold War") )

title(expression("Improved Mean Balance: " * phantom("Matched ") *
  "vs. " * phantom("Unmatched               ")),col.main="black")
title(expression(phantom("Improved Mean Balance: ") * "Matched " *
  phantom("vs. ") * phantom("Unmatched               ")),col.main="red")
title(expression(phantom("Improved Mean Balance: ") * phantom("Matched ") *
  phantom("vs. ") * "Unmatched               "),col.main="blue")



  ## propensity score matching
  ## write the data so I can run the model in stata
out1<-cbind(data$countryname[as.numeric(rownames(m.dat2))], m.dat2)
setwd(crappath) 
write.csv(out1, file="mdat2.csv")
setwd(currentpath)

  ** in Stata
insheet using "crap/mdat2.csv", clear
rename  datacountrynameasnumericrownames countryname
egen countrynum = group(countryname)
tsset countrynum year

xtlogit aidshock11oecd lptsavefilled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy llnrgdpc llnpopulation loil linstab ethfrac relfrac ncontig logmtn coldwar spline1 spline2 spline3, re
gen samp = e(sample)
predict pscore if samp==1
sort pscore

save "crap/pscoredat.dta", replace

  ## back in R
setwd(crappath)
library(foreign)
dat <- read.dta("pscoredat.dta")
head(dat)
  ## order them by pscore
dat <- dat[rev(order(dat$pscore)),]

tdat <- dat[dat$aidshock11oecd==1,]
cdat <- dat[dat$aidshock11oecd==0,]
cdattemp <- cdat

matchdat <- c()
for(i in 1:nrow(tdat)){
  tunit <- tdat[i,]
  t <- tunit$pscore
  cpool <- cdattemp$pscore
  cunit <- cdattemp[which(abs((t-cpool))==min(abs(t-cpool))),]
  newmatch <- rbind(tunit, cunit)
  matchdat <- rbind(matchdat,newmatch)
}

nrow(matchdat)

write.csv(matchdat, file="psmatchdat.csv")


## LOVE PLOT FUNCTION ###########################################################
## Make a function to create love plots to assess balance

  ## Example inputs
#myformula <- t ~ age + education + black + hispanic + nodegree + married + re74 + re75
#dataset1 <- lalonde
#dataset2 <- match.data(m.out)

## RED is the first dataset, BLUE is the second

loveplot <- function(myformula, dataset1, dataset2, 
             main="Standardized Means Before and After Matching
RED is the first dataset, BLUE is the second", varnames=NULL) {
   ##  love.plot takes three arguments: (1) a formula where
   ##  treatment ~ covariate1 + covariate 2 ...
   ##  (2) an unmatched dataset, and (3) a matched dataset

   data1 <- model.frame(myformula,data=dataset1)
   data2 <- model.frame(myformula,data=dataset2)

   X1 <- model.matrix(myformula, dataset1)[,-1]
   X2 <- model.matrix(myformula, dataset2)[,-1]

   t1 <- model.extract(data1,"response")
   t2 <- model.extract(data2,"response")

   varnames1 <- colnames(X1)
   varnames2 <- colnames(X2)

   X1.t <- X1[t1==1,]
   X1.c <- X1[t1==0,]

   X2.t <- X2[t2==1,]
   X2.c <- X2[t2==0,]

   dif.means1 <- (apply(X1.t, MAR=2, FUN=mean) - apply(X1.c, MAR=2, FUN=mean))/apply(X1, MAR=2, FUN=sd)
   dif.means2 <- (apply(X2.t, MAR=2, FUN=mean) - apply(X2.c, MAR=2, FUN=mean))/apply(X2, MAR=2, FUN=sd)

     ## make the plot
   par(mar=c(5,9,5,2)+.1)
   plot(x=dif.means1, y = rev(seq(1,length(dif.means1),1)), type="n", 
        main=main,
        xlab = "(mean treated - mean control)/sd treated", ylab="", axes=F,
        xlim=c( min(c(dif.means1,dif.means2)),  max(c(dif.means1,dif.means2)))
        #xlim=c( min(c(dif.means1,dif.means2,-.02)),  max(c(dif.means1,dif.means2,.02)))
        )
   abline(v=0, lty=2)
   for(i in 1:length(dif.means1)){
      abline(h=i, col="gray80")
   }
   points(x=dif.means1, y = rev(seq(1,length(dif.means1),1)), col="red", pch=1)
   points(x=dif.means2, y = rev(seq(1,length(dif.means2),1)), col="blue", pch=1)
   one.is.better <- data.frame(cbind(rev(seq(1,length(dif.means1),1)),
                          abs(dif.means1) < abs(dif.means2),
                          dif.means1))
   points(x=one.is.better[,3][one.is.better[,2]==1],
          y=one.is.better[,1][one.is.better[,2]==1],
          col="red", pch=19)
   two.is.better <- data.frame(cbind(rev(seq(1,length(dif.means1),1)),
                          abs(dif.means1) > abs(dif.means2),
                          dif.means2))
   points(x=two.is.better[,3][two.is.better[,2]==1],
          y=two.is.better[,1][two.is.better[,2]==1],
          col="blue", pch=19) 
   #axis(1, at=rev(seq( round(min(dif.means1),.5)) , round(max(dif.means1),.5),length.out = 5), labels=T) 
   axis(1, at=seq(-100,100,.5), labels=T) 
   if(is.null(varnames)){
     varnames <- colnames(X1)
   }
   axis(2, at=rev(seq(1,length(dif.means1),1)), labels=varnames, las=2)
   
   
     ## add a key so I can remember
   #legend(x="topleft", legend=c("1","2"), bty="n",text.col=c(2,4), horiz=T)
}
## END LOVE PLOT FUNCTION ###################################################

form <- (aidshock11oecd ~ lPTSave.filled + lassassinbanks + lriotsbanks + lstrikesbanks
                  + ldemonstrationsbanks + linfantmort + lnciv 
                  + lpartautocracy + lpartdemocracy + lfactionaldemoc
                  + lfulldemocracy 
			+ lln.rgdpc + lln.population + loil + linstab 
                  + ethfrac + relfrac + ncontig + logmtn + ColdWar)
			#+ .spline1 + .spline2 + .spline3 )

matchdat2 <- matchdat
colnames(matchdat2) <- gsub("lptsavefilled", "lPTSave.filled", colnames(matchdat2),fixed=T)
colnames(matchdat2) <- gsub("llnrgdpc", "lln.rgdpc", colnames(matchdat2),fixed=T)
colnames(matchdat2) <- gsub("llnpopulation", "lln.population", colnames(matchdat2),fixed=T)
colnames(matchdat2) <- gsub("coldwar", "ColdWar", colnames(matchdat2),fixed=T)
colnames(matchdat2) <- gsub("spline1", ".spline1", colnames(matchdat2),fixed=T)
colnames(matchdat2) <- gsub("spline2", ".spline2", colnames(matchdat2),fixed=T)
colnames(matchdat2) <- gsub("spline3", ".spline3", colnames(matchdat2),fixed=T)


loveplot(form,matchdat2,data,
         main="",
         varnames=c("Human Rights Abuse","Assassinations","Riots","Strikes","Demonstrations",
                    "Infant Mortality","Bad Neighborhood","Partial Autocracy",
                    "Partial Democracy","Factional Democracy","Full Democracy","GDP pc",
                    "Population","Oil","Instability","Ethnic Frac.","Religious Frac.",
                    "Noncontiguous","Mountains","Cold War") )

title(expression("Improved Mean Balance: " * phantom("Matched ") *
  "vs. " * phantom("Unmatched               ")),col.main="black")
title(expression(phantom("Improved Mean Balance: ") * "Matched " *
  phantom("vs. ") * phantom("Unmatched               ")),col.main="red")
title(expression(phantom("Improved Mean Balance: ") * phantom("Matched ") *
  phantom("vs. ") * "Unmatched               "),col.main="blue")


## reset the working directory
setwd(currentpath)

*/

	*************
	** End R code
	*************

** This code runs the matching analysis
** After running the matching procedure to create a .csv...

  ** Propensity score matching
insheet using "crap\psmatchdat.csv", clear
rename m2y prio
rename lptsavefilled lPTSave_filled
rename llnrgdpc lln_rgdpc
rename llnpopulation lln_population
rename coldwar ColdWar
rename spline1 _spline1
rename spline2 _spline2
rename spline3 _spline3
relogit prio aidshock11oecd lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar  _spline1 _spline2 _spline3, cl(countrynum)
outreg2 using "output/finalmodelsoecd", word append

  ** Genetic Matching
clear
insheet using "crap/matched data.csv"
rename datacountrynameasnumericrownames countryname
rename llnpopulation lln_population
rename llnrgdpc lln_rgdpc
rename coldwar ColdWar
rename spline1 _spline1
rename spline2 _spline2
rename spline3 _spline3
capture rename lptsavefilled lPTSave_filled
capture rename m2y prio
egen countrynum = group(countryname)
tsset countrynum year
  ** doesn't make sense to include aidshocks11pos in the matching
relogit prio aidshock11oecd lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, cl(countrynum)
outreg2 using "output/finalmodelsoecd", word append




*********************************************************
** Redo key models with PLAID + military aid
*********************************************************

use cwdata, clear

****************************
** Main model
****************************

  ** This is part of making the table of results
capture erase "output/finalmodelsmil.txt"
capture erase "output/finalmodelsmil.rtf"


use cwdata, clear
relogit prio aidshock11mil aidshock11posmil lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, cl(countrynum)

outreg2 using "output/finalmodelsmil", word append


****************************
** Fixed/Random effects
****************************

xtlogit prio aidshock11mil aidshock11posmil lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, fe
outreg2 using "output/finalmodelsmil", word append



*************************************************************
** Matching Models
*************************************************************

	********************
	** R code for matching
	********************
** Run the matching procedure by copying this code into R
/*

## Need to get the working directory

## Set the working directory to the location with the data

## Then grab the paths for later
currentpath <- getwd()
crappath <- paste(currentpath,"/crap",sep="")
outpath <- paste(currentpath,"/output",sep="")

## Reading in the data
library(foreign)
library(Zelig)
data<-read.dta("cwdata.dta", convert.underscore=T)

##################################################
## MATCHING

## The model (to create the data for matching)
form3<- (prio ~ aidshock11mil + lPTSave.filled + lassassinbanks + lriotsbanks + lstrikesbanks
                  + ldemonstrationsbanks + linfantmort + lnciv 
                  + lpartautocracy + lpartdemocracy + lfactionaldemoc
                  + lfulldemocracy 
			+ lln.rgdpc + lln.population + loil + linstab 
                  + ethfrac + relfrac + ncontig + logmtn + ColdWar
			+ .spline1 + .spline2 + .spline3 
                  + year  ## year is in here so that we have it later
                  )

m2<-zelig(form3, data=data, model="relogit")
summary(m2)

nrow(m2$data)  ## should be 2627 -- same as main model

  ## get rid of the observations that aren't in the main model
data <- data[as.numeric(rownames(m2$data)),]
rownames(data) <- row(data)[,1]



## Make a figure of the distribution of aid change with AID SHOCKS in gray
hist(data$smaidchmil)
plot(density(na.omit(data$smaidchmil)),type="n", main="Distribution of Changes in Aid", xlim=c(-.05,.05), 
     xlab="Changes in Aid over/GDP", 
     ylab="Density
(Number of observations with this level of Aid Change)")
#abline(v=quantile(na.omit(data$smaidch), probs=.15), lty=2)
#legend(x="topright",c("  15th Percentile","(Aid Shock Cut-off)"), lty=c(1,0),box.lty=0)
legend(x="topright",c("Lowest 15th Percentile","(Coded as Aid Shocks)"), 
       pch=c(15,NA),box.lty=0,col="gray55")
xvals2 <- c(density(na.omit(data$smaidchmil))$x,rev(density(na.omit(data$smaidchmil))$x))
yvals2 <- c(density(na.omit(data$smaidchmil))$y, rep(0,length(density(na.omit(data$smaidchmil))$y)))
polygon( xvals2,yvals2, col="gray95", lty=0)
cutoff <- quantile(na.omit(data$smaidchmil), probs=.15)
xvals <- c(density(na.omit(data$smaidchmil))$x[density(na.omit(data$smaidchmil))$x<cutoff],rev(density(na.omit(data$smaidchmil))$x[density(na.omit(data$smaidchmil))$x<cutoff]))
yvals <- c(density(na.omit(data$smaidchmil))$y[density(na.omit(data$smaidchmil))$x<cutoff], rep(0,length(density(na.omit(data$smaidchmil))$y[density(na.omit(data$smaidchmil))$x<cutoff])))
polygon( xvals,yvals, col="gray55", lty=0)
lines(density(na.omit(data$smaidchmil)))


summary(data$smaidchmil)

  ## we run it again
## The model (to create the data for matching)
form3<- (prio ~ aidshock11mil + lPTSave.filled + lassassinbanks + lriotsbanks + lstrikesbanks
                  + ldemonstrationsbanks + linfantmort + lnciv 
                  + lpartautocracy + lpartdemocracy + lfactionaldemoc
                  + lfulldemocracy 
			+ lln.rgdpc + lln.population + loil + linstab 
                  + ethfrac + relfrac + ncontig + logmtn + ColdWar
			+ .spline1 + .spline2 + .spline3 
                  + year  ## year is in here so that we have it later
                  )

m2<-zelig(form3, data=data, model="relogit")
summary(m2)



## Matching Procedure
library(MatchIt)

head(m2$model[2:26])
m.dat2 <- cbind(m2$model[2:26],m2$y)

  ## with the additional covariates
match.form3<- (aidshock11mil ~ lPTSave.filled + lassassinbanks + lriotsbanks + lstrikesbanks
                  + ldemonstrationsbanks + linfantmort + lnciv 
                  + lpartautocracy + lpartdemocracy + lfactionaldemoc
                  + lfulldemocracy 
			+ lln.rgdpc + lln.population + loil + linstab 
                  + ethfrac + relfrac + ncontig + logmtn + ColdWar
			+ .spline1 + .spline2 + .spline3 )


  ## Genetic WITH PITF vars
set.seed(1111)
out.m <- matchit(match.form3, data=m.dat2, 
		method="genetic")
alarm()

summary(out.m)

#plot(out.m)

head(match.data(out.m))
out<-cbind(data$countryname[as.numeric(rownames(match.data(out.m)))],
           data$year[as.numeric(rownames(match.data(out.m)))],
          (match.data(out.m)))

## out$cunit is the treatmentid for treated units and the 
## id of the matched treated unit for control units
out$cunit<-as.numeric(rownames(out))
for(i in 1:length(rownames(out.m$match.matrix))){
  unit<-rownames(out.m$match.matrix)[i]
  out$cunit[rownames(out)==unit]<- as.numeric(out.m$match.matrix[unit,1])
}

setwd(crappath)
write.csv(out, file="matched data.csv")
setwd(currentpath)

names(out)
names(data)

## LOVE PLOT FUNCTION ###########################################################
## Make a function to create love plots to assess balance

  ## Example inputs
#myformula <- t ~ age + education + black + hispanic + nodegree + married + re74 + re75
#dataset1 <- lalonde
#dataset2 <- match.data(m.out)

## RED is the first dataset, BLUE is the second

loveplot <- function(myformula, dataset1, dataset2, 
             main="Standardized Means Before and After Matching
RED is the first dataset, BLUE is the second", varnames=NULL) {
   ##  love.plot takes three arguments: (1) a formula where
   ##  treatment ~ covariate1 + covariate 2 ...
   ##  (2) an unmatched dataset, and (3) a matched dataset

   data1 <- model.frame(myformula,data=dataset1)
   data2 <- model.frame(myformula,data=dataset2)

   X1 <- model.matrix(myformula, dataset1)[,-1]
   X2 <- model.matrix(myformula, dataset2)[,-1]

   t1 <- model.extract(data1,"response")
   t2 <- model.extract(data2,"response")

   varnames1 <- colnames(X1)
   varnames2 <- colnames(X2)

   X1.t <- X1[t1==1,]
   X1.c <- X1[t1==0,]

   X2.t <- X2[t2==1,]
   X2.c <- X2[t2==0,]

   dif.means1 <- (apply(X1.t, MAR=2, FUN=mean) - apply(X1.c, MAR=2, FUN=mean))/apply(X1, MAR=2, FUN=sd)
   dif.means2 <- (apply(X2.t, MAR=2, FUN=mean) - apply(X2.c, MAR=2, FUN=mean))/apply(X2, MAR=2, FUN=sd)

     ## make the plot
   par(mar=c(5,9,5,2)+.1)
   plot(x=dif.means1, y = rev(seq(1,length(dif.means1),1)), type="n", 
        main=main,
        xlab = "(mean treated - mean control)/sd treated", ylab="", axes=F,
        xlim=c( min(c(dif.means1,dif.means2)),  max(c(dif.means1,dif.means2)))
        #xlim=c( min(c(dif.means1,dif.means2,-.02)),  max(c(dif.means1,dif.means2,.02)))
        )
   abline(v=0, lty=2)
   for(i in 1:length(dif.means1)){
      abline(h=i, col="gray80")
   }
   points(x=dif.means1, y = rev(seq(1,length(dif.means1),1)), col="red", pch=1)
   points(x=dif.means2, y = rev(seq(1,length(dif.means2),1)), col="blue", pch=1)
   one.is.better <- data.frame(cbind(rev(seq(1,length(dif.means1),1)),
                          abs(dif.means1) < abs(dif.means2),
                          dif.means1))
   points(x=one.is.better[,3][one.is.better[,2]==1],
          y=one.is.better[,1][one.is.better[,2]==1],
          col="red", pch=19)
   two.is.better <- data.frame(cbind(rev(seq(1,length(dif.means1),1)),
                          abs(dif.means1) > abs(dif.means2),
                          dif.means2))
   points(x=two.is.better[,3][two.is.better[,2]==1],
          y=two.is.better[,1][two.is.better[,2]==1],
          col="blue", pch=19) 
   #axis(1, at=rev(seq( round(min(dif.means1),.5)) , round(max(dif.means1),.5),length.out = 5), labels=T) 
   axis(1, at=seq(-100,100,.5), labels=T) 
   if(is.null(varnames)){
     varnames <- colnames(X1)
   }
   axis(2, at=rev(seq(1,length(dif.means1),1)), labels=varnames, las=2)
   
   
     ## add a key so I can remember
   #legend(x="topleft", legend=c("1","2"), bty="n",text.col=c(2,4), horiz=T)
}
## END LOVE PLOT FUNCTION ###################################################

form <- (aidshock11mil ~ lPTSave.filled + lassassinbanks + lriotsbanks + lstrikesbanks
                  + ldemonstrationsbanks + linfantmort + lnciv 
                  + lpartautocracy + lpartdemocracy + lfactionaldemoc
                  + lfulldemocracy 
			+ lln.rgdpc + lln.population + loil + linstab 
                  + ethfrac + relfrac + ncontig + logmtn + ColdWar)
			#+ .spline1 + .spline2 + .spline3 )

loveplot(form,out,data,
         main="",
         varnames=c("Human Rights Abuse","Assassinations","Riots","Strikes","Demonstrations",
                    "Infant Mortality","Bad Neighborhood","Partial Autocracy",
                    "Partial Democracy","Factional Democracy","Full Democracy","GDP pc",
                    "Population","Oil","Instability","Ethnic Frac.","Religious Frac.",
                    "Noncontiguous","Mountains","Cold War") )

title(expression("Improved Mean Balance: " * phantom("Matched ") *
  "vs. " * phantom("Unmatched               ")),col.main="black")
title(expression(phantom("Improved Mean Balance: ") * "Matched " *
  phantom("vs. ") * phantom("Unmatched               ")),col.main="red")
title(expression(phantom("Improved Mean Balance: ") * phantom("Matched ") *
  phantom("vs. ") * "Unmatched               "),col.main="blue")



  ## propensity score matching
  ## write the data so I can run the model in stata
out1<-cbind(data$countryname[as.numeric(rownames(m.dat2))], m.dat2) 
setwd(crappath)
write.csv(out1, file="mdat2.csv")
setwd(currentpath)

  ** in Stata
insheet using "crap/mdat2.csv", clear
rename  datacountrynameasnumericrownames countryname
egen countrynum = group(countryname)
tsset countrynum year

xtlogit aidshock11mil lptsavefilled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy llnrgdpc llnpopulation loil linstab ethfrac relfrac ncontig logmtn coldwar spline1 spline2 spline3, re
gen samp = e(sample)
predict pscore if samp==1
sort pscore

save "crap/pscoredat.dta", replace

  ## back in R
setwd(crappath)
library(foreign)
dat <- read.dta("pscoredat.dta")
head(dat)
  ## order them by pscore
dat <- dat[rev(order(dat$pscore)),]

tdat <- dat[dat$aidshock11mil==1,]
cdat <- dat[dat$aidshock11mil==0,]
cdattemp <- cdat

matchdat <- c()
for(i in 1:nrow(tdat)){
  tunit <- tdat[i,]
  t <- tunit$pscore
  cpool <- cdattemp$pscore
  cunit <- cdattemp[which(abs((t-cpool))==min(abs(t-cpool))),]
  newmatch <- rbind(tunit, cunit)
  matchdat <- rbind(matchdat,newmatch)
}

nrow(matchdat)

write.csv(matchdat, file="psmatchdat.csv")


## LOVE PLOT FUNCTION ###########################################################
## Make a function to create love plots to assess balance

  ## Example inputs
#myformula <- t ~ age + education + black + hispanic + nodegree + married + re74 + re75
#dataset1 <- lalonde
#dataset2 <- match.data(m.out)

## RED is the first dataset, BLUE is the second

loveplot <- function(myformula, dataset1, dataset2, 
             main="Standardized Means Before and After Matching
RED is the first dataset, BLUE is the second", varnames=NULL) {
   ##  love.plot takes three arguments: (1) a formula where
   ##  treatment ~ covariate1 + covariate 2 ...
   ##  (2) an unmatched dataset, and (3) a matched dataset

   data1 <- model.frame(myformula,data=dataset1)
   data2 <- model.frame(myformula,data=dataset2)

   X1 <- model.matrix(myformula, dataset1)[,-1]
   X2 <- model.matrix(myformula, dataset2)[,-1]

   t1 <- model.extract(data1,"response")
   t2 <- model.extract(data2,"response")

   varnames1 <- colnames(X1)
   varnames2 <- colnames(X2)

   X1.t <- X1[t1==1,]
   X1.c <- X1[t1==0,]

   X2.t <- X2[t2==1,]
   X2.c <- X2[t2==0,]

   dif.means1 <- (apply(X1.t, MAR=2, FUN=mean) - apply(X1.c, MAR=2, FUN=mean))/apply(X1, MAR=2, FUN=sd)
   dif.means2 <- (apply(X2.t, MAR=2, FUN=mean) - apply(X2.c, MAR=2, FUN=mean))/apply(X2, MAR=2, FUN=sd)

     ## make the plot
   par(mar=c(5,9,5,2)+.1)
   plot(x=dif.means1, y = rev(seq(1,length(dif.means1),1)), type="n", 
        main=main,
        xlab = "(mean treated - mean control)/sd treated", ylab="", axes=F,
        xlim=c( min(c(dif.means1,dif.means2)),  max(c(dif.means1,dif.means2)))
        #xlim=c( min(c(dif.means1,dif.means2,-.02)),  max(c(dif.means1,dif.means2,.02)))
        )
   abline(v=0, lty=2)
   for(i in 1:length(dif.means1)){
      abline(h=i, col="gray80")
   }
   points(x=dif.means1, y = rev(seq(1,length(dif.means1),1)), col="red", pch=1)
   points(x=dif.means2, y = rev(seq(1,length(dif.means2),1)), col="blue", pch=1)
   one.is.better <- data.frame(cbind(rev(seq(1,length(dif.means1),1)),
                          abs(dif.means1) < abs(dif.means2),
                          dif.means1))
   points(x=one.is.better[,3][one.is.better[,2]==1],
          y=one.is.better[,1][one.is.better[,2]==1],
          col="red", pch=19)
   two.is.better <- data.frame(cbind(rev(seq(1,length(dif.means1),1)),
                          abs(dif.means1) > abs(dif.means2),
                          dif.means2))
   points(x=two.is.better[,3][two.is.better[,2]==1],
          y=two.is.better[,1][two.is.better[,2]==1],
          col="blue", pch=19) 
   #axis(1, at=rev(seq( round(min(dif.means1),.5)) , round(max(dif.means1),.5),length.out = 5), labels=T) 
   axis(1, at=seq(-100,100,.5), labels=T) 
   if(is.null(varnames)){
     varnames <- colnames(X1)
   }
   axis(2, at=rev(seq(1,length(dif.means1),1)), labels=varnames, las=2)
   
   
     ## add a key so I can remember
   #legend(x="topleft", legend=c("1","2"), bty="n",text.col=c(2,4), horiz=T)
}
## END LOVE PLOT FUNCTION ###################################################

form <- (aidshock11mil ~ lPTSave.filled + lassassinbanks + lriotsbanks + lstrikesbanks
                  + ldemonstrationsbanks + linfantmort + lnciv 
                  + lpartautocracy + lpartdemocracy + lfactionaldemoc
                  + lfulldemocracy 
			+ lln.rgdpc + lln.population + loil + linstab 
                  + ethfrac + relfrac + ncontig + logmtn + ColdWar)
			#+ .spline1 + .spline2 + .spline3 )

matchdat2 <- matchdat
colnames(matchdat2) <- gsub("lptsavefilled", "lPTSave.filled", colnames(matchdat2),fixed=T)
colnames(matchdat2) <- gsub("llnrgdpc", "lln.rgdpc", colnames(matchdat2),fixed=T)
colnames(matchdat2) <- gsub("llnpopulation", "lln.population", colnames(matchdat2),fixed=T)
colnames(matchdat2) <- gsub("coldwar", "ColdWar", colnames(matchdat2),fixed=T)
colnames(matchdat2) <- gsub("spline1", ".spline1", colnames(matchdat2),fixed=T)
colnames(matchdat2) <- gsub("spline2", ".spline2", colnames(matchdat2),fixed=T)
colnames(matchdat2) <- gsub("spline3", ".spline3", colnames(matchdat2),fixed=T)


loveplot(form,matchdat2,data,
         main="",
         varnames=c("Human Rights Abuse","Assassinations","Riots","Strikes","Demonstrations",
                    "Infant Mortality","Bad Neighborhood","Partial Autocracy",
                    "Partial Democracy","Factional Democracy","Full Democracy","GDP pc",
                    "Population","Oil","Instability","Ethnic Frac.","Religious Frac.",
                    "Noncontiguous","Mountains","Cold War") )

title(expression("Improved Mean Balance: " * phantom("Matched ") *
  "vs. " * phantom("Unmatched               ")),col.main="black")
title(expression(phantom("Improved Mean Balance: ") * "Matched " *
  phantom("vs. ") * phantom("Unmatched               ")),col.main="red")
title(expression(phantom("Improved Mean Balance: ") * phantom("Matched ") *
  phantom("vs. ") * "Unmatched               "),col.main="blue")

## reset the working directory
setwd(currentpath)

*/

	*************
	** End R code
	*************

  ** Propensity score matching
insheet using "crap\psmatchdat.csv", clear
rename m2y prio
rename lptsavefilled lPTSave_filled
rename llnrgdpc lln_rgdpc
rename llnpopulation lln_population
rename coldwar ColdWar
rename spline1 _spline1
rename spline2 _spline2
rename spline3 _spline3
relogit prio aidshock11mil lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar  _spline1 _spline2 _spline3, cl(countrynum)
outreg2 using "output/finalmodelsmil", word append
** This code runs the matching analysis
** After running the matching procedure to create a .csv...

  ** Genetic Matching
clear
insheet using "crap/matched data.csv"
rename datacountrynameasnumericrownames countryname
rename llnpopulation lln_population
rename llnrgdpc lln_rgdpc
rename coldwar ColdWar
rename spline1 _spline1
rename spline2 _spline2
rename spline3 _spline3
capture rename lptsavefilled lPTSave_filled
capture rename m2y prio
egen countrynum = group(countryname)
tsset countrynum year
  ** doesn't make sense to include aidshocks11pos in the matching
relogit prio aidshock11mil lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, cl(countrynum)
outreg2 using "output/finalmodelsmil", word append


** END OF CODE
