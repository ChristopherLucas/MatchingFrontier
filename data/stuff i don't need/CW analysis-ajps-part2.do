/* Analysis for Aid Shocks

  This file does most of the empirical analysis

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


**********************************************
** Descriptive look at the data
**********************************************

/*
use cwdata, clear
capture drop countrynum
egen countrynum = group(countryname)
tsset countrynum year

gen fprio = f.prio
gen f2prio = f2.prio
gen f3prio = f3.prio
gen f4prio = f4.prio
gen f5prio = f5.prio

gen prio5year = prio + fprio + f2prio + f3prio + f4prio

sort smaidch

  ** keeps only the obs in my sample
relogit prio aidshock11 aidshock11pos lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, cl(countrynum)
keep if e(sample)==1
*edit countryname year smaidch prio5year prio fprio f2prio f3prio f4prio f5prio

egen severityofshock = rank(smaidch)

gen mostsevere = 1 if severityofshock<=15
replace severityofshock=. if severityofshock>15

egen topseverity = max(mostsevere), by(countryname)

egen magnitudeofworstshock = min(smaidch),by(countryname)

sort magnitudeofworstshock countryname year 
edit countryname year smaidch aidshock11 mostsevere prio severityofshock if topseverity==1
** This shows the tscs data for the most severe aid shocks.
** To see that severe aid shocks are followed by conflict onset
** look at the years in which "mostsevere" == 1 and then look at 
** the value of "prio" in that year and the following years.  When
** "prio" == 1, this means that there was civil conflict.
*/ 

/* Make a list of aid shocks that are followed by civil war

gen follow = 0
tsset countrynum year
*replace follow = 1 if prio==1 & (aidshock11==1 | l.aidshock11==1 | l2.aidshock11==1 | l3.aidshock11==1 | l4.aidshock11==1 | l5.aidshock11==1 | l6.aidshock11==1)
replace follow = 1 if aidshock11==1 & (prio==1 | f.prio==1 | f2.prio==1 | f3.prio==1 | f4.prio==1 | f5.prio==1 | f6.prio==1)
replace follow = 1 if l.aidshock11==1 & (prio==1 | f.prio==1 | f2.prio==1 | f3.prio==1 | f4.prio==1 | f5.prio==1)
replace follow = 1 if l2.aidshock11==1 & (prio==1 | f.prio==1 | f2.prio==1 | f3.prio==1 | f4.prio==1)
replace follow = 1 if l3.aidshock11==1 & (prio==1 | f.prio==1 | f2.prio==1 | f3.prio==1)
replace follow = 1 if l4.aidshock11==1 & (prio==1 | f.prio==1 | f2.prio==1)
replace follow = 1 if l5.aidshock11==1 & (prio==1 | f.prio==1)
replace follow = 1 if l6.aidshock11==1 & (prio==1)

sort countryname year
edit countryname year smaidch aidshock11 prio mostsevere severityofshock if follow==1
** This creates a list of all the aid shocks that were
** followed within six years by civil conflict onset.
** To see this, look for "aidshock" == 1 and see that
** "prio" == 1 in one of the subsequent years.
** These are the cases where it is possible that aid
** shocks could have some effect on conflict onset.
*/


*********************************
** Show the cutoff for the aid shocks variable
*********************************
use cwdata, clear

** This is the average level of aid dependence
** First, run the model so that I have the right sample
relogit prio aidshock11 aidshock11pos lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, cl(countrynum)

** Negative Aid Shocks
** this is the aidshock11 cutoff
_pctile smaidch if e(sample)==1, p(15)
di r(r1)

sum aidpergdp if e(sample)==1
  ** number of aid dollars per 100 dollars of GDP = 
di 100*r(mean)
  ** an aid decrease of .0054 is a ...
di .54/(100*r(mean))
  ** ... percent decrease in foreign aid.

** Positive Aid Shocks
** this is the aidshock11pos cutoff
_pctile smaidch if inmysample==1, p(85)
di r(r1)

sum aidpergdp if e(sample)==1
  ** number of aid dollars per 100 dollars of GDP = 
di 100*r(mean)
  ** an aid increase of .00524 is a...
di .524/(100*r(mean))
  ** ... percent increase in foreign aid.


*************************************************************************
** Analysis
*************************************************************************

** after running this section, the file "output/finalmodels.doc" will have the results

  ** This is part of making the table of results
capture erase "output/finalmodels.txt"
capture erase "output/finalmodels.rtf"

***************************************************
** Main Model: aid shocks with full covariate set
***************************************************
use cwdata, clear
relogit prio aidshock11 aidshock11pos lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, cl(countrynum)

outreg2 using "output/finalmodels", word append

  ** test that the negative and positive aid shock coeffs aren't equal
test aidshock11 = aidshock11pos

  ** quantities of interest
set seed 1234
setx  (aidshock11) 0 (aidshock11pos) 0 (lpartautocracy) 1 (lpartdemocracy lfactionaldemoc lfulldemocracy) 0 (lPTSave_filled linfantmort lnciv lln_rgdpc  lln_population  loil ethfrac relfrac ncontig logmtn _spline1 _spline2 _spline3) mean ( ColdWar lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks  linstab) median
relogitq, pr listx
setx  (aidshock11) 1 (aidshock11pos) 0 (lpartautocracy) 1 (lpartdemocracy lfactionaldemoc lfulldemocracy) 0 (lPTSave_filled linfantmort lnciv lln_rgdpc  lln_population  loil ethfrac relfrac ncontig logmtn _spline1 _spline2 _spline3) mean ( ColdWar lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks  linstab) median
relogitq, pr listx
relogitq, fd(pr) listx changex(aidshock11 0 1)


  ** see how many onsets are in the sample
sum prio if e(sample)==1
tab prio if e(sample)==1
  ** see how many countries are in the sample
egen cgroup = group(countryname) if e(sample)==1
sum cgroup
drop cgroup
  ** see which years are in the sample
sum year if e(sample)==1

  ** without positive aid shocks
relogit prio aidshock11 lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, cl(countrynum)

  ** In the first AJPS version we had a smaller covariate set, but more obs. (1975-2004)
relogit prio aidshock11 aidshock11pos lln_rgdpc  ln_population  oil instab ethfrac relfrac ncontig logmtn ColdWar lpolity2 , cl(countrynum)
local oldN = e(N)

  ** to see where the additional covariates force us to lose coverage:
quietly relogit prio aidshock11 aidshock11pos lPTSave_filled lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar , cl(countrynum)
di `oldN' - e(N) " obs lost from PTS"
local oldN = e(N)
  ** bankscombined
quietly relogit prio aidshock11 aidshock11pos lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar , cl(countrynum)
di `oldN' - e(N) " more obs lost from banks"
local oldN = e(N)
  ** infantmort 
quietly relogit prio aidshock11 aidshock11pos lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar , cl(countrynum)
di `oldN' - e(N) " more obs lost from infantmort"
local oldN = e(N)
  ** nciv (bad neighborhood)
quietly relogit prio aidshock11 aidshock11pos lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar , cl(countrynum)
di `oldN' - e(N) " more obs lost from nciv"
local oldN = e(N)
  ** Bates democracy var 
quietly relogit prio aidshock11 aidshock11pos lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar , cl(countrynum)
di `oldN' - e(N) " more obs lost from bates democracy"
local oldN = e(N)


  ** We don't actually include the following covariates but we looked at them.
  ** State descrimination 3293 -> 2214
quietly relogit prio aidshock11 aidshock11pos lstatediscrim lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar , cl(countrynum)
di `oldN' - e(N) " more obs lost from lstatediscrim"
local oldN = e(N)
  ** physint 
quietly relogit prio aidshock11 aidshock11pos lstatediscrim lphysint lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar , cl(countrynum)
di `oldN' - e(N) " more obs lost from physint"

  ** MAR var
relogit prio aidshock11 aidshock11pos lstatediscrim lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, cl(countrynum)


***************************************************
** Continuous Measures of Aid Change
***************************************************

  ** These models are discussed briefly in the appendix
  ** all aid changes (look at coefficient on "smaidch")
relogit prio smaidch lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, cl(countrynum)
  ** trying a curvilinear relationship
gen smaidch2 = smaidch^2
gen smaidch3 = smaidch^3
relogit prio smaidch smaidch2 lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, cl(countrynum)
relogit prio smaidch smaidch2 smaidch3 lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, cl(countrynum)

  ** try to make a multicategory var
relogit prio aidshock11 lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, cl(countrynum)
capture drop inmysample
gen inmysample = e(sample)
capture drop smaidcat
gen smaidcat = .
local k = 0
local oldq = -100

foreach NUM of numlist  10 20 30 40 50 60 70 80 90 {
  local k = `k' + 1
  di `k'
  di `NUM'
   _pctile smaidch if inmysample==1, p(`NUM')
   local newq=r(r1)
   di `newq'
   replace smaidcat = `k' if smaidch>=`oldq' & smaidch<`newq'
   local oldq=`newq'
}

  ** with 9 cats, cat 6 has the 0's
*edit smaidcat smaidch
char smaidcat[omit] 1

xi: relogit prio i.smaidcat lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, cl(countrynum)
** The lower categories are the negative aid changes.
** The higher categories are the positive aid changes
bysort smaidcat: sum smaidch

** Look only at negative aid changes
** "negaid" is the same as the "smaidch" if the change is negative
** and zero if "smaidch" is positive.  "posaid" is the reverse.
relogit prio negaid lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, cl(countrynum)
relogit prio negaid posaid lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, cl(countrynum)

***************************************************
** VIF test
***************************************************
  ** checking for collinearity :http://www.ats.ucla.edu/stat/Stata/webbooks/logistic/chapter3/statalog3.htm
  ** full covariates
collin aidshock11 aidshock11pos lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar
  ** without infantmort -- looks like it is collinear with GDP p.c.
collin aidshock11 aidshock11pos lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks   lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar
  ** Either way, everything is below 5, which is the rule of thumb.
  ** AND -- this would mean that the variances were INFLATED, meaning that we are
  ** UNDERESTIMATING the significance of Aid shocks, not over-estimating it.

  ** in any case, the results stay the same
use cwdata, clear
  ** without infant mortality
relogit prio aidshock11 aidshock11pos lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, cl(countrynum)
  ** without GDP pc
relogit prio aidshock11 aidshock11pos lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, cl(countrynum)
  ** without both
relogit prio aidshock11 aidshock11pos lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, cl(countrynum)

***************************************************
** Looking at Influential Observations
***************************************************
use cwdata, clear
  ** code from here: http://www.ats.ucla.edu/stat/Stata/webbooks/logistic/chapter3/statalog3.htm

  ** results are saved in "output/influencemodels"
capture erase "output/influencemodels.txt"
capture erase "output/influencemodels.rtf"

  ** can't do this plot with relogit because it doesn't support the "rstand" command
  ** relogit fixes some of the under-prediction, so logit is likely to look worse than relogit.
logit prio aidshock11 aidshock11pos lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, cl(countrynum)
  ** Actually, the results look really really similar
  ** plot 1 -- plot of Pearson residuals versus predicted probabilities.
predict p
predict stdres, rstand
gen uid=10000*countrynum + year
scatter stdres p, mlabel(uid) ylab(-4(2) 16) yline(0)

  ** plot 2 -- plot of Pearson residuals versus observation ID.
gen id=_n
scatter stdres id, mlab(uid) ylab(-4(2) 16) yline(0)

  ** look at the obs that are influential
*edit countryname year prio aidshock11 if stdres >2

  ** rerun the model with just observations that had stdres < 5
relogit prio aidshock11 aidshock11pos lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3 if stdres<5, cl(countrynum)
outreg2 using "output/influencemodels", word append

  ** stdres < 1 doesn't work because it excludes ALL aid shocks
  ** stdres < 3
logit prio aidshock11 aidshock11pos lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3 if stdres<3, cl(countrynum)
 
  ** plot 3 -- plot of deviance residuals versus predicted probabilities.
logit prio aidshock11 aidshock11pos lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, cl(countrynum)
predict dv, dev
scatter dv p, mlab(uid) yline(0)
  ** plot 4 -- plot of deviance residuals versus unit IDs.
scatter dv id, mlab(uid)
  ** Run some models excluding these influential observations
  ** if dv < 2
logit prio aidshock11 aidshock11pos lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3 if dv<2, cl(countrynum)
relogit prio aidshock11 aidshock11pos lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy                lfactionaldemoc                lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3 if dv<2, cl(countrynum)
outreg2 using "output/influencemodels", word append
  ** can't do dv<1 because then all conflicts are excluded

logit prio aidshock11 aidshock11pos lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, cl(countrynum)
  ** plot 5 -- Leverage vs predicted probabilities
predict hat, hat
scatter hat p, mlab(uid)  yline(0)
  ** plot 6 -- Leverage vs unit ID.
scatter hat id, mlab(uid)

  ** rerun the model with just observations that had hat < .05
relogit prio aidshock11 aidshock11pos lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3 if hat<.05, cl(countrynum)
outreg2 using "output/influencemodels", word append
 
***************************************************************
** Run the leads to predict aid shocks as a reviewer suggests
***************************************************************
  ** one of the reviewers suggested this model.  They agreed
  ** to be convinced if the lead of conflict didn't predict aid
  ** aid shocks.  This might be suggestive that donors aren't 
  ** anticipating conflict and withholding aid in anticipation.
  ** We don't place a ton of stock in this test either way, but
  ** we don't find that the lead of conflict predicts aid shocks.

use cwdata, clear
tsset countrynum year
gen f1prio = f.prio
gen f2prio = f2.prio
  ** The model with leads included
logit aidshock11 f1prio f2prio lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, cl(countrynum)
  ** Including random effects
xtlogit aidshock11 f1.prio f2.prio lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, re
  ** Including fixed effects
xtlogit aidshock11 f1.prio f2.prio lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, fe
  ** include the lags of prio too, (with random effects)
xtlogit aidshock11 l.prio l2.prio l3.prio f1.prio f2.prio lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, re
  ** include the lags of prio too, (with fixed effects)
xtlogit aidshock11 l.prio l2.prio l3.prio f1.prio f2.prio lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, fe


*************************************************************
** Fixed Effects Models / Random Effects models
*************************************************************

** Fixed Effects
use cwdata, clear
xtlogit prio aidshock11 aidshock11pos lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, fe

outreg2 using "output/finalmodels", word append

** continuous treatments
xtlogit prio smaidch aidshock11pos lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, fe
xtlogit prio negaid aidshock11pos lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, fe

** Random Effects
use cwdata, clear
xtlogit prio aidshock11 aidshock11pos lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, re


*************************************************************
** Matching Models
*************************************************************

	********************
	** R code for matching
	********************
** Run the matching procedure by copying this code into R
/*

## You need to set your working directory to where the dataset is.

## Reading in the data
library(foreign)
library(Zelig)
data<-read.dta("cwdata.dta", convert.underscore=T)

##################################################
## MATCHING

## The model (to create the data for matching)
form3<- (prio ~ aidshock11 + lPTSave.filled + lassassinbanks + lriotsbanks + lstrikesbanks
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
hist(data$smaidch)

## get the current directory
currentpath <- getwd()

## the new path
outdir <- paste(currentpath,"/output", sep="")
crapdir <- paste(currentpath,"/crap", sep="")

## set the new directory to the output folder to save the figure
setwd(outdir)
pdf(file = "AidChangeDist.pdf", width = 7, height = 6)
par(mar=c(5,6,5,2)+.01)
plot(density(na.omit(data$smaidch)),type="n", main="Distribution of Changes in Aid", xlim=c(-.05,.05), 
     xlab="Changes in Aid/GDP", 
     ylab="Density
(Number of observations at a given level of Aid Change)",
     axes=F)
#abline(v=quantile(na.omit(data$smaidch), probs=.15), lty=2)
#legend(x="topright",c("  15th Percentile","(Aid Shock Cut-off)"), lty=c(1,0),box.lty=0)
legend(x="topleft",c("Lowest 15 Percent","(Coded as Aid Shocks)","",
                      "Upper 15 Percent","(Coded as Positive Aid Shocks)"), 
       pch=c(15,NA,NA,15,NA),box.lty=0,col=c("gray35","gray65"))
xvals2 <- c(density(na.omit(data$smaidch))$x,rev(density(na.omit(data$smaidch))$x))
yvals2 <- c(density(na.omit(data$smaidch))$y, rep(0,length(density(na.omit(data$smaidch))$y)))
polygon( xvals2,yvals2, col="gray65", lty=0)

  ## for positive aid shocks
cutoff <- quantile(na.omit(data$smaidch), probs=.85)
cutoff
xvals <- c(density(na.omit(data$smaidch))$x[density(na.omit(data$smaidch))$x<cutoff],rev(density(na.omit(data$smaidch))$x[density(na.omit(data$smaidch))$x<cutoff]))
yvals <- c(density(na.omit(data$smaidch))$y[density(na.omit(data$smaidch))$x<cutoff], rep(0,length(density(na.omit(data$smaidch))$y[density(na.omit(data$smaidch))$x<cutoff])))
polygon( xvals,yvals, col="gray95", lty=0)
  ## for negative shocks
cutoff <- quantile(na.omit(data$smaidch), probs=.15)
cutoff
xvals <- c(density(na.omit(data$smaidch))$x[density(na.omit(data$smaidch))$x<cutoff],rev(density(na.omit(data$smaidch))$x[density(na.omit(data$smaidch))$x<cutoff]))
yvals <- c(density(na.omit(data$smaidch))$y[density(na.omit(data$smaidch))$x<cutoff], rep(0,length(density(na.omit(data$smaidch))$y[density(na.omit(data$smaidch))$x<cutoff])))
polygon( xvals,yvals, col="gray35", lty=0)
lines(density(na.omit(data$smaidch)))
axis(1,at=seq(-.06,.06,.02),labels=T)
axis(2,at=seq(0,200,50),labels=T)
dev.off()
## set the directory back to the original directory
setwd(currentpath)

summary(data$smaidch)

  ## we run it again
## The model (to create the data for matching)
form3<- (prio ~ aidshock11 + lPTSave.filled + lassassinbanks + lriotsbanks + lstrikesbanks
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
match.form3<- (aidshock11 ~ lPTSave.filled + lassassinbanks + lriotsbanks + lstrikesbanks
                  + ldemonstrationsbanks + linfantmort + lnciv 
                  + lpartautocracy + lpartdemocracy + lfactionaldemoc
                  + lfulldemocracy 
			+ lln.rgdpc + lln.population + loil + linstab 
                  + ethfrac + relfrac + ncontig + logmtn + ColdWar
			+ .spline1 + .spline2 + .spline3 )


  ## Genetic WITH PITF vars (Takes 19 minutes on my machine)
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

setwd(crapdir)
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

form <- (aidshock11 ~ lPTSave.filled + lassassinbanks + lriotsbanks + lstrikesbanks
                  + ldemonstrationsbanks + linfantmort + lnciv 
                  + lpartautocracy + lpartdemocracy + lfactionaldemoc
                  + lfulldemocracy 
			+ lln.rgdpc + lln.population + loil + linstab 
                  + ethfrac + relfrac + ncontig + logmtn + ColdWar)
			#+ .spline1 + .spline2 + .spline3 )


setwd(outdir)
jpeg(filename = "Genmatch Loveplot.jpg", width = 6.5, height = 6, res=200,
     units = "in")
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
dev.off()

setwd(currentpath)

  ## propensity score matching
  ## write the data so I can run the model in stata
out1<-cbind(data$countryname[as.numeric(rownames(m.dat2))], m.dat2) 
setwd(crapdir)
write.csv(out1, file="mdat2.csv")
setwd(currentpath)

  ** in Stata -- just copy and paste this into stata with the directories
  **             set to the path where the datasets are

insheet using "crap/mdat2.csv", clear
rename  datacountrynameasnumericrownames countryname
egen countrynum = group(countryname)
tsset countrynum year

xtlogit aidshock11 lptsavefilled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy llnrgdpc llnpopulation loil linstab ethfrac relfrac ncontig logmtn coldwar spline1 spline2 spline3, re
gen samp = e(sample)
predict pscore if samp==1
sort pscore
outreg2 using "output/pscoremodel", word append

save "crap/pscoredat.dta", replace


  ## back in R
setwd(crapdir)
library(foreign)
dat <- read.dta("pscoredat.dta")
head(dat)
  ## order them by pscore
dat <- dat[rev(order(dat$pscore)),]

tdat <- dat[dat$aidshock11==1,]
cdat <- dat[dat$aidshock11==0,]
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

form <- (aidshock11 ~ lPTSave.filled + lassassinbanks + lriotsbanks + lstrikesbanks
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

setwd(outdir)
jpeg(filename = "Pscore Loveplot.jpg", width = 6.5, height = 6, res=200,
     units = "in")
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
dev.off()
setwd(currentpath)
 

## Code to put the two love plots on top of each other


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


  ## Pscore balance
setwd(crapdir)
matchdat <- read.csv(file="psmatchdat.csv")

form <- (aidshock11 ~ lPTSave.filled + lassassinbanks + lriotsbanks + lstrikesbanks
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

  ## make the combined love plot
setwd(outdir)
jpeg(filename = "Combined Loveplots.jpg", width = 12, height = 6, res=200,
     units = "in")

par(mfrow=c(1,2))

loveplot(form,matchdat2,data,
         main="",
         varnames=c("Human Rights Abuse","Assassinations","Riots","Strikes","Demonstrations",
                    "Infant Mortality","Bad Neighborhood","Partial Autocracy",
                    "Partial Democracy","Factional Democracy","Full Democracy","GDP pc",
                    "Population","Oil","Instability","Ethnic Frac.","Religious Frac.",
                    "Noncontiguous","Mountains","Cold War") )

title(expression("Propensity Score Matching: " * phantom("Matched ") *
  "vs. " * phantom("Unmatched               ")),col.main="black")
title(expression(phantom("Propensity Score Matching: ") * "Matched " *
  phantom("vs. ") * phantom("Unmatched               ")),col.main="red")
title(expression(phantom("Propensity Score Matching: ") * phantom("Matched ") *
  phantom("vs. ") * "Unmatched               "),col.main="blue")



  ## Genetic matching balance

setwd(crapdir)
out <- read.csv(file="matched data.csv")
setwd(outdir)
names(out)
names(data)

form <- (aidshock11 ~ lPTSave.filled + lassassinbanks + lriotsbanks + lstrikesbanks
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

title(expression("Genetic Matching: " * phantom("Matched ") *
  "vs. " * phantom("Unmatched               ")),col.main="black")
title(expression(phantom("Genetic Matching: ") * "Matched " *
  phantom("vs. ") * phantom("Unmatched               ")),col.main="red")
title(expression(phantom("Genetic Matching: ") * phantom("Matched ") *
  phantom("vs. ") * "Unmatched               "),col.main="blue")

dev.off()

## reset the working directory
setwd(currentpath)

*/

	*************
	** End R code
	*************

** This code shows you the matched dataset (with the years merged in):
  ** for the full covariate set
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
keep if weights!=.
   ** Obs. with weights==1 are in the matched dataset.
move year countryname
move  aidshock11 lln_population
move cunit  year

  ** To see which units are remaining in each time series
sort  countryname year
*edit
  ** To see which units are matched to which
sort cunit aidshock11
*edit
  ** To see what proportion of treate units are matched to observations within the same country

save "crap/tempM.dta", replace

/*
## in R

## Set working directory to where the data is 

## set the new path from the current one
currentpath <- getwd()
setwd(paste(currentpath,"/crap",sep=""))

library(foreign)
dat <- read.dta("tempM.dta")

dat2 <- subset(dat, select=c("cunit","countryname"))

cunits <- unique(dat$cunit)
holder <- rep(NA,length(cunits))
for(i in 1:length(cunits)){
  c <- cunits[i]
  cnames <- dat$countryname[dat$cunit==c]
  ucnames <- unique(cnames)
  if(length(ucnames)<length(cnames)) {
    holder[i] <- 1
  } else {
    holder[i] <- 0
  }
} 

mean(holder)  ## this is the overall number of same-country matches.
# [1] 0.8918

## reset the working directory
setwd(currentpath)

######
*/



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

*relogit prio aidshock11 lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar  _spline1 _spline2 _spline3, cl(countrynum)
  ** drop ncontig -- it's colinear
relogit prio aidshock11 lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac  logmtn ColdWar  _spline1 _spline2 _spline3, cl(countrynum)

outreg2 using "output/finalmodels", word append

	** quantities of interest
set seed 1234
setx  (aidshock11) 0  (lpartautocracy) 1 (lpartdemocracy lfactionaldemoc lfulldemocracy) 0 (lPTSave_filled linfantmort lnciv lln_rgdpc  lln_population  loil ethfrac relfrac         logmtn _spline1 _spline2 _spline3) mean ( ColdWar lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks  linstab) median
relogitq, pr listx
setx  (aidshock11) 1  (lpartautocracy) 1 (lpartdemocracy lfactionaldemoc lfulldemocracy) 0 (lPTSave_filled linfantmort lnciv lln_rgdpc  lln_population  loil ethfrac relfrac         logmtn _spline1 _spline2 _spline3) mean ( ColdWar lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks  linstab) median
relogitq, pr listx
relogitq, fd(pr) listx changex(aidshock11 0 1)


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
	

  ** full model (PTS and low-level conflict vars)
egen countrynum = group(countryname)
tsset countrynum year
  ** doesn't make sense to include aidshocks11pos in the matching
relogit prio aidshock11 lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, cl(countrynum)
outreg2 using "output/finalmodels", word append

  ** THIS SHOULD BE WEIGHTED -- GENMATCH RETURNS OBS. WEIGHTS!!!!
  ** We didn't run this quite right for the AJPS R&R
  ** But it doesn't really matter because the weights don't
  ** make much of a difference.
relogit prio aidshock11 lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3  [iweight=weights],  cl(countrynum) 


************************************************************
** instrumental variables -- with invalid instruments
************************************************************

** This uses Security Council membership as an instrument.
clear
use cwdata
** get the sample
relogit prio aidshock11 aidshock11pos lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, cl(countrynum)
gen samp = e(sample)
tsset countrynum year
ivprobit prio (aidshock11=l1.scmember) lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar  _spline1 _spline2 _spline3 if samp==1, cl(countryname) first
test l1.scmember
  ** without covariates
ivprobit prio (aidshock11=l1.scmember) if samp==1, cl(countryname) first

  ** The implausible donor gdp levels instrument
  ** This is the same covariates as I used for UNSC membership
probit aidshock11 l1.llndonorgdp lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar  _spline1 _spline2 _spline3 if samp==1
ivprobit prio (aidshock11=llndonorgdp) lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar  _spline1 _spline2 _spline3 if samp==1, cl(countryname) first
  ** note that donorgdp is a really strong predictor of conflict
probit prio aidshock11 llndonorgdp lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar  _spline1 _spline2 _spline3 if samp==1, cl(countryname)
scatter donorgdp year if samp==1
gen donorgdpchange = donorgdp - l.donorgdp
  ** This is the one we use
ivprobit prio (aidshock11=ll.donorgdpchange) lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar  _spline1 _spline2 _spline3 if samp==1, cl(countryname) first
scatter donorgdpchange year if samp==1
  ** without covariates
ivprobit prio (aidshock11=ll.donorgdpchange) if samp==1, cl(countryname) first


** UN friend instrument
  ** The continuous affinity to the US variable
use cwdata, clear
  ** get the sample
relogit prio aidshock11 aidshock11pos lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, cl(countrynum)
gen samp=e(sample)
  ** run some IV models
ivprobit prio (aidshock11=s3un) lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar  _spline1 _spline2 _spline3 if samp==1, cl(countryname) first
ivprobit prio (aidshock11=s3un) if samp==1, cl(countryname) first
ivprobit prio (aidshock11=s2un) lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar  _spline1 _spline2 _spline3 if samp==1, cl(countryname) first


** make a binary affinity variable called "friend"
** Remember that this is only US friends
relogit prio aidshock11 aidshock11pos lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, cl(countrynum)
_pctile s3un if e(sample)==1, p(75)
local cutoff=r(r1)
gen friend = 1 if s3un>=`cutoff' & s3un!=.
replace friend = 0 if s3un<`cutoff' &s3un!=.
capture drop friend2
_pctile s3un if e(sample)==1, p(25)
local cutoff=r(r1)
gen friend2 = 1 if s3un>=`cutoff' & s3un!=.
replace friend2 = 0 if s3un<`cutoff' &s3un!=.
  ** friend is missing values for 2003 & 2004 because of when the UN var ends.
tab friend if e(sample)==1
ivprobit prio (aidshock11=friend) lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar  _spline1 _spline2 _spline3 if samp==1, cl(countryname) first
  ** without covariates
ivprobit prio (aidshock11=friend) if samp==1, cl(countryname) first


**************************************************
** Testing for an interaction between Aid Shocks and Aid Dependence
**************************************************

** parametric interaction
use "cwdata.dta", clear
gen aidshock11Xaiddep= aidshock11*lagaidpergdp
relogit prio aidshock11 lagaidpergdp aidshock11Xaiddep lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, cl(countrynum)

** For different percentiles of aid dependence (lagaidpergdp)
use "cwdata.dta", clear
capture drop samp
relogit prio aidshock11 aidshock11pos lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, cl(countrynum)
gen samp=e(sample)

** below 50th pctile
_pctile lagaidpergdp if samp==1, p(50)
local q50=r(r1)
relogit prio aidshock11 lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3 if lagaidpergdp<`q50', cl(countrynum)

** above 50th pctile 
_pctile lagaidpergdp if samp==1, p(50)
local q50=r(r1)
relogit prio aidshock11 lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3 if lagaidpergdp>`q50', cl(countrynum)

** between 50th pctile and 75th 
_pctile lagaidpergdp if samp==1, p(50)
local q50=r(r1)
_pctile lagaidpergdp if samp==1, p(75)
local q75=r(r1)
logit prio aidshock11 lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3 if lagaidpergdp>`q50' & lagaidpergdp<`q75', cl(countrynum)

** above 75th pctile (note, without oil or nwstate because of colinearity)
logit prio aidshock11 aidshock11pos lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3 if lagaidpergdp>`q75', cl(countrynum)

gen aidshock11Xaiddep= aidshock11*lagaidpergdp
relogit prio aidshock11 lagaidpergdp aidshock11Xaiddep lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, cl(countrynum)


**********************************************
** A Model with Aid Dependence (no interaction)
**********************************************

use "cwdata.dta", clear
relogit prio aidshock11 aidshock11pos lagaidpergdp lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, cl(countrynum)


************************************
** Alternative Definitions of Treatment
************************************
use "cwdata.dta", clear

** Use just last year's changes to code aidshock (33)
relogit prio aidshock33 lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, cl(countrynum)

** Using just lagged aid change (continuous)
relogit prio lagaidgdpchange lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, cl(countrynum)

** Variations on the aidshock variable
use "cwdata.dta", clear

** with 25 pctile cutoff, 2 year smooth
relogit prio aidshock22 lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, cl(countrynum)
relogit prio aidshock22 aidshock22pos lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, cl(countrynum)
** with 10 pctile cutoff
relogit prio aidshock44 lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, cl(countrynum)
relogit prio aidshock44 aidshock44pos lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, cl(countrynum)
** with 20 pctile cutoff
relogit prio aidshock55 lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, cl(countrynum)
relogit prio aidshock55 aidshock55pos lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, cl(countrynum)

** 5 year average of aid shocks
relogit prio smaidsh lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, cl(countrynum)

************************
** systematic variations on the aidshock variable
************************

use "cwdata.dta", clear
relogit prio aidshock11 lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, cl(countrynum)

matrix A = [0,0]

foreach NUM of numlist 5/50 {
  relogit prio aidshockvar`NUM' lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, cl(countrynum)
  mat b = e(b)
  local b = b[1,1]
  mat v = e(V)
  local v = v[1,1]
  matrix A =A\ [`b', `v']
  /*set seed 12345
  setx  (aidshockvar`NUM') 0 (lln_rgdpc ln_population instab ethfrac relfrac logmtn lpolity2  _spline1 _spline2 _spline3) mean  (oil ncontig ColdWar) median 
  relogitq, fd(pr) listx changex(aidshockvar`NUM' 0 1)
  matrix A =A\ [r(dPrU_1), r(dPrL_1),  r(dPr_1)]
  */
}

svmat A
keep A*
outsheet using "output/shockCIs.csv", comma replace

/*
## Make the plot in R
## Set working directory to the location of the data

## then set the path we need from this path
currentpath <- getwd()
setwd(paste(currentpath,"/output",sep=""))

pdf(file="ShockCutoffsFig.pdf",width = 6.5, height = 5)
#jpeg(filename = "ShockCutoffsFig.jpg", width = 6.5, height = 5, res=200,
#     units = "in")
dat <- read.csv("shockCIs.csv")

dat <- dat[-1,]
dat <- na.omit(dat)

x <- as.numeric(rownames(dat))+3

y <- dat[,1]
L <- y - 1.96*sqrt(dat[,2])
U <- y + 1.96*sqrt(dat[,2])

plot(x,y, type="l", ylim=c(-1,2),
   xlim=c(5,35),
   axes=F,
   lwd=2,
   xlab="Percentile Cutoff for Aid Shocks", ylab="Estimated Aid Shock Coefficient",
   main="The Estimated Effect of Aid Shocks with Different Cut-offs")
#points(x,y, pch=20)
abline(h=0, col="gray50", lwd=2)
lines(x,L,lty=2)
lines(x,U,lty=2)

#abline(v=10, lty=3, col="gray")
#abline(v=15, lty=3, col="gray")
#abline(v=20, lty=3, col="gray")
#abline(v=30, lty=3, col="gray")
#abline(v=40, lty=3, col="gray")

axis(1,at=seq(0,35,5),labels=T)
#axis(2,at=seq(-.01,.04,.01), labels=T)
axis(2,at=seq(-1,2,1), labels=T)
#text(x=14, y=.015, "Actual Cutoff", srt=90)

dev.off()

## The reason this plot is choppy after about 37 is that at this point the value of the aid shock at the 37th percentile is very small
##   At the 40th percentile, the value of "smaidch" is 0, meaning that there are a lot of 0s in the dataset.
##   I should look at the distribution of this more carefully, but that's what's going on.

## reset the working directory
setwd(currentpath)

*/

** get rid of the junk file
erase "output/shockCIs.csv"


************************
** systematic variations on the POSITIVE aidshock variable
************************

use "cwdata.dta", clear
relogit prio aidshock11 lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, cl(countrynum)


*matrix A = [0,0,0]
matrix A = [0,0]

foreach NUM of numlist 50/95 {
  relogit prio aidshockposvar`NUM' aidshock11 lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, cl(countrynum)
  mat b = e(b)
  local b = b[1,1]
  mat v = e(V)
  local v = v[1,1]
  matrix A =A\ [`b', `v']
  /*set seed 12345
  setx  (aidshockvar`NUM') 0 (lln_rgdpc ln_population instab ethfrac relfrac logmtn lpolity2  _spline1 _spline2 _spline3) mean  (oil ncontig ColdWar) median 
  relogitq, fd(pr) listx changex(aidshockvar`NUM' 0 1)
  matrix A =A\ [r(dPrU_1), r(dPrL_1),  r(dPr_1)]
  */
}

svmat A
keep A*
outsheet using "output/posshockCIs.csv", comma replace

/*
## Make the plot in R
## Set working directory to the location of the data

## then set the path we need from this path
currentpath <- getwd()
setwd(paste(currentpath,"/output",sep=""))

jpeg(filename = "PosShockCutoffsFig.jpg", width = 6.5, height = 5, res=200,
     units = "in")
dat <- read.csv("posshockCIs.csv")

dat <- dat[-1,]
dat <- na.omit(dat)

x <- as.numeric(rownames(dat))+3 + 45

y <- dat[,1]
L <- y - 1.96*sqrt(dat[,2])
U <- y + 1.96*sqrt(dat[,2])

plot(x,y, type="l", ylim=c(-1,2),
   xlim=c(65,100),
   axes=F,
   lwd=2,
   xlab="Percentile Cutoff for Positive Aid Shocks", ylab="Positive Aid Shock Coefficient",
   main="The Estimated Effect of Positive Aid Shocks
 with Different Cut-offs")
#points(x,y, pch=20)
abline(h=0, col="gray50", lwd=2)
lines(x,L,lty=2)
lines(x,U,lty=2)

#abline(v=10, lty=3, col="gray")
#abline(v=15, lty=3, col="gray")
#abline(v=20, lty=3, col="gray")
#abline(v=30, lty=3, col="gray")
#abline(v=40, lty=3, col="gray")

axis(1,at=seq(65,100,5),labels=T)
#axis(2,at=seq(-.01,.04,.01), labels=T)
axis(2,at=seq(-1,2,1), labels=T)
#text(x=14, y=.015, "Actual Cutoff", srt=90)

dev.off()

## The reason this plot is choppy after about 37 is that at this point the value of the aid shock at the 37th percentile is very small
##   At the 40th percentile, the value of "smaidch" is 0, meaning that there are a lot of 0s in the dataset.
##   I should look at the distribution of this more carefully, but that's what's going on.

## reset the working directory
setwd(currentpath)

*/

erase "output/posshockCIs.csv"

******************************************
** With a panel bootstrap
******************************************

   ** get the point estimates
use "cwdata.dta", clear
relogit prio aidshock11 aidshock11pos lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, cl(countrynum)
mat truebetas = e(b)

set matsize 800
set seed 12345
matrix betas = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
local boot = 0
foreach NUM of numlist 1/500 {
  use "cwdata.dta", clear
  bsample , cluster(countrynum) idcluster(newcountrynum)
  capture relogit prio aidshock11 aidshock11pos lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, cl(newcountrynum)
  if _rc > 0 {
    continue
  }
  capture relogit prio aidshock11 aidshock11pos lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, cl(newcountrynum)
  local boot = `boot' + 1
  matrix betas = betas\e(b) 
  di `NUM' " tries so far"
  di `boot' " bootstraps so far"
}

svmat betas
keep betas*
drop in 1

matrix ciL = [0]
matrix ciU = [0]
foreach NUM of numlist 1/26 {
  _pctile betas`NUM', p(2.5)
  mat ciL = ciL\r(r1)
  _pctile betas`NUM', p(97.5)
  mat ciU = ciU\r(r1)
}

mat ciL = ciL[2..27,1]
mat ciU = ciU[2..27,1]
mat bootresult = truebetas',ciL,ciU
mat li bootresult


********************************************
** Trying Signorino and Carter's t, t2, t3
********************************************

  ** This is part of making the table of results
capture erase "output/extramodels1.txt"
capture erase "output/extramodels1.rtf"


use "cwdata.dta", clear
egen t = group(year)
gen t2 = t^2
gen t3 = t^3
relogit prio aidshock11 aidshock11pos lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar t t2 t3, cl(countrynum)
outreg2 using "output/extramodels1", word append

*******************************************
** Try leaving out 1991-1996
*******************************************
use "cwdata.dta", clear
  ** 1991-1996
relogit prio aidshock11 aidshock11pos lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3 if year<1991 | year>1996, cl(countrynum)
outreg2 using "output/extramodels1", word append
  ** 1991-2000
relogit prio aidshock11 aidshock11pos lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3 if year<1991 | year>2000, cl(countrynum)



********************************************
** With the other DVs
********************************************
  ** This is part of making the table of results
capture erase "output/otherDVmodels.txt"
capture erase "output/otherDVmodels.rtf"


  ** subsequent conflict years as 0's
use "cwdata.dta", clear
drop _spline* peaceyrs

relogit prio2 aidshock11 aidshock11pos lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar , cl(countrynum)

btscs prio2 year countrynum  if e(sample)==1, g(peaceyrs) nspline(3)
tsset countrynum year
relogit prio2 aidshock11 aidshock11pos lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar  _spline1 _spline2 _spline3, cl(countrynum)
outreg2 using "output/otherDVmodels", word append

  ** only the first war after 1980 counted, zeros after the first war starts
use "cwdata.dta", clear
relogit firstwar aidshock11 aidshock11pos lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, cl(countrynum)
outreg2 using "output/otherDVmodels", word append

  ** only the type 3 (domestic) conflicts
use "cwdata.dta", clear
relogit priotype3 aidshock11 aidshock11pos lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, cl(countrynum)
  ** to see which conflict onsets this leaves out:
gen diff = prio-priotype3
*edit countryname year prio priotype3 if diff!=0
outreg2 using "output/otherDVmodels", word append

  ** only the 1000 death conflicts WITH ONSET BEING THE TIME THEY REACH 1000 DEATHS
use "cwdata.dta", clear
relogit prio1000 aidshock11 aidshock11pos lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, cl(countrynum)
  ** of course, there are very few onsets with 1000 deaths
sum prio1000 if e(sample)==1
di r(N)*r(mean)
  ** whereas there are many more with 25 deaths
sum prio if e(sample)==1
di r(N)*r(mean)

  ** looking at the onset of conflicts that EVENTUALLY reached 1000 battle deaths
use "cwdata.dta", clear
relogit prio1k aidshock11 aidshock11pos lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, cl(countrynum)
outreg2 using "output/otherDVmodels", word append
  ** of course, there are fewer onsets with 1000 deaths
sum prio1k if e(sample)==1
di r(N)*r(mean)
  ** whereas there are many more with 25 deaths
sum prio if e(sample)==1
di r(N)*r(mean)
  
  ** if we exclude the 1000 death wars, we get good results
gen prio25only = prio
replace prio25only=0 if prio1000==1 & prio==1
relogit prio25only aidshock11 aidshock11pos lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, cl(countrynum)
 
capture drop prio25only
gen prio25only = prio
replace prio25only=0 if prio1k==1 & prio==1
relogit prio25only aidshock11 aidshock11pos lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, cl(countrynum)
 


****************************************
** Multiple Imputation for the main models
****************************************

** I use all obs for which we have the dependent variable
use cwdata, clear

relogit prio aidshock11 aidshock11pos lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar _spline1 _spline2 _spline3, cl(countrynum)
gen inmymodel = e(sample)
sum year if e(sample)==1
keep countryname year inmymodel aidshock11 _spline*
save "crap/sample data.dta", replace

use cwdata, clear

merge countryname year using "crap/sample data.dta", unique sort

keep if prio!=.

run "data/Standardize Country Names.master do file.RHR.08.28.07.do"
keep if  stateinyeart_g==1
  ** I use "lagaidgdpchange" and leave out the splines on purpose
  ** We calculate them after the imputation.
keep countrynum countryname year inmymodel prio lagaidgdpchange lagaidgdpchange lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar
move countrynum countryname
drop if year<1981 | year>2005
di _N
sum inmymodel if inmymodel==1
  ** This is the number of missing observations
di _N - r(N)

drop inmymodel
save "crap/data for impute.dta", replace


	********************
	** R code to do multiple imputation
	********************
** Run the following code in R
/*

## Set working directory to the location of the main dat

## Set the path we need from that path
currentpath <- getwd()
setwd(paste(currentpath,"/crap",sep=""))

library(Amelia)

dat<-read.dta("data for impute.dta")

rbind(col(dat)[1,],head(dat))
set.seed(12345)
#out <- amelia(dat, ts=1, cs=2, idvars=3, polytime=2, intercs=F)
out <- amelia(dat, ts=1, cs=2, idvars=3, polytime=3, intercs=F)


write.amelia(out, "outdata")

## reset the working directory
setwd(currentpath)

*/

	*************
	** End R code
	*************

** After the imputation in R...

** data set-up
clear
forvalues i=1/5 {
insheet using "crap/outdata`i'.csv"

btscs prio year countrynum, g(peaceyrs) nspline(3)
tsset countrynum year
gen inmysample=1

tssmooth ma smaidch=lagaidgdpchange, window(1 1 0)
tssmooth ma sm2aidch=lagaidgdpchange, window(2 1 0)
tssmooth ma sm3aidch=lagaidgdpchange, window(3 1 0)
tssmooth ma sm4aidch=lagaidgdpchange, window(4 1 0)

*tssmooth ma smaidde= lagaidpergdp, window(1 1 0)
*tssmooth ma sm4aidde= lagaidpergdp, window(4 1 0)

** To drop the crappy aid data years prior to 1975
drop if year<1975

gen negaid=smaidch if smaidch<0
replace negaid=0 if negaid==.

_pctile smaidch if inmysample==1, p(15)
local shock=r(r1)
gen aidshock11 = 1 if smaidch<=`shock'
replace aidshock11 = 0 if aidshock11==.

_pctile smaidch if inmysample==1, p(25)
local shock=r(r1)
gen aidshock22 = 1 if smaidch<=`shock'
replace aidshock22 = 0 if aidshock22==.

_pctile lagaidgdpchange if inmysample==1, p(15)
local shock=r(r1)
gen aidshock33 = 1 if lagaidgdpchange <=`shock'
replace aidshock33 = 0 if aidshock33==.
tssmooth ma smaidsh = aidshock33, window(4 1 0)

_pctile smaidch if inmysample==1, p(10)
local shock=r(r1)
gen aidshock44 = 1 if smaidch<=`shock'
replace aidshock44 = 0 if aidshock44==.

_pctile smaidch if inmysample==1, p(20)
local shock=r(r1)
gen aidshock55 = 1 if smaidch<=`shock'
replace aidshock55 = 0 if aidshock55==.

_pctile smaidch if inmysample==1, p(85)
local shock=r(r1)
gen aidshock11pos = 1 if smaidch>=`shock'
replace aidshock11pos = 0 if aidshock11pos==.

save crap/MI`i', replace
clear
}
**************

  ** Put all the imputed datasets together
use crap/MI1
gen imputedset=1
append using crap/MI2
replace imputedset=2 if imputedset==.
append using crap/MI3
replace imputedset=3 if imputedset==.
append using crap/MI4
replace imputedset=4 if imputedset==.
append using crap/MI5
replace imputedset=5 if imputedset==.

***********************
** Imputed models
***********************

** The model for aidshock11
forvalues i=1/5 {
capture preserve
keep if imputedset==`i'
relogit prio aidshock11 aidshock11pos lptsave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn coldwar  _spline1 _spline2 _spline3, cl(countrynum)
matrix beta`i'=e(b)
matrix se`i'=vecdiag(cholesky(e(V)))
restore
}

matrix ibeta=(beta1+beta2+beta3+beta4+beta5)/5
matrix between=(1/(5-1))*( vecdiag(diag(beta1-ibeta)*diag(beta1-ibeta))+vecdiag(diag(beta2-ibeta)*diag(beta2-ibeta))+vecdiag(diag(beta3-ibeta)*diag(beta3-ibeta))+vecdiag(diag(beta4-ibeta)*diag(beta4-ibeta))+vecdiag(diag(beta5-ibeta)*diag(beta5-ibeta)) )
matrix ise=((se1+se2+se3+se4+se5)/5) + (1+(1/5))*between

matrix miresults = (ibeta\ise)

** results for model 3
matrix list miresults
  ** format the table better
matrix miresults2 = miresults'
mat li miresults2

/** in R, to calculate the p-value
zscore<-.636/.242
  ## p-value
2*(1-pnorm(zscore))
**********************/



************************************************
** To make a chart of aidshocks in R:
************************************************

** This will give you the numbers for below:

use cwdata, clear
keep if aidshock11==1
egen aidshockcheck=count(aidshock11),by(year)
keep year aidshockcheck
duplicates drop
sort year
*edit

save "crap/aid shocks over time.dta", replace


## In R
library(foreign)

## Set working directory to the main folder

currentpath <- getwd()
crappath <- paste(currentpath,"/crap",sep="")
outpath <- paste(currentpath,"/output",sep="")

setwd(crappath)
dat <- read.dta("aid shocks over time.dta")
year <- dat[,1]
aidshocks <- dat[,2]
setwd(outpath)
pdf(file = "AidShocksPerYear.pdf",width = 6.5, height = 4.5)
#jpeg(filename = "AidShocksPerYear.jpg", width = 6.5, height = 4.5, res=200,
#     units = "in")
plot(x=year, y=aidshocks, type="n", main="Aid Shocks per Year ", 
     xlab="Year", ylab="Number of Aid Shocks",
     ylim=c(0,50),
     axes=F)
abline(h=seq(0,50,10),lty=3, col="gray75")
#abline(h=0)
axis(1,at=seq(1975,2010,5),labels=T)
axis(2,at=seq(0,50,10), labels=T)
## Stretch out the plot
lines(cbind(year,aidshocks), lwd=2)
##
#segments(x0=year,x1=year,y0=rep(0,length(aidshocks)), y1=aidshocks,
#         lwd=2, col="gray40", lend=2)
#lines(ksmooth(year,aidshocks, kernel="normal",bandwidth=3),lty=2)
dev.off()

## reset the working directory
setwd(currentpath)

## END OF ALL CODE


