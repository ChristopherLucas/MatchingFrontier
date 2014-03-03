L1 <- function(strataholder){
  L1 <- 0
  num.treated <- 0
  num.control <- 0
  for(strat in strataholder){
    num.treated <- num.treated + sum((names(strat) == 1))
    num.control <- num.control + sum((names(strat) == 0))
  }
  for(strat in strataholder){
      strat.imb <- (sum(names(strat) == 1))/num.treated - (sum(names(strat) == 0))/num.control
      L1 <- L1 + abs(strat.imb)
  }
  return(L1 * .5)
}
