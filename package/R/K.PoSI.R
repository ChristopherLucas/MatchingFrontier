K.PoSI <- function(PoSI.object, confidence=c(.95,.99), alpha=NULL, df.err=NULL, eps.PoSI=1E-6, digits=3)
{
    if(class(PoSI.object) != "PoSI") { cat("K.PoSI: first argument is not of class 'PoSI'.\n");  return }
    rU <- 1/PoSI.object
    df <- attributes(PoSI.object)$d
    if(is.null(alpha)) { alpha <- 1-confidence } else { confidence <- 1-alpha }
    if(is.null(df.err)) { # sigma known  ==>  z-statistics
        prob.PoSI  <- function(K) mean(pchisq((K*rU)^2, df))
    } else { # sigma estimated with df.err  ==>  t-statistics
        prob.PoSI  <- function(K) mean(pf((K*rU)^2/df, df, df.err)) 
    }
    # Bisection search for K.PoSI:
    K.PoSI <- sapply(confidence,
                     function(cvg) {
                         K.lo <- 0
                         K.hi <- 30
                         while(abs(K.lo - K.hi) > eps.PoSI) {
                             K <- (K.lo + K.hi)/2
                             if(mean(prob.PoSI(K)) > cvg) {
                                 K.hi <- K
                             } else {
                                 K.lo <- K
                             } }
                         K }
                     )
    names(K.PoSI) <- confidence
    round(K.PoSI, digits)
} # end of 'K.PoSI <- function(...'
# Generic function:
K <- function(object, ...) { UseMethod("K") }
