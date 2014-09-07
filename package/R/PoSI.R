# Computations of inner products of random unit vectors (U)
# with 'linear contrasts', that is, adjusted predictor vectors (L):
PoSI <- function(X, Nsim=1000, bundleSZ=100000, modelsSZ=1:ncol(X),
                 eps=1E-8, center=T, scale=T, verbose=1)
{
    if(verbose>=1) { proc.time.init <- proc.time() }
    # Standardize the columns of X for numerical stability; if intercept is to be selected, set 'center=F':
    X.scaled <- scale(X, center=T, scale=T)
    # Remove columns with NAs:
    sel <- apply(X.scaled, 2, function(x) !any(is.na(x)))
    X.scaled <- X.scaled[,sel]  
    if(verbose >= 1 & any(!sel)) cat("Removed column(s)",which(!sel),"due to NAs after standardizing'\n")
    # Map X to Scheffe canonical coordinates X.cc:  X.cc' X.cc = X'X  (d x p)
    X.svd   <- svd(X.scaled)
    sel <- (X.svd$d > eps)
    if(verbose >= 1 & any(!sel)) cat("Reduced rank:",sum(sel),"  (Change eps?)\n")
    X.cc <- X.svd$d[sel] * t(X.svd$v[,sel]) # Full-model Scheffe canonical coords. of X.
    # Clean up:
    rm(X.scaled, X.svd)
    # Notation:
    p <- ncol(X.cc)                         # Number of predictors
    d <- nrow(X.cc)                         # d=rank(X), can be < p when X is rank-deficient, as when p>n
    # Submodels can only be of size <= d; drop those exceeding d:
    modelsSZ <- modelsSZ[modelsSZ <= d]     # Remove models that are too large.
    bundleSZ <- max(bundleSZ, max(modelsSZ)) # Make sure one bundle is able to contain at least one model.
    # Random unit vectors for which inner products with adjusted predictors (L) will be formed:
    U <- matrix(rnorm(d*Nsim),ncol=d)       # Nsim x d
    U <- U / sqrt(rowSums(U^2))             # Matrix of unit vectors u in R^d (=rows) for max_l |<u,l>|
    # Prepare loop over models to store all possible linear combinations l:
    UL.max <- rep(0,Nsim)                   # To contain  max_l |<u,l>|  for each random row u of U.
    L <- matrix(NA, nrow=bundleSZ, ncol=d)  # One bundle of adjusted predictors l (bundleSZ x d)
    Nmodels  <- 0                           # To be returned among attributes()
    Ntests   <- 0                           # To be returned among attributes()
    ibundles <- 0                           # For verbose>=1
    if(verbose>=1) {
        cat("Number of contrasts/adjusted predictors to process:",sum(choose(p,modelsSZ)*modelsSZ),"\n")
        cat("Number of bundles:",sum(ceiling(choose(p,modelsSZ)*modelsSZ/bundleSZ)),"\n")
    }
    for(m in modelsSZ) {                    # Loop over model sizes.
        i.store <- 1                        # Pointer into next free row of L (= d-vectors)
        M <- 1:m                            # Current submodel, initialized to first submodel
        M.last <- (p-m+1):p                 # The last submodel of size m
        repeat {                            # Loop over bundles of adjusted predictors
            # Verbosity level 2: print all models & dims
            if(verbose>=2) cat("Model:", M,"\n")
            X.m <- X.cc[,M]                 # Select predictors for this submodel
            X.qr <- qr(t(X.m)%*%X.m)        # QR decomposition of submodel in canon. coords.
            if(X.qr$rank < length(M)) next  # Rank-deficient submodel ==> omit.
            L[i.store:(i.store+m-1),] <- t(X.m %*% qr.solve(X.qr)) # Submodel adjusted predictors (rows of L)
            Nmodels <- Nmodels + 1
            Ntests  <- Ntests + m
            i.store <- i.store+m            # Move pointer to next free column of L.
            # If bundle L of adjusted predictors l is full, or if this was the last model, then
            # accumulate  max_l |<u,l>|  and re-initialize L:
            if(( (i.store+m-1) > bundleSZ ) | !(any(M < M.last) )) {
                L[1:(i.store-1),] <- L[1:(i.store-1),] /     # Normalize rows of L to unit length:
                                     sqrt(rowSums(L[1:(i.store-1),,drop=F]^2))
                # Implementation note: Against 'help()', '..%*%t(..)' is a touch faster than 'tcrossprod()'.
                UL <- abs(U %*% t(L[1:(i.store-1),,drop=F])) # Inner prods |<u,l>| of rows of U and L
                UL.max <- pmax(UL.max, apply(UL, 1, max))    # Accumulate  max_l |<u,l>|
                i.store <- 1                                 # Initialize L to fill up a new bundle
                # Verbosity level 1: print on bundle completion
                if(verbose>=1) {
                    ibundles <- ibundles + 1
                    cat("                         Done with bundle",ibundles,"   model sz =",m,"\n") 
                }
            }
            # Find next model in enumeration:
            if(any(M < M.last)) {            # Not at the last submodel yet, hence find the next one:
                i <- max(which(M < M.last))  # Pointer to the highest predictor index not in the last model
                M[i] <- M[i]+1   # Move this predictor index to point to the next predictor.
                if(i < m) { M[(i+1):m] <- (M[i]+1):(M[i]+m-i) } # Set the remaining to their minima.
            } else break
        } # end of 'repeat{'
    } # end of 'for(m in modelsSZ) {'
    if(verbose>=1) {
        cat("p =",p,", d =",d,"  processed",Ntests,"tests in",Nmodels,"models.  Times in seconds:\n")
        print(proc.time() - proc.time.init)
    }
    attributes(UL.max)$d <- d
    attributes(UL.max)$p <- p
    attributes(UL.max)$Nmodels <- Nmodels
    attributes(UL.max)$Ntests  <- Ntests
    class(UL.max) <- "PoSI"
    UL.max
} # end of 'PoSI <- function(...)'
