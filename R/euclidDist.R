euclidDist <-
function(Tnms, Cnms, dataset, covars) {
    Ts <- as.matrix(dataset[Tnms, covars])
    Cs <- as.matrix(dataset[Cnms, covars])
    d <- matrix(NA, nrow(Ts), nrow(Cs))
    for (t in 1:nrow(Ts)) for (c in 1:nrow(Cs)) d[t,c] <- sqrt(sum((Ts[t,]-Cs[c,])^2))
    return(d)
}
