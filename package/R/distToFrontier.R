distToFrontier <-
function(distance.mat){
    cat("Calculating theoretical frontier...\n")
    row.mins <- apply(distance.mat, 1, function(x) min(x))
    col.mins <- apply(distance.mat, 2, function(x) min(x))
    row.mins.inds <- apply(distance.mat, 1, function(x) as.integer(names(which.min(x))))
    col.mins.inds <- apply(distance.mat, 2, function(x) as.integer(names(which.min(x))))
    matched.to <- c(row.mins.inds, col.mins.inds)[order(as.integer(names(c(row.mins.inds, col.mins.inds))))]
    minimums <- c(row.mins, col.mins)
    sorted.minimums <- sort(unique(c(row.mins, col.mins)), decreasing = TRUE)
    drop.order <- lapply(sorted.minimums, function(x) as.integer(names(minimums[minimums == x])))
    cat("Calculating information for plotting the frontier...\n")
    weighted.vals <- unlist(lapply(drop.order, function(x) length(x))) * sorted.minimums
    Xs <- cumsum(rev(lapply(drop.order, function(x) length(x))))
    Ys <- cumsum(rev(weighted.vals)) / Xs
    return(list(drop.order = drop.order, Xs = Xs, Ys = Ys, matched.to = matched.to))
}
