modelDependence <-
function(dataset, treatment, verbose = TRUE, seed = 1, num.binnings, cutpoints = NA){
    set.seet(seed)
    for(i in 1:num.binnings){
        # should export a #bins x #obs matrix (see cut() + outer())

        # model dependence in bins with common support is width of bins,
        # model dependence in bins without common support is num obs
        dummy.mat <- # sample from possible bins and asign dummies

        
    }

    
}
