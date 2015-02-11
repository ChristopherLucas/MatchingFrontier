getCutpoint <-
function(dataset, base.form, cov){
    base.mod <- lm(base.form, data = dataset)
    mod.form <- as.formula(paste(as.character(base.form[2]),
                                 as.character(base.form[1]),
                                 cov))
    seg.reg <- segmented(base.mod, seg.Z=mod.form[c(1,3)], psi = median(dataset[[cov]]))            
    cutpoint <- seg.reg$psi[2]
    return(cutpoint)
}
