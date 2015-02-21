getCutpoint <-
function(dataset, base.form, cov){
    base.mod <- lm(base.form, data = dataset)
    mod.form <- as.formula(paste(as.character(base.form[2]),
                                 as.character(base.form[1]),
                                 cov))
    print(cov)
    print(base.mod); print(mod.form[c(1,3)]); print(mean(dataset[[cov]]))
    seg.reg <- segmented(base.mod, seg.Z=mod.form[c(1,3)], psi = mean(dataset[[cov]]))
    print(seg.reg)
    cutpoint <- seg.reg$psi[2]
#    cutpoint <- mean(dataset[[cov]])
    return(cutpoint)
}
