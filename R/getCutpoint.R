getCutpoint <-
function(dataset, base.form, cov, median = FALSE){
    if(!median){
        if(median(dataset[[cov]], na.rm = T) == min(dataset[[cov]], na.rm = T)){
            msg <- paste('the Athey-Imbens intervals cannot be calculated since the minimum and median of ', cov, ' are equal. Solutions: (1)set the argument `means.as.cutpoints` to be TRUE; or (2)set the argument `Athey.Imbens` to be FALSE.', sep = '')
            customStop(msg, 'estimateEffects()')
        } 
        mod.form <- as.formula(paste(as.character(as.formula(base.form)[2]),
                                     as.character(as.formula(base.form)[1]),
                                     cov))
        
        if(length(unique(dataset[[as.character(base.form[2])]])) == 2){
            base.mod <- glm(mod.form, data = dataset, family = 'binomial')
        } else{
            base.mod <- lm(mod.form, data = dataset)
        }
        
        seg.reg <- segmented(base.mod, seg.Z=mod.form[c(1,3)], psi = median(dataset[[cov]]), control = seg.control(it.max = 10000))
        cutpoint <- seg.reg$psi[2]
        print(cov)
        print(cutpoint)
    }
    else{
        cutpoint <- median(dataset[[cov]], na.rm = T)
    }
    return(cutpoint)
}
