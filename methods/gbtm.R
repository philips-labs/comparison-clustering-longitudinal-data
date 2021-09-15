# 
longclus_gbtm = function(tsdata, numclus, numruns, maxiter, 
                         model.fixed=Value~Time, 
                         model.mixture=~Time, 
                         store='all', ...) {
    
    result = longclus_gmm(tsdata, numclus=numclus, numruns=numruns,
                          model.fixed=model.fixed,
                          model.random=~-1,
                          model.mixture=model.mixture,
                          maxiter=maxiter,
                          store=store, ...)
    result$R2c = NULL
    return(result)
}