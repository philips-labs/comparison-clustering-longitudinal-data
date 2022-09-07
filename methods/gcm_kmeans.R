longclus_gcm_kmeans = function(tsdata, numclus, numruns=1,
                               model.fixed=Value~Time, #fixed effects
                               model.random=~1+Time, #random effects
                               model.diagcov=TRUE, #diagonal covariance matrix
                               trends='gcm', #method used for computing the trends
                               standardize=FALSE, #standardize the parameters prior to clustering #TODO set to true
                               store='all',
                               seed=NULL) {
    tsdata = as.data.table(tsdata)
    set.seed(seed)
    ids = unique(tsdata$Id)
    start = Sys.time()
    xtime = unique(tsdata$Time)
    dt_time = data.table(Time=xtime)

    # need to substitute model.fixed and model.random otherwise predictY throws errors
    gcm = eval(substitute(hlme(fixed=model.fixed, random=model.random, subject='Id', ng=1, idiag=model.diagcov, data=tsdata)))
    stopifnot(gcm$pred$Id == as.integer(tsdata$Id)) #ensure proper order assumption for later code

    if(standardize) {
        R = ranef(gcm) %>% scale
    } else {
        R = ranef(gcm)
    }
    capture.output(gcmsum <- summary(gcm)) %>% invisible #suppress printouts
    coef_fe = t(gcmsum) %>% as.data.table %>% .[1,] %>%
        setnames(paste0('fixed_', names(.)))
    coef_re = data.table(Id=ids, ranef(gcm))
    coefs_traj = cbind(Id=ids, coef_fe[rep(1, nrow(coef_re)), ], coef_re[,-'Id'])
    pred_traj = data.table(tsdata[, .(Id, Time)], Pred=gcm$pred$pred_ss)

    cld = clusterLongData(traj=R, idAll=ids, time=seq_len(ncol(R)), varNames='Value')
    par = parALGO(saveFreq=1e99, scale=TRUE, startingCond='kmeans++')
    kml(cld, nbClusters=numclus, nbRedrawing=numruns, toPlot='none', parAlgo=par)

    modk = slot(cld, paste0('c', numclus))[[1]]
    clusters = getClusters(cld, numclus)
    coefs_traj[, Cluster := clusters]
    pred_traj[, Cluster := clusters[Id]]

    postProbs = modk@postProba
    colnames(postProbs) = levels(clusters)

    # compute centers
    centers = coefs_traj[, lapply(.SD, mean), keyby=Cluster, .SDcols=setdiff(colnames(coefs_traj), c('Id', 'Cluster'))]

    # compute trends
    dt_trends = pred_traj[, .(Value=mean(Pred)), by=.(Cluster, Time)]

    # store results
    result = result_summary(tsdata, clusters=clusters, dt_trends=dt_trends, start=start, postProbs=postProbs, numclus=numclus)
    result$BIC = modk@criterionValues['BIC']
    result$AIC = modk@criterionValues[['AIC']]
    result$converged = gcm$conv == 1
    result$model$gcm = switch(store, all=gcm, core={gcm_clean = gcm; gcm_clean[c('pred', 'predRE', 'pprob')] = NULL; gcm_clean}, none=NULL)
    result$model$kmeans = switch(store, all=modk, core=modk, none=NULL)
    result$model$centers = centers

    return(result)
}
