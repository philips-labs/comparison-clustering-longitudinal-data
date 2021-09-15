longclus_lm_kmeans = function(tsdata, numclus, numruns=1,
                               model=Value~Time,
                               trends='lm', #method used for computing the trends
                               store='all',
                               seed=NULL) {
    set.seed(seed)
    ids = unique(tsdata$Id)
    start = Sys.time()
    xtime = unique(tsdata$Time)
    dt_time = data.table(Time=xtime)
    
    lmObjects = lapply(levels(tsdata$Id), function(id) {
        lm(model, data=tsdata[Id == id])
    })
    
    coefs_traj = cbind(Id=ids, sapply(lmObjects, '[[', 'coefficients') %>% t) %>% as.data.table
    pred_traj = data.table(tsdata[, .(Id, Time)], Pred=lapply(lmObjects, predict) %>% unlist)
    
    cld = clusterLongData(traj=as.matrix(coefs_traj[, -'Id']), idAll=ids, time=seq_len(length(coefs_traj)-1), varNames='Value')
    par = parALGO(saveFreq=1e99, scale=TRUE, startingCond='kmeans++')
    kml(cld, nbClusters=numclus, nbRedrawing=numruns, toPlot='none', parAlgo=par)
    
    modk = slot(cld, paste0('c', numclus))[[1]]
    clusters = getClusters(cld, numclus)
    clusnames = levels(clusters)
    coefs_traj[, Cluster := clusters]
    pred_traj[, Cluster := clusters[Id]]
    
    postProbs = modk@postProba
    colnames(postProbs) = levels(clusters)
    
    # compute centers
    centers = coefs_traj[, lapply(.SD, mean), keyby=Cluster, .SDcols=setdiff(colnames(coefs_traj), c('Id', 'Cluster'))]
    
    # compute trends
    if(trends == 'lm') {
        dt_trends = pred_traj[, .(Value=mean(Pred)), by=.(Cluster, Time)]
    } else {
        xdata = copy(tsdata)
        xdata[, Cluster := clusters[.GRP], by=Id]
        dt_trends = xdata[, .(Value=mean(Value)), by=.(Cluster, Time)]
    }
    
    # store results
    result = result_summary(tsdata, clusters=clusters, dt_trends=dt_trends, start=start, postProbs=postProbs)
    result$BIC = modk@criterionValues['BIC']
    result$converged = TRUE
    result$model$gcm = switch(store, all=lmObjects, core={list(coefs=coefs_traj, pred=pred_traj)}, none=NULL)
    result$model$kmeans = switch(store, all=modk, core=modk, none=NULL)
    result$model$centers = centers
    return(result)
}