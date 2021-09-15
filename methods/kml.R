longclus_kml = function(tsdata, numclus, numruns=1, store='all', seed=NULL, value=VALUE) {
    set.seed(seed)
    xdata = tsdata_standardize(tsdata, value=value)
    tsmat = tsdata_matrix(xdata)

    stopifnot(!anyNA(tsmat))

    start = Sys.time()
    cld = clusterLongData(traj=tsmat, idAll=rownames(tsmat), time=colnames(tsmat) %>% as.numeric, varNames='Value')
    par = parALGO(saveFreq=1e99, scale=FALSE, startingCond='kmeans++')
    kml(cld, nbClusters=numclus, nbRedrawing=numruns, toPlot='none', parAlgo=par)

    clusters = getClusters(cld, numclus)

    model = slot(cld, paste0('c', numclus))[[1]]
    xdata[, Cluster := clusters[.GRP], by=Id]
    dt_trends = xdata[, .(Value=mean(Value)), by=.(Cluster, Time)]
    postProbs = model@postProba
    colnames(postProbs) = levels(clusters)

    result = result_summary(xdata, clusters=clusters, dt_trends=dt_trends, start=start, postProbs=postProbs, numclus=numclus)
    result$BIC = -model@criterionValues['BIC']
    
    # special case for k=1 since kml outputs NA
    if (is.na(result$BIC) && numclus == 1) {
        alldata = merge(xdata, dt_trends, by = c('Cluster', 'Time')) %>%
            setnames(c('Value.x', 'Value.y'), c('Obs', 'Mean'))
        assert_that(nrow(alldata) == nrow(xdata))
        sigma = alldata[, longitudinalData:::sdcNA(Obs - Mean)]
        ll = sum(dnorm(alldata$Obs, alldata$Mean, sigma, log = TRUE))
        np = model@nbClusters * uniqueN(xdata$Time) + 1
        result$BIC = -2 * ll + np * log(length(model@clusters))
    }
    
    result$AIC = model@criterionValues[['AIC']]
    result$model = switch(store, all=model, core=model, none=NULL)
    result$converged = TRUE #kmeans is guaranteed to converge

    return(result)
}
