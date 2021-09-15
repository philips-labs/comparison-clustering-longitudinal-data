longclus_mixtvem = function(tsdata, numclus, numruns=50,
                            model.numknots=6,
                            model.degree=3,
                            model.autocor=FALSE,
                            store='all',
                            seed=NULL,
                            value=VALUE,
                            ...) {
    set.seed(seed)
    xdata = tsdata_standardize(tsdata, value=value)
    xdata[, Intercept := 1]

    start = Sys.time()
    models = tryCatch({
        TVEMMixNormal(
            dep=xdata$Value,
            id=xdata$Id,
            time=xdata$Time,
            tcov=xdata$Intercept,
            numClasses=numclus,
            numStarts=numruns,
            numInteriorKnots=model.numknots, deg=model.degree,
            assumeIndependence=!model.autocor,
            getSEs=FALSE,
            doPlot=FALSE,
            ...)
    }, error=function(e) {
        print(e)
        result = list()
        result$time = as.numeric(Sys.time() - start, 'secs')
        result$BIC
        result$converged = FALSE
        result$model = NULL
        return(result)
    })

    if(is.null(models$bestFit)) {
        #an error occurred, return the results
        return(models)
    }

    model = models$bestFit
    clusnames = LETTERS[1:numclus]
    clusters = factor(apply(model$postProbsBySub, 1, which.max), levels=1:numclus, labels=clusnames)
    postProbs = model$postProbsBySub
    colnames(postProbs) = clusnames

    # Compute trends
    stopifnot(nrow(model$fittedY) == nrow(xdata))
    dt_trajmarg = data.table(model$fittedY, Id=xdata$Id, Time=xdata$Time) %>%
        melt(id=c('Id', 'Time'), variable.name='Cluster', value.name='Value')
    dt_trends = unique(dt_trajmarg[, -'Id'], by=c('Time', 'Cluster')) %>%
        .[, Cluster := factor(as.integer(Cluster), levels=1:numclus, labels=clusnames)] %>%
        setcolorder(c('Cluster', 'Time', 'Value'))

    # Results
    result = result_summary(xdata, clusters=clusters, dt_trends=dt_trends, start=start, postProbs=postProbs, numclus=numclus)
    result$BIC = model$bic
    result$converged = model$converged
    result$model = switch(store, all=model,
                          core={modc = model; modc[c('residsY', 'postProbsByAssessment', 'fittedY', 'intId', 'dep', 'postProbsBySub', 'fittedProb')] = NULL; modc},
                          none=NULL)
    return(result)
}
