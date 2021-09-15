result_summary = function(tsdata, clusters, dt_trends, start, postProbs, numclus) {
    stopifnot(uniqueN(tsdata$Id) == length(clusters))
    stopifnot(!anyNA(clusters))
    stopifnot(length(clusters) == nrow(postProbs))
    stopifnot(nrow(dt_trends) == numclus * uniqueN(tsdata$Time))
    N = length(clusters)
    ref_clusters = tsdata_groups(tsdata)
    ref_trends = tsdata_trends(tsdata)

    result = list()
    result$start = start
    result$time = as.numeric(Sys.time() - start, 'secs')
    result$clusters = clusters
    result$trends = dt_trends

    # Reference metrics
    result$refClusters = ref_clusters
    result$refDunn = clusEval_dunn(tsdata, ref_clusters)
    result$refSilhouette = clusEval_silhouette(tsdata, ref_clusters)
    result$trendWRSS = trendSimilarity_wmsse(dt_trends, ref_trends, ref_clusters) #deprecated
    result$trendWMSSE = result$trendWRSS
    result$trendWMMSE = trendSimilarity_wmmse(dt_trends, ref_trends, ref_clusters)

    # Label similarities
    result$ARI = labelSimilarity_adjustedRand(clusters, ref_clusters)
    result$KL = labelSimilarity_kl(clusters, ref_clusters)
    # result$Mirkin = labelSimilarity_mirkin(clusters, ref_clusters)
    sj = labelSimilarity_splitJoin(clusters, ref_clusters)
    result$sj1 = sj[1]
    result$sj2 = sj[2]
    result$sjMin = min(sj)
    result$sjTotal = sum(sj)
    result$NSJ = sum(sj) / 2 / N
    result$VI = labelSimilarity_vi(clusters, ref_clusters)

    # Trend evaluation
    result$MAE = trendEval_mae(tsdata, dt_trends, clusters)
    result$MSE = trendEval_mse(tsdata, dt_trends, clusters)
    result$WMAE = trendEval_wmae(tsdata, dt_trends, postProbs)
    result$WMSE = trendEval_wmse(tsdata, dt_trends, postProbs)
    result$WRSS = trendEval_wrss(tsdata, dt_trends, postProbs)

    # Cluster evaluation
    result$CalinskiHarabasz = clusEval_calinskiHarabasz(tsdata, clusters)
    result$DaviesBouldin = clusEval_daviesBouldin(tsdata, clusters)
    result$silhouette = clusEval_silhouette(tsdata, clusters)

    # Other metrics
    result$emptyClusters = numclus - uniqueN(clusters)
    result$loneClusters = sum(table(clusters) == 1)
    return(result)
}
