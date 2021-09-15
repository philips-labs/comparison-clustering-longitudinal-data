# @deprecated
trendSimilarity_wrss = function(tsdata, trends) {
    refTrends = tsdata_trends(tsdata)
    refClusters = tsdata_groups(tsdata)
    trendSimilarity_wmsse(trends, refTrends, refClusters)
}

# Weighted minimum sum of squared errors (WMSSE) ####
trendSimilarity_wmsse = function(trends, refTrends, refClusters) {
    if(is.null(refTrends) || anyNA(refClusters)) {
        return(NaN)
    }
    stopifnot(is.data.table(trends))
    stopifnot(is.data.table(refTrends))
    refTrends = tsdata_standardize(refTrends, group=CLUSTER)

    trendMat = dcast(trends, Cluster ~ Time, value.var='Value') %>% .[,-'Cluster'] %>% as.matrix
    refMat = dcast(refTrends, Group ~ Time, value.var='Value') %>% .[,-'Group'] %>% as.matrix
    refProp = table(refClusters) / length(refClusters)
    wmsse = foreach(clus=1:nrow(refMat), .combine=sum) %do% {
        refVec = refMat[clus,]
        refProp[clus] * min(rowSums(sweep(trendMat, 2, refVec)^2))
    }
    return(wmsse)
}

# Weighted minimum mean of squared errors (WMMSE) ####
trendSimilarity_wmmse = function(trends, refTrends, refClusters) {
    numobs = uniqueN(trends$Time)
    trendSimilarity_wmsse(trends, refTrends, refClusters) / numobs
}
