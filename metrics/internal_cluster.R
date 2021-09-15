# Calinski-Harabasz index (CH) ####
clusEval_calinskiHarabasz = function(tsdata, clusters) {
    clusEval.argchecks(tsdata, clusters)
    if(anyNA(clusters)) { return(NaN) }

    X = tsdata_matrix(tsdata)
    clusterCrit::intCriteria(X, as.integer(clusters), crit='Calinski_Harabasz')[[1]]
}

# Davies–Bouldin (DB) ####
clusEval_daviesBouldin = function(tsdata, clusters) {
    clusEval.argchecks(tsdata, clusters)
    if(anyNA(clusters)) { return(NaN) }

    X = tsdata_matrix(tsdata)
    clusterCrit::intCriteria(X, as.integer(clusters), crit='Davies_Bouldin')[[1]]
}

# Dunn index (DI) ####
clusEval_dunn = function(tsdata, clusters) {
    clusEval.argchecks(tsdata, clusters)
    if(anyNA(clusters)) { return(NaN) }

    X = tsdata_matrix(tsdata)
    clusterCrit::intCriteria(X, as.integer(clusters), crit='Dunn')[[1]]
}

# Log-SS ratio ####
clusEval_logSSRatio = function(tsdata, clusters) {
    clusEval.argchecks(tsdata, clusters)
    if(anyNA(clusters)) { return(NaN) }

    X = tsdata_matrix(tsdata)
    clusterCrit::intCriteria(X, as.integer(clusters), crit='Log_SS_Ratio')[[1]]
}

# Ray-Turi (RT) ####
clusEval_rayTuri = function(tsdata, clusters) {
    clusEval.argchecks(tsdata, clusters)
    if(anyNA(clusters)) { return(NaN) }

    X = tsdata_matrix(tsdata)
    clusterCrit::intCriteria(X, as.integer(clusters), crit='Ray_Turi')[[1]]
}

# Silhouette index (SI) ####
clusEval_silhouette = function(tsdata, clusters) {
    clusEval.argchecks(tsdata, clusters)
    if(anyNA(clusters)) { return(NaN) }

    X = tsdata_matrix(tsdata)
    clusterCrit::intCriteria(X, as.integer(clusters), crit='Silhouette')$silhouette
}

clusEval.argchecks = function(tsdata, clusters) {
    stopifnot(length(clusters) == uniqueN(tsdata$Id))
}
