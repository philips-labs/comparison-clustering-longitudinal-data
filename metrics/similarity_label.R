labelSimilarity_all = function(clus, refclus) {
    list(
        KL=similarity_kl(clus, refclus),
        Kappa=similarity_kappa(clus, refclus),
        Jaccard=similarity_jaccard(clus, refclus)
    )
}

# Adjusted Rand Index (ARI) ####
labelSimilarity_adjustedRand = function(clus, refclus) {
    if(anyNA(refclus)) {
        return(NaN)
    }
    mclust::adjustedRandIndex(clus, refclus)
}

# Jaccard index ####
labelSimilarity_jaccard = function(clus, refclus) {
    if(anyNA(refclus)) {
        return(NaN)
    }
    clusteval::cluster_similarity(clus %>% as.integer, refclus %>% as.integer, similarity='jaccard')
}

# Cohen's kappa ####
labelSimilarity_kappa = function(clus, refclus) {
    if(anyNA(refclus)) {
        return(NaN)
    }
    psych::cohen.kappa(x=cbind(clus %>% as.integer, refclus %>% as.integer))$kappa
}

# Kullback–Leibler divergence (KL) ####
labelSimilarity_kl = function(clus, refclus) {
    if(anyNA(refclus)) {
        return(NaN)
    }
    y = table(clus) %>% as.integer
    yref = table(refclus) %>% as.integer
    if(length(y) != length(yref)) {
        if(length(y) > length(yref)) {
            yref = c(yref, rep(0, length(y) - length(yref)))
        } else {
            y = c(y, rep(0, length(yref) - length(y)))
        }
    }
    entropy::KL.Dirichlet(y1=y, y2=yref, a1=1/length(y), a2=1/length(y))
}

# Mirkin index (MI) ####
labelSimilarity_mirkin = function(clus, refclus) {
    library(mclustcomp)
    if(anyNA(refclus)) {
        return(NaN)
    }
    mclustcomp::mclustcomp(clus, refclus, type.out='mirkin')[,1]
}


# Variation of information (VI) ####
labelSimilarity_vi = function(clus, refclus) {
    library(igraph)
    if(anyNA(refclus)) {
        return(NaN)
    }
    igraph::compare(clus, refclus, 'vi')
}

# Split-join distance ####
# @return pair of distances
labelSimilarity_splitJoin = function(clus, refclus) {
    library(igraph)
    if(anyNA(refclus)) {
        return(NaN)
    }
    igraph::split_join_distance(clus, refclus)
}

labelSimilarity_splitJoin_min = function(clus, refclus) {
    library(igraph)
    if(anyNA(refclus)) {
        return(NaN)
    }
    igraph::split_join_distance(clus, refclus) %>% min
}

labelSimilarity_splitJoin_total = function(clus, refclus) {
    library(igraph)
    if(anyNA(refclus)) {
        return(NaN)
    }
    igraph::compare(clus, refclus, 'split.join')
}

# Normalized split-join distance
labelSimilarity_nsj = function(clus, refclus) {
    sj = labelSimilarity_splitJoin_total(clus, refclus)
    sj / 2 / length(clus)
}

labelSimilarity_nsj_min = function(clus, refclus) {
    labelSimilarity_splitJoin_min(clus, refclus) / length(clus)
}

labelSimilarity_nsj_max = function(clus, refclus) {
    max(labelSimilarity_splitJoin(clus, refclus)) / length(clus)
}
