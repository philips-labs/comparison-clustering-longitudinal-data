relativeEntropy = function(pp) {
    pp = pmax(pp, .Machine$double.xmin)
    ent = -1 * sum(rowSums(pp * log(pp)))
    1 - ent / (nrow(pp) * log(ncol(pp)))
}
