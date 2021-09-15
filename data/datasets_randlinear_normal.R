# longdata_randlinear(dataseed=NULL, numgroups=4, numtraj=20, numobs=10, noise=.01, int.sd=.1, slo.sd=.1) %>% plot_trajectories
longdata_randlinear = function(dataseed, numgroups, numtraj, numobs, noise, int.sd, slo.sd, props=sqrt(1:numgroups), ...) {
    stopifnot(numgroups >= 2)
    set.seed(dataseed)
    props = props / sum(props) #normalize
    stopifnot(all.equal(sum(props), 1))

    groups = lapply(1:numgroups, function(i) {
        tsdata_create_repmeas(group=paste0('G', i), numtraj=round(numtraj*props[i]), numobs=numobs, t.start=0, t.end=1) %>%
            tsdata_add_poly(coefs=runif(2, c(-1, -1), c(1, 1)), distr='norm', sd=c(int.sd, slo.sd))
    })
    dt_all = tsdata_merge(groups) %>%
        tsdata_add_noise(sd=noise)
    return(dt_all)
}

longdata_randlinear2 = function(...) {
    longdata_randlinear(numgroups=2, ...)
}

longdata_randlinear3 = function(...) {
    longdata_randlinear(numgroups=3, ...)
}

longdata_randlinear4 = function(...) {
    longdata_randlinear(numgroups=4, ...)
}
