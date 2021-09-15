# longdata_randquad_propnoise(dataseed=NULL, numgroups=5, numtraj=100, numobs=10, propnoise=.01, int.sd=.1, slo.sd=.1, q.sd=0) %>% plot_trajectories
longdata_randquad_propnoise = function(dataseed, numgroups, numtraj, numobs, propnoise, int.sd, slo.sd, q.sd=0, props=sqrt(1:numgroups), ...) {
    stopifnot(numgroups >= 2)
    set.seed(dataseed)
    props = props / sum(props)
    stopifnot(all.equal(sum(props), 1))

    groups = lapply(1:numgroups, function(i) {
        tsdata_create_repmeas(group=paste0('G', i), numtraj=round(numtraj*props[i]), numobs=numobs, t.start=0, t.end=1) %>%
            tsdata_add_poly(coefs=runif(3, c(1, -1, -1), c(3, 1, 1)), distr='norm', sd=c(int.sd, slo.sd, q.sd))
    })
    dt_all = tsdata_merge(groups) %>%
        tsdata_add_propnoise(prop=propnoise)
    return(dt_all)
}

longdata_randquadG_propnoise = function(numgroups, re, ...) {
    longdata_randquad_propnoise(numgroups=numgroups, int.sd=re, slo.sd=re, q.sd=re, ...)
}

longdata_randquad3_propnoise = function(re, ...) {
    longdata_randquad_propnoise(numgroups=3, int.sd=re, slo.sd=re, q.sd=re, ...)
}

longdata_randquad4_propnoise = function(re, ...) {
    longdata_randquad_propnoise(numgroups=4, int.sd=re, slo.sd=re, q.sd=re, ...)
}

longdata_randquad5_propnoise = function(re, ...) {
    longdata_randquad_propnoise(numgroups=5, int.sd=re, slo.sd=re, q.sd=re, ...)
}
