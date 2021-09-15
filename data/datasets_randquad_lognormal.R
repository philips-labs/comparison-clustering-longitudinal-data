# longdata_randquad_lognorm(dataseed=NULL, numgroups=5, numtraj=200, numobs=10, noise=.01, int.mean=log(.15), slo.mean=log(.15), q.mean=log(.15), int.sd=log(2), slo.sd=log(2), q.sd=log(2)) %>% plot_trajectories
longdata_randquad_lognorm = function(dataseed, numgroups, numtraj, numobs, noise,
                                     int.mean, slo.mean, q.mean,
                                     int.sd=sqrt(.5), slo.sd=sqrt(.5), q.sd=sqrt(.5), props=sqrt(1:numgroups), ...) {
    stopifnot(numgroups >= 2)
    set.seed(dataseed)
    props = props / sum(props)
    stopifnot(all.equal(sum(props), 1))
    message('groups')
    groups = lapply(1:numgroups, function(i) {
        tsdata_create_repmeas(group=paste0('G', i), numtraj=round(numtraj*props[i]), numobs=numobs, t.start=0, t.end=1) %>%
            tsdata_add_poly(coefs=runif(3, c(-1, -1, -1), c(1, 1, 1)), distr='lnorm', meanlog=c(int.mean, slo.mean, q.mean), sdlog=c(int.sd, slo.sd, q.sd))
    })
    message('merge')
    dt_all = tsdata_merge(groups) %>%
        tsdata_add_noise(sd=noise)
    return(dt_all)
}

longdata_randquad_lognormG = function(re, numgroups, ...) {
    longdata_randquad_lognorm(numgroups=numgroups, int.mean=re, slo.mean=re, q.mean=re, ...)
}

longdata_randquad_lognorm2 = function(re, ...) {
    longdata_randquad_lognorm(numgroups=2, int.mean=re, slo.mean=re, q.mean=re, ...)
}

longdata_randquad_lognorm3 = function(re, ...) {
    longdata_randquad_lognorm(numgroups=3, int.mean=re, slo.mean=re, q.mean=re, ...)
}

longdata_randquad_lognorm4 = function(re, ...) {
    longdata_randquad_lognorm(numgroups=4, int.mean=re, slo.mean=re, q.mean=re, ...)
}

longdata_randquad_lognorm5 = function(re, ...) {
    longdata_randquad_lognorm(numgroups=5, int.mean=re, slo.mean=re, q.mean=re, ...)
}
