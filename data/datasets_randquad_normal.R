#### Random quadratic ####
# longdata_randquad(dataseed=NULL, numgroups=5, numtraj=200, numobs=10, noise=.01, int.sd=.1, slo.sd=.1, q.sd=0) %>% plot_trajectories
longdata_randquad = function(dataseed, numgroups, numtraj, numobs, noise, int.sd, slo.sd, q.sd=0, props=sqrt(1:numgroups), ...) {
    stopifnot(numgroups >= 2)
    set.seed(dataseed)
    props = props / sum(props)
    stopifnot(all.equal(sum(props), 1))

    groups = lapply(1:numgroups, function(i) {
        tsdata_create_repmeas(group=paste0('G', i), numtraj=round(numtraj*props[i]), numobs=numobs, t.start=0, t.end=1) %>%
            tsdata_add_poly(coefs=runif(3, c(-1, -1, -1), c(1, 1, 1)), distr='norm', sd=c(int.sd, slo.sd, q.sd))
    })
    dt_all = tsdata_merge(groups) %>%
        tsdata_add_noise(sd=noise)
    return(dt_all)
}

longdata_randquadG = function(re, numgroups, ...) {
    longdata_randquad(numgroups=numgroups, int.sd=re, slo.sd=re, q.sd=re, ...)
}

longdata_randquad2 = function(re, ...) {
    longdata_randquad(numgroups=2, int.sd=re, slo.sd=re, q.sd=re, ...)
}

longdata_randquad3 = function(re, ...) {
    longdata_randquad(numgroups=3, int.sd=re, slo.sd=re, q.sd=re, ...)
}

longdata_randquad4 = function(re, ...) {
    longdata_randquad(numgroups=4, int.sd=re, slo.sd=re, q.sd=re, ...)
}

longdata_randquad5 = function(re, ...) {
    longdata_randquad(numgroups=5, int.sd=re, slo.sd=re, q.sd=re, ...)
}

longdata_randquad6 = function(re, ...) {
    longdata_randquad(numgroups=6, int.sd=re, slo.sd=re, q.sd=re, ...)
}

longdata_randquad7 = function(re, ...) {
    longdata_randquad(numgroups=7, int.sd=re, slo.sd=re, q.sd=re, ...)
}
