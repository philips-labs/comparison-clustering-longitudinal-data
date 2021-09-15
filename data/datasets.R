# Contains hard-coded datasets
####

# n=100; meas=10; noise=.1; int.sd=.1; slo.sd=.1
longdata_linear3 = function(dataseed, numtraj, numobs, noise, int.sd, slo.sd, ...) {
    set.seed(dataseed)
    # Group 1 (50%) stable around 0
    dt_group1 = tsdata_create_repmeas(group='Stable', numtraj=round(numtraj*.5), numobs=numobs, t.start=0, t.end=1) %>%
        tsdata_add_poly(coefs=0, distr='norm', sd=int.sd)

    # Group 2 (30%) increasing from (0, -.5) to (0, 1.5)
    dt_group2 = tsdata_create_repmeas(group='Increasing', numtraj=round(numtraj*.3), numobs=numobs, t.start=0, t.end=1) %>%
        tsdata_add_poly(coefs=c(-.5, 1), distr='norm', sd=c(int.sd, slo.sd))

    # Group 3 (20%) decreasing from (0, .5) to (0, -1.5)
    dt_group3 = tsdata_create_repmeas(group='Decreasing', numtraj=round(numtraj*.2), numobs=numobs, t.start=0, t.end=1) %>%
        tsdata_add_poly(coefs=c(.5, -1), distr='norm', sd=c(int.sd, slo.sd))

    dt_all = tsdata_merge(list(dt_group1, dt_group2, dt_group3)) %>%
        tsdata_add_noise(sd=noise)
    return(dt_all)
}

# n=100; meas=10; noise=.1; int.sd=.1; slo.sd=.1; q.sd=0
longdata_quad3 = function(dataseed, numtraj, numobs, noise, int.sd, slo.sd, q.sd=0, ...) {
    set.seed(dataseed)
    # Group 1 (10%): stable around zero
    dt_group1 = tsdata_create_repmeas(group='Stable', numtraj=round(numtraj*.4), numobs=numobs, t.start=0, t.end=1) %>%
        tsdata_add_poly(coefs=0, distr='norm', sd=int.sd)
    # Group 2 (10%): increasing from (0,0) to (0,1)
    dt_group2 = tsdata_create_repmeas(group='Increasing', numtraj=round(numtraj*.2), numobs=numobs, t.start=0, t.end=1) %>%
        tsdata_add_poly(coefs=c(0,1), distr='norm', sd=c(int.sd, slo.sd))
    # Group 3 (40%): parabola around (.5,.5)
    dt_group3 = tsdata_create_repmeas(group='Parabola', numtraj=round(numtraj*.4), numobs=numobs, t.start=0, t.end=1) %>%
        tsdata_add_poly(coefs=c(0,3,-3), distr='norm', sd=c(int.sd, slo.sd, q.sd))

    dt_all = tsdata_merge(list(dt_group1, dt_group2, dt_group3)) %>%
        tsdata_add_noise(sd=noise)
    return(dt_all)
}

# dataseed=1; numtraj=100; numobs=10; propnoise=.4; int.sd=.1; slo.sd=.1
longdata_linear3_propnoise = function(dataseed, numtraj, numobs, propnoise, int.sd, slo.sd, ...) {
    set.seed(dataseed)
    # Group 1 (50%) stable around 0
    dt_group1 = tsdata_create_repmeas(group='Stable', numtraj=round(numtraj*.5), numobs=numobs, t.start=0, t.end=1) %>%
        tsdata_add_poly(coefs=10, distr='norm', sd=int.sd)

    # Group 2 (30%) increasing from (0, -.5) to (0, 1.5)
    dt_group2 = tsdata_create_repmeas(group='Increasing', numtraj=round(numtraj*.3), numobs=numobs, t.start=0, t.end=1) %>%
        tsdata_add_poly(coefs=c(6.5, 1), distr='norm', sd=c(int.sd, slo.sd))

    # Group 3 (20%) decreasing from (0, .5) to (0, -1.5)
    dt_group3 = tsdata_create_repmeas(group='Decreasing', numtraj=round(numtraj*.2), numobs=numobs, t.start=0, t.end=1) %>%
        tsdata_add_poly(coefs=c(8.5, -1), distr='norm', sd=c(int.sd, slo.sd))

    dt_all = tsdata_merge(list(dt_group1, dt_group2, dt_group3)) %>%
        tsdata_add_propnoise(prop=propnoise)
    return(dt_all)
}
