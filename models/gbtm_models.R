# Linear ####
longmodel_gbtm1 = quote(longclus_gbtm(tsdata, numclus,
                                     model.fixed=Value~Time,
                                     model.mixture=~Time,
                                     numruns=get0('numruns', ifnotfound=20),
                                     maxiter=get0('maxiter', ifnotfound=20),
                                     store=get0('store', ifnotfound='none'),
                                     seed=seed))

# Quadratic ####
longmodel_gbtm2_raw = quote(longclus_gbtm(tsdata, numclus,
                                          model.fixed=Value~Time+I(Time^2),
                                          model.mixture=~Time+I(Time^2),
                                          numruns=get0('numruns', ifnotfound=20),
                                          maxiter=get0('maxiter', ifnotfound=20),
                                          store=get0('store', ifnotfound='none'),
                                          seed=seed))

longmodel_gbtm2_orth = quote(longclus_gbtm(tsdata, numclus,
                                           model.fixed=Value~poly(Time, degree=2),
                                           model.mixture=~poly(Time, degree=2),
                                           numruns=get0('numruns', ifnotfound=20),
                                           maxiter=get0('maxiter', ifnotfound=20),
                                           store=get0('store', ifnotfound='none'),
                                           seed=seed))

# Selection ####
longmodel_gbtm2 = longmodel_gbtm2_raw

# BLRT ####
blrt_longmodel_gbtm2 = quote(longclus_gmm_blrt(tsdata, minclus, maxclus,
    model.fixed=Value~Time+I(Time^2),
    model.mixture=~Time+I(Time^2),
    model.random=~-1,
    numruns=get0('numruns', ifnotfound=20),
    maxiter=get0('maxiter', ifnotfound=20),
    store=get0('store', ifnotfound='none'),
    seed=seed))