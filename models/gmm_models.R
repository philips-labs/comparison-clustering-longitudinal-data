# Linear ####
longmodel_gmm1c = quote(longclus_gmm(tsdata, numclus,
                                    model.fixed=Value~Time,
                                    model.mixture=~Time,
                                    model.random=~1,
                                    numruns=get0('numruns', ifnotfound=20),
                                    maxiter=get0('maxiter', ifnotfound=20),
                                    store=get0('store', ifnotfound='none'),
                                    model.diagcov=TRUE,
                                    model.classcov=FALSE,
                                    seed=seed))

# Quadratic ####
longmodel_gmm2c = quote(longclus_gmm(tsdata, numclus,
                                     model.fixed=Value~Time+I(Time^2),
                                     model.mixture=~Time+I(Time^2),
                                     model.random=~1,
                                     numruns=get0('numruns', ifnotfound=20),
                                     maxiter=get0('maxiter', ifnotfound=20),
                                     store=get0('store', ifnotfound='none'),
                                     model.diagcov=TRUE,
                                     model.classcov=FALSE,
                                     seed=seed))

longmodel_gmm2l = quote(longclus_gmm(tsdata, numclus,
                                     model.fixed=Value~Time+I(Time^2),
                                     model.mixture=~Time+I(Time^2),
                                     model.random=~Time,
                                     numruns=get0('numruns', ifnotfound=20),
                                     maxiter=get0('maxiter', ifnotfound=20),
                                     store=get0('store', ifnotfound='none'),
                                     model.diagcov=TRUE,
                                     model.classcov=FALSE,
                                     seed=seed))

longmodel_gmm2q = quote(longclus_gmm(tsdata, numclus,
                                     model.fixed=Value~Time+I(Time^2),
                                     model.mixture=~Time+I(Time^2),
                                     model.random=~Time+I(Time^2),
                                     numruns=get0('numruns', ifnotfound=20),
                                     maxiter=get0('maxiter', ifnotfound=20),
                                     store=get0('store', ifnotfound='none'),
                                     model.diagcov=TRUE,
                                     model.classcov=FALSE,
                                     seed=seed))

# Orthogonal polynomial models ###
longmodel_gmm2c_orth = quote(longclus_gmm(tsdata, numclus,
                                          model.fixed=Value~poly(Time, degree=2),
                                          model.mixture=~poly(Time, degree=2),
                                          model.random=~1,
                                          numruns=get0('numruns', ifnotfound=20),
                                          maxiter=get0('maxiter', ifnotfound=20),
                                          store=get0('store', ifnotfound='none'),
                                          model.diagcov=TRUE,
                                          model.classcov=FALSE,
                                          seed=seed))

# Selection ####
longmodel_gmm2 = longmodel_gmm2q

# BLRT ####
# same settings as longmodel_gmm2q
blrt_longmodel_gmm2 = quote(longclus_gmm_blrt(tsdata, minclus, maxclus,
    model.fixed=Value~Time+I(Time^2),
    model.mixture=~Time+I(Time^2),
    model.random=~Time+I(Time^2),
    samples=get0('samples', ifnotfound=500),
    numruns=get0('numruns', ifnotfound=20),
    maxiter=get0('maxiter', ifnotfound=20),
    store=get0('store', ifnotfound='none'),
    model.diagcov=TRUE,
    model.classcov=FALSE,
    seed=seed))