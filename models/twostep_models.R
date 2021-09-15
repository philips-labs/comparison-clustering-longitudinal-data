#### LM-KM ####
longmodel_lm1km = quote(longclus_lm_kmeans(tsdata, numclus,
                                          numruns=get0('numruns', ifnotfound=10),
                                          store=get0('store', ifnotfound='none'),
                                          model=Value~Time,
                                          seed=seed))

longmodel_lm1km_orth = quote(longclus_lm_kmeans(tsdata, numclus,
                                               numruns=get0('numruns', ifnotfound=10),
                                               store=get0('store', ifnotfound='none'),
                                               model=Value~poly(Time, degree=1),
                                               seed=seed))

longmodel_lm2km = quote(longclus_lm_kmeans(tsdata, numclus,
                                           numruns=get0('numruns', ifnotfound=10),
                                           store=get0('store', ifnotfound='none'),
                                           model=Value~poly(Time, degree=2, raw=TRUE),
                                           seed=seed))

longmodel_lm2km_orth = quote(longclus_lm_kmeans(tsdata, numclus,
                                                numruns=get0('numruns', ifnotfound=10),
                                                store=get0('store', ifnotfound='none'),
                                                model=Value~poly(Time, degree=2),
                                                seed=seed))


#### GCM-KM ####
longmodel_gcm1km = quote(longclus_gcm_kmeans(tsdata, numclus,
                                            numruns=get0('numruns', ifnotfound=10),
                                            store=get0('store', ifnotfound='none'),
                                            model.fixed=Value~Time,
                                            model.random=~Time,
                                            seed=seed))

longmodel_gcm2km = quote(longclus_gcm_kmeans(tsdata, numclus,
                                             numruns=get0('numruns', ifnotfound=10),
                                             store=get0('store', ifnotfound='none'),
                                             model.fixed=Value~Time + I(Time^2),
                                             model.random=~Time + I(Time^2),
                                             seed=seed))

longmodel_gcm2km_orth = quote(longclus_gcm_kmeans(tsdata, numclus,
                                                  numruns=get0('numruns', ifnotfound=10),
                                                  store=get0('store', ifnotfound='none'),
                                                  model.fixed=Value~poly(Time, degree=2),
                                                  model.random=~poly(Time, degree=2),
                                                  seed=seed))
