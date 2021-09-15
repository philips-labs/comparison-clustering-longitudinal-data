longmodel_mixtvem_fe = quote(longclus_mixtvem(tsdata, numclus,
                                              model.numknots=6, model.degree=3, model.autocor=FALSE,
                                              numruns=get0('numruns', ifnotfound=20),
                                              store=get0('store', ifnotfound='none'),
                                              seed=seed))

longmodel_mixtvem_nugget = quote(longclus_mixtvem(tsdata, numclus,
                                                  model.numknots=6, model.degree=3, model.autocor=TRUE,
                                                  numruns=get0('numruns', ifnotfound=20),
                                                  store=get0('store', ifnotfound='none'),
                                                  seed=seed))

longmodel_mixtvem = longmodel_mixtvem_fe
