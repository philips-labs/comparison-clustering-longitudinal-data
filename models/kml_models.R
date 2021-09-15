longmodel_kml = quote(longclus_kml(tsdata, numclus,
                                   numruns=get0('numruns', ifnotfound=10),
                                   store=get0('store', ifnotfound='none'),
                                   seed=seed))
