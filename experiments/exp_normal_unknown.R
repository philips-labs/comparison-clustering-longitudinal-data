cases_unknown_groups = expand.grid(data='longdata_randquadG',
                                    numgroups=2:5,
                                    model=c('longmodel_kml', 'longmodel_gcm2km', 'longmodel_gbtm2', 'longmodel_gmm2', 'longmodel_mixtvem_nugget'),
                                    numtraj=500,
                                    numobs=10,
                                    numclus=2:7,
                                    re=c(RE_NORM_LOW, RE_NORM_HIGH),
                                    noise=.1,
                                    dataseed=1:100,
                                    seed=1) %>% as.data.table %T>% print
experiment_submit(name='unknown_groups_normal', cases=cases_unknown_groups)
