cases_propnoise2 = expand.grid(data='longdata_randquadG_propnoise',
                               numgroups=2,
                               numclus=2,
                               model=c('longmodel_kml', 'longmodel_gcm2km', 'longmodel_gbtm2', 'longmodel_gmm2', 'longmodel_mixtvem_nugget'),
                               numtraj=500,
                               numobs=10,
                               re=c(RE_NORM_LOW, RE_NORM_HIGH),
                               propnoise=c(.01, .03, .05),
                               dataseed=1:100,
                               seed=1) %>% as.data.table %T>% print
experiment_submit(name='propnoise_known', cases=cases_propnoise2)

cases_propnoise3 = expand.grid(data='longdata_randquadG_propnoise',
                               numgroups=3,
                               numclus=3,
                               model=c('longmodel_kml', 'longmodel_gcm2km', 'longmodel_gbtm2', 'longmodel_gmm2', 'longmodel_mixtvem_nugget'),
                               numtraj=500,
                               numobs=10,
                               re=c(RE_NORM_LOW, RE_NORM_HIGH),
                               propnoise=c(.01, .03, .05),
                               dataseed=1:100,
                               seed=1) %>% as.data.table %T>% print
experiment_submit(name='propnoise_known', cases=cases_propnoise3)

cases_propnoise4 = expand.grid(data='longdata_randquadG_propnoise',
                               numgroups=4,
                               numclus=4,
                               model=c('longmodel_kml', 'longmodel_gcm2km', 'longmodel_gbtm2', 'longmodel_gmm2', 'longmodel_mixtvem_nugget'),
                               numtraj=500,
                               numobs=10,
                               re=c(RE_NORM_LOW, RE_NORM_HIGH),
                               propnoise=c(.01, .03, .05),
                               dataseed=1:100,
                               seed=1) %>% as.data.table %T>% print
experiment_submit(name='propnoise_known', cases=cases_propnoise4)

cases_propnoise5 = expand.grid(data='longdata_randquadG_propnoise',
                               numgroups=5,
                               numclus=5,
                               model=c('longmodel_kml', 'longmodel_gcm2km', 'longmodel_gbtm2', 'longmodel_gmm2', 'longmodel_mixtvem_nugget'),
                               numtraj=500,
                               numobs=10,
                               re=c(RE_NORM_LOW, RE_NORM_HIGH),
                               propnoise=c(.01, .03, .05),
                               dataseed=1:100,
                               seed=1) %>% as.data.table %T>% print
experiment_submit(name='propnoise_known', cases=cases_propnoise5)

cases_propnoise6 = expand.grid(data='longdata_randquadG_propnoise',
                               numgroups=6,
                               numclus=6,
                               model=c('longmodel_kml', 'longmodel_gcm2km', 'longmodel_gbtm2', 'longmodel_gmm2', 'longmodel_mixtvem_nugget'),
                               numtraj=500,
                               numobs=10,
                               re=c(RE_NORM_LOW, RE_NORM_HIGH),
                               propnoise=c(.01, .03, .05),
                               dataseed=1:100,
                               seed=1) %>% as.data.table %T>% print
experiment_submit(name='propnoise_known', cases=cases_propnoise6)
