cases_blrt = expand.grid(
    data='longdata_randquadG',
    numgroups=2:5,
    model = c('blrt_longmodel_gbtm2', 'blrt_longmodel_gmm2'),
    numtraj=c(500), 
    numobs=c(10), 
    numruns=c(20), 
    minclus=2,
    maxclus=7,
    re=c(RE_NORM_LOW, RE_NORM_HIGH),
    noise=.1, 
    dataseed=1:100, 
    seed=1) %>% 
    as.data.table() %T>% 
    print()

# experiment_delete('blrt')
experiment_submit(name='blrt', cases=cases_blrt)
