#### Settings ####
cases_gcmkm3 = expand.grid(data='longdata_randquad3', 
                         model=c('longmodel_gcm2km'),
                         numtraj=c(200),
                         numobs=c(4),
                         numruns=c(1, 5, 10, 20, 50, 100, 200), 
                         numclus=3,
                         noise=c(.1, .3),
                         dataseed=1:100,
                         seed=1, 
                         int.sd=c(.1), #.5
                         slo.sd=.1) %>% as.data.table; nrow(cases_gcmkm3)
experiment_submit(name='gcmkm_optim', cases=cases_gcmkm3)

cases_gcmkm4 = expand.grid(data='longdata_randquad4', 
                         model=c('longmodel_gcm2km'),
                         numtraj=c(500), #200, 500, 1000
                         numobs=c(4), #10
                         numruns=c(1, 5, 10, 20, 50, 100, 200),
                         numclus=4,
                         noise=c(.1, .3), #.3
                         dataseed=1:100,
                         seed=1, 
                         int.sd=.1,
                         slo.sd=.1) %>% as.data.table; nrow(cases_gcmkm4)
experiment_submit(name='gcmkm_optim', cases=cases_gcmkm4)

cases_gcmkm5 = expand.grid(data='longdata_randquad5', 
                         model=c('longmodel_gcm2km'),
                         numtraj=c(500), #200, 500, 1000
                         numobs=c(4), #10
                         numruns=c(1, 5, 10, 20, 50, 100, 200),
                         numclus=5,
                         noise=c(.1, .3, .5), #.3
                         dataseed=1:100,
                         seed=1, 
                         int.sd=.1,
                         slo.sd=.1) %>% as.data.table; nrow(cases_gcmkm5)
experiment_submit(name='gcmkm_optim', cases=cases_gcmkm5)


#### Results ####
results = experiment_getCases('gcmkm_optim', function(m) c(ARI=m$adjustedRand, WRSS=m$WRSS, trendWRSS=m$trends$WRSS, refSilhouette=m$ref$Silhouette, Dunn=m$Dunn, Silhouette=m$silhouette, Time=m$time, Converged=m$converged, EmptyClusters=m$emptyClusters, LoneClusters=m$loneClusters, MinClusterSize=min(table(m$clusters)))) %>%
    .[, Valid := MinClusterSize > 5]

rx = results[data == 'longdata_randquad5'] %>%
    setkey(model, numruns)


## ARI ####
rx[numobs == 4 & numclus == 5 & int.sd == .1, 
   c(Time=mean(Time), mean_se(ARI)), by=.(noise, numruns)] %>% 
    ggplot(aes(x=factor(numruns), y=y, ymin=ymin, ymax=ymax)) + 
    geom_pointrange() +
    geom_point(aes(color=Time, size=Time)) +
    scale_color_viridis(option='plasma') +
    scale_size_continuous(guide=FALSE) + labs(x='Number of runs', y='ARI') +
    facet_wrap(~paste0('noise=', noise), ncol=1, scales='free_y')

## DiffARI ####
rxdiff = rx[numobs == 4 & numclus == 5 & int.sd == .1, .(DiffARI=diff(ARI), numruns=tail(numruns, -1), DiffTime=diff(Time)), by=.(dataseed, noise)]

rxdiff[, c(DiffTime=mean(DiffTime), mean_se(DiffARI)), by=.(noise, numruns)] %>% 
    ggplot(aes(x=factor(numruns), y=y, ymin=ymin, ymax=ymax)) + 
    geom_pointrange() +
    geom_point(aes(color=DiffTime)) +
    scale_color_viridis(option='plasma') +
    scale_size_continuous(guide=FALSE) + labs(x='Number of runs', y='DiffARI') +
    facet_wrap(~paste0('noise=', noise), ncol=1, scales='free_y')
