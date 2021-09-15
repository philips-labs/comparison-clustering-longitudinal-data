#### Settings ####
cases_gbtm3 = expand.grid(data='longdata_randquad3', 
                          model=c('longmodel_gbtm2'),
                          numtraj=c(200),
                          numobs=c(4),
                          numruns=c(5, 20, 100, 200), 
                          maxiter=c(5, 10, 20, 100),
                          numclus=3,
                          noise=c(.1, .3),
                          dataseed=1:50,
                          seed=1, 
                          int.sd=c(.1), #.5
                          slo.sd=.1) %>% as.data.table; nrow(cases_gbtm3)
experiment_submit(name='hlme_optim', cases=cases_gbtm3)

cases_gbtm4 = expand.grid(data='longdata_randquad4', 
                          model=c('longmodel_gbtm2'),
                          numtraj=c(500), #200, 500, 1000
                          numobs=c(4), #10
                          numruns=c(5, 20, 100, 200), 
                          maxiter=c(5, 10, 20, 50),
                          numclus=4,
                          noise=c(.1, .3), #.3
                          dataseed=1:50,
                          seed=1, 
                          int.sd=.1,
                          slo.sd=.1) %>% as.data.table; nrow(cases_gbtm4)
experiment_submit(name='gbtm_optim', cases=cases_gbtm4)

cases_gbtm5 = expand.grid(data='longdata_randquad5', 
                          model=c('longmodel_gbtm2'),
                          numtraj=c(500), #200, 500, 1000
                          numobs=c(4), #10
                          numruns=c(5, 20, 100, 200), 
                          maxiter=c(5, 10, 20, 50),
                          numclus=5,
                          noise=c(.1), #.3
                          dataseed=1:50,
                          seed=1, 
                          int.sd=.1,
                          slo.sd=.1) %>% as.data.table; nrow(cases_gbtm5)
experiment_submit(name='gbtm_optim', cases=cases_gbtm5)

#### Results ####
results = experiment_getCases('gbtm_optim', function(m) c(ARI=m$adjustedRand, WRSS=m$WRSS, trendWRSS=m$trends$WRSS, refSilhouette=m$ref$Silhouette, Dunn=m$Dunn, Silhouette=m$silhouette, Time=m$time, Converged=m$converged, EmptyClusters=m$emptyClusters, LoneClusters=m$loneClusters, MinClusterSize=min(table(m$clusters)))) %>%
    .[, Valid := MinClusterSize > 5]

rx = results[data == 'longdata_randquad4']

#### Convergence ####
rx[numtraj == 500 & numobs == 4 & noise == .1 & numclus == 4 & int.sd == .1, .(Time=mean(Time), Converged=mean(Converged)), by=.(maxiter, numruns)] %>% 
    ggplot(aes(x=factor(maxiter), fill=Converged, y=factor(numruns), size=Time)) + 
    geom_tile(aes(fill=Converged), color='black', size=.2) + 
    geom_point(aes(color=Time), shape=15) +
    scale_color_viridis(option='plasma') +
    scale_fill_viridis() +
    scale_size_continuous(guide=FALSE) +
    coord_equal()

rx[, .(Time=mean(Time), ARI=mean(ARI)), by=.(numruns, maxiter)]

#### Converged ARI ####
rx[Converged == TRUE & numtraj == 500 & numobs == 4 & noise == .3 & numclus == 4 & int.sd == .1, 
   .(Time=mean(Time), ConvergedARI=quantile(ARI, .75)), by=.(maxiter, numruns)] %>% 
    ggplot(aes(x=factor(maxiter), fill=ConvergedARI, y=factor(numruns), size=Time)) + 
    geom_tile(color='black', size=.2) + 
    geom_point(aes(color=Time), shape=15) +
    scale_color_viridis(option='plasma') +
    scale_fill_viridis() +
    scale_size_continuous(guide=FALSE) +
    coord_equal()

rx[Converged == TRUE, .(Time=mean(Time), ARI=mean(ARI)), by=.(numruns, maxiter)]

#### Valid ####
rx[numtraj == 500 & numobs == 4 & noise == .1 & numclus == 4 & int.sd == .1, .(Time=mean(Time), Valid=mean(Valid)), by=.(maxiter, numruns)] %>% 
    ggplot(aes(x=factor(maxiter), fill=Valid, y=factor(numruns), size=Time)) + 
    geom_tile(color='black', size=.2) + 
    geom_point(aes(color=Time), shape=15) +
    scale_color_viridis(option='plasma') +
    scale_fill_viridis() +
    scale_size_continuous(guide=FALSE) +
    coord_equal()

#### Valid ARI ####
rx[numtraj == 500 & Valid == TRUE & numobs == 4 & noise == .1 & numclus == 5 & int.sd == .1, 
   .(Time=mean(Time), ValidARI=mean(ARI)), by=.(maxiter, numruns)] %>% 
    ggplot(aes(x=factor(maxiter), fill=ValidARI, y=factor(numruns), size=Time)) + 
    geom_tile(color='black', size=.2) + 
    geom_point(aes(color=Time), shape=15) +
    scale_color_viridis(option='plasma') +
    scale_fill_viridis() +
    scale_size_continuous(guide=FALSE) +
    coord_equal()
