# Model specification ####
cases_gmmspec3 = expand.grid(data='longdata_randquad3',
                             model=c('longmodel_gmm2q'),
                             numtraj=c(500),
                             numobs=c(10, 50),
                             numruns=c(20),
                             numclus=3,
                             re=c(.3),
                             noise=c(.1),
                             dataseed=1:20,
                             seed=1) %>% as.data.table; nrow(cases_gmmspec3)
experiment_submit(name='gmm_spec', cases=cases_gmmspec3)

results_gmm = experiment_getOutputTable('gmm_spec')

results_gmm[numtraj == 500 & noise == .1 & numclus == 3, .(.N, CR=sum(converged)/.N, MedARI=median(ARI,na.rm=TRUE), Time=mean(time, na.rm=TRUE)), keyby=.(model, numtraj, numruns, numobs, re)]

################################################################################
#### Settings ####
cases_gmm3 = expand.grid(data='longdata_randquad3',
                           model=c('longmodel_gmm2q'),
                           numtraj=200,
                           numobs=c(4),
                           numruns=c(5, 20, 100, 200),
                           maxiter=c(1, 5, 10, 20),
                           numclus=3,
                           re=c(.1, .3),
                           noise=.1,
                           dataseed=1:50,
                           seed=1) %>% as.data.table; nrow(cases_gmm3)
experiment_submit(name='gmm_optim', cases=cases_gmm3)

cases_gmm4 = expand.grid(data='longdata_randquad4',
                         model=c('longmodel_gmm2q'),
                         numtraj=500,
                         numobs=4,
                         numruns=c(5, 20, 100, 200), #400
                         maxiter=c(5, 10, 20),
                         numclus=4,
                         re=.1,
                         noise=c(.1),
                         dataseed=1:50,
                         seed=1) %>% as.data.table %>%
    addCaseParameters(data.table(int.sd=c(.1, .2, .3), slo.sd=c(.1, .2, .3), q.sd=c(.1, .2, .3))); nrow(cases_gmm4)
experiment_submit(name='gmm_optim', cases=cases_gmm4)

cases_gmm5 = expand.grid(data='longdata_randquad5',
                         model=c('longmodel_gmm2q'),
                         numtraj=500,
                         numobs=4,
                         numruns=c(20, 100, 200, 400), #400
                         maxiter=c(10, 20),
                         numclus=5,
                         re=.1,
                         noise=c(.1),
                         dataseed=1:50,
                         seed=1) %>% as.data.table %>%
    addCaseParameters(data.table(int.sd=c(.1, .3), slo.sd=c(.1, .3), q.sd=c(.1, .3))); nrow(cases_gmm5)
experiment_submit(name='gmm_optim', cases=cases_gmm5)


#### Results ####
results_gmm = experiment_getOutputTableOf('gmm_optim', function(m) c(ARI=m$adjustedRand, WRSS=m$WRSS, trendWRSS=m$trends$WRSS, refSilhouette=m$ref$Silhouette, Dunn=m$Dunn, Silhouette=m$silhouette, Time=m$time, Converged=m$converged, EmptyClusters=m$emptyClusters, LoneClusters=m$loneClusters, MinClusterSize=min(table(m$clusters)))) %>%
    .[, Valid := MinClusterSize > 5]

rx = results_gmm[data == 'longdata_randquad5' & model == 'longmodel_gmm2q']; nrow(rx)
## ARI ####
rx[numtraj == 500 & numobs == 4 & noise == .1 & numclus == 5 & int.sd == .1,
        .(Time=mean(Time), ARI=mean(ARI)), by=.(maxiter, numruns)] %>%
    ggplot(aes(x=factor(maxiter), fill=ARI, y=factor(numruns), size=Time)) +
    geom_tile() +
    geom_point(shape=15) +
    scale_color_viridis(option='plasma') +
    scale_fill_viridis() +
    scale_size_continuous(guide=FALSE) +
    coord_equal()

## Convergence ####
rx[numtraj == 500 & numobs == 4 & noise == .1 & numclus == 5 & int.sd == .3, .(Time=mean(Time), Converged=mean(Converged)), by=.(maxiter, numruns)] %>%
    ggplot(aes(x=factor(maxiter), fill=Converged, y=factor(numruns), size=Time)) +
    geom_tile(aes(fill=Converged), color='black', size=.2) +
    geom_point(aes(color=Time), shape=15) +
    scale_color_viridis(option='plasma') +
    scale_fill_viridis() +
    scale_size_continuous(guide=FALSE) +
    coord_equal()

## Converged ARI ####
rx[Converged == TRUE & numtraj == 500 & numobs == 4 & noise == .1 & numclus == 5 & int.sd == .1,
        .(Time=mean(Time), ConvergedARI=mean(ARI)), by=.(maxiter, numruns)] %>%
    ggplot(aes(x=factor(maxiter), fill=ConvergedARI, y=factor(numruns), size=Time)) +
    geom_tile(color='black', size=.2) +
    geom_point(aes(color=Time), shape=15) +
    scale_color_viridis(option='plasma') +
    scale_fill_viridis() +
    scale_size_continuous(guide=FALSE) +
    coord_equal()

rx[Converged == TRUE, .(Time=mean(Time), ARI=mean(ARI)), by=.(numruns, maxiter)]

## Valid ####
rx[numtraj == 500 & numobs == 4 & noise == .1 & numclus == 3 & int.sd == .1, .(Time=mean(Time), Valid=mean(Valid)), by=.(maxiter, numruns)] %>%
    ggplot(aes(x=factor(maxiter), fill=Valid, y=factor(numruns), size=Time)) +
    geom_tile(color='black', size=.2) +
    geom_point(aes(color=Time), shape=15) +
    scale_color_viridis(option='plasma') +
    scale_fill_viridis() +
    scale_size_continuous(guide=FALSE) +
    coord_equal()

## Valid ARI ####
rx[numtraj == 500 & Valid == TRUE & numobs == 4 & noise == .3 & numclus == 4 & int.sd == .1,
        .(Time=mean(Time), ValidARI=mean(ARI)), by=.(maxiter, numruns)] %>%
    ggplot(aes(x=factor(maxiter), fill=ValidARI, y=factor(numruns), size=Time)) +
    geom_tile(color='black', size=.2) +
    geom_point(aes(color=Time), shape=15) +
    scale_color_viridis(option='plasma') +
    scale_fill_viridis() +
    scale_size_continuous(guide=FALSE) +
    coord_equal()
