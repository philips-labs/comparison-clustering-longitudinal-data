# Model specification ####
cases_mixspec = expand.grid(data='longdata_randquadG',
                         numgroups=4,
                         numclus=4,
                         model=c('longmodel_mixtvem_fe', 'longmodel_mixtvem_nugget'),
                         numtraj=500,
                         numobs=c(10, 25),
                         numruns=20,
                         re=.2,
                         noise=.1,
                         dataseed=1:20,
                         seed=1,
                         store='all') %>% as.data.table; nrow(cases_mixspec)
experiment_submit(name='mixtvem_spec', cases=cases_mixspec)
# nugget superior over fe for numobs=10
# nugget: doesn't converge at all for numobs=50; fe preferred
# performance not affected by numruns
# performance deterioriates with increased numtraj, re, and numobs (all independent)

# Numruns ####

# Analysis ####
rxmt = experiment_getOutputTable('mixtvem_spec') %>%
    .[, NSJ := sjTotal / (2 * numtraj)] %>%
    .[, WMMSE := trendWRSS / numobs * 10]

rxmt[, .(.N, CR=sum(converged)/.N, MeanNSJ=mean(NSJ,na.rm=TRUE), MeanWMMSE=mean(WMMSE,na.rm=TRUE)), keyby=.(model, numtraj, numruns, numobs, re)]
# rxmt[numtraj == 500 & noise == .1 & numclus == 3, .(.N, CR=sum(converged)/.N, MedARI=median(ARI,na.rm=TRUE), Time=mean(time, na.rm=TRUE)), keyby=.(model, model.numknots, numtraj, numruns, numobs, re)]

mod = lm(ARI ~ model + numtraj + numobs + re, data=rxmt[converged==TRUE & numruns == 50])

plot(allEffects(mod))
plot(allEffects(mod, xlevels=list(numobs=c(10,50), numtraj=c(200,500), noise=c(.01, .1))))

ggplot(rxmt, aes(x=model, y=trendWRSS, color=data)) +
    stat_summary(fun.data=mean_cl_normal, geom='pointrange') +
    stat_summary(fun.y=mean, geom='line') +
    facet_wrap(~paste0('T=', numobs) + paste0('RE=', re) + paste0('e=', noise)) +
    labs(title='Adjusted Rand index',
         subtitle=sprintf('data=%s', unique(rxmt$data))) + xrot45
