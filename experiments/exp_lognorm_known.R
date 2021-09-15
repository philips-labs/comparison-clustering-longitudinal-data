cases_lognormal2 = expand.grid(data='longdata_randquad_lognormG',
                               numgroups=2,
                               numclus=2,
                               model=c('longmodel_kml', 'longmodel_gcm2km', 'longmodel_gbtm2', 'longmodel_gmm2', 'longmodel_mixtvem_nugget', 'longmodel_mixtvem_fe'),
                               numtraj=500,
                               numobs=10,
                               re=c(RE_LNORM_LOW, RE_LNORM_MED, RE_LNORM_HIGH),
                               noise=c(.01, .1),
                               dataseed=1:100,
                               seed=1) %>% as.data.table %T>% print
experiment_submit(name='lognormal_known', cases=cases_lognormal2)

cases_lognormal3 = expand.grid(data='longdata_randquad_lognormG',
                                    numgroups=3,
                                    numclus=3,
                                    model=c('longmodel_kml', 'longmodel_gcm2km', 'longmodel_gbtm2', 'longmodel_gmm2', 'longmodel_mixtvem_nugget', 'longmodel_mixtvem_fe'),
                                    numtraj=500,
                                    numobs=10,
                                    re=c(RE_LNORM_LOW, RE_LNORM_MED, RE_LNORM_HIGH),
                                    noise=c(.01, .1),
                                    dataseed=1:100,
                                    seed=1) %>% as.data.table %T>% print
experiment_submit(name='lognormal_known', cases=cases_lognormal3)

cases_lognormal4 = expand.grid(data='longdata_randquad_lognormG',
                               numgroups=4,
                               numclus=4,
                               model=c('longmodel_kml', 'longmodel_gcm2km', 'longmodel_gbtm2', 'longmodel_gmm2', 'longmodel_mixtvem_nugget', 'longmodel_mixtvem_fe'),
                               numtraj=500,
                               numobs=10,
                               re=c(RE_LNORM_LOW, RE_LNORM_MED, RE_LNORM_HIGH),
                               noise=c(.01, .1),
                               dataseed=1:100,
                               seed=1) %>% as.data.table %T>% print
experiment_submit(name='lognormal_known', cases=cases_lognormal4)

cases_lognormal5 = expand.grid(data='longdata_randquad_lognormG',
                               numgroups=5,
                               numclus=5,
                               model=c('longmodel_kml', 'longmodel_gcm2km', 'longmodel_gbtm2', 'longmodel_gmm2', 'longmodel_mixtvem_nugget', 'longmodel_mixtvem_fe'),
                               numtraj=500,
                               numobs=10,
                               re=c(RE_LNORM_LOW, RE_LNORM_MED, RE_LNORM_HIGH),
                               noise=c(.01, .1),
                               dataseed=1:100,
                               seed=1) %>% as.data.table %T>% print
experiment_submit(name='lognormal_known', cases=cases_lognormal5)

cases_lognormal6 = expand.grid(data='longdata_randquad_lognormG',
                               numgroups=6,
                               numclus=6,
                               model=c('longmodel_kml', 'longmodel_gcm2km', 'longmodel_gbtm2', 'longmodel_gmm2', 'longmodel_mixtvem_nugget', 'longmodel_mixtvem_fe'),
                               numtraj=500,
                               numobs=10,
                               re=c(RE_LNORM_LOW, RE_LNORM_MED, RE_LNORM_HIGH),
                               noise=c(.01, .1),
                               dataseed=1:100,
                               seed=1) %>% as.data.table %T>% print
experiment_submit(name='lognormal_known', cases=cases_lognormal6)

# Results ####
library(effects)
results_log = experiment_getOutputTable('lognormal_known') %>%
    .[, NSJ := sjTotal / (2 * numtraj)] %>%
    .[, Valid := converged == TRUE & emptyClusters == 0 & loneClusters == 0]

rx_log = results_log

rx_log[, mean(NSJ, na.rm=TRUE), keyby=model]

# ecdf nsj
ggplot(rx_log, aes(x=NSJ, color=model)) +
    stat_ecdf(geom='step') +
    facet_wrap(~numgroups + re)

# ecdf mwrss
ggplot(rx_log, aes(x=trendWRSS, color=model)) +
    stat_ecdf(geom='step') +
    facet_wrap(~numgroups + re)

mod_nsj = lm(NSJ ~ numgroups + noise + re * model + re * numgroups, data=rx_log); print(BIC(mod_nsj))
summary(mod_nsj)

# average effects
rbindlist(idcol='effect', list(
    numgroups=Effect('numgroups', mod_nsj) %>% as.data.table,
    re=Effect('re', mod_nsj) %>% as.data.table,
    noise=Effect('noise', mod_nsj) %>% as.data.table)) %>%
    ggplot(mapping=aes(x=numgroups, y=fit, ymin=lower, ymax=upper)) +
    geom_ribbon(fill='red') +
    geom_line() +
    facet_wrap(~ effect, scales='free_x')

# Model-specific effects
rbindlist(idcol='effect', list(
    numgroups=Effect(c('numgroups', 'model'), mod_nsj) %>% as.data.table,
    re=Effect(c('re', 'model'), mod_nsj) %>% as.data.table,
    noise=Effect(c('noise', 'model'), mod_nsj) %>% as.data.table)) %>%
    ggplot(aes(x=numgroups, y=fit, ymin=lower, ymax=upper, color=model)) +
    geom_line() +
    scale_y_continuous(breaks=seq(0,1,by=.1)) +
    coord_cartesian(ylim=c(0, .6)) +
    facet_wrap(~ effect, scales='free_x') +
    labs(x='Effect', y='NSJ')

#MWRSS
mod_wrss = lm(trendWRSS ~ re * model, data=rx_log); BIC(mod_wrss)

# Model-specific effects
rbindlist(idcol='effect', list(
    re=Effect(c('re', 'model'), mod_wrss) %>% as.data.table)) %>%
    ggplot(aes(x=re, y=fit, ymin=lower, ymax=upper, color=model)) +
    geom_line() +
    scale_y_continuous(breaks=seq(0,1,by=.1)) +
    facet_wrap(~ effect, scales='free_x') +
    labs(x='Effect', y='MWRSS')
