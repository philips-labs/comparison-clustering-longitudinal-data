cases_normal2 = expand.grid(data=c('longdata_randquad2'),
                                    model=c('longmodel_kml', 'longmodel_gcm2km', 'longmodel_gbtm2', 'longmodel_gmm2', 'longmodel_mixtvem_nugget', 'longmodel_mixtvem_fe'),
                                    numtraj=c(200, 500, 1000),
                                    numobs=c(4, 10, 25),
                                    numclus=2,
                                    re=c(RE_NORM_LOW, RE_NORM_MED, RE_NORM_HIGH),
                                    noise=c(.01, .1),
                                    dataseed=1:100,
                                    seed=1) %>% as.data.table %T>% print
experiment_submit(name='normal_known', cases=cases_normal2)

cases_normal3 = expand.grid(data=c('longdata_randquad3'),
                                    model=c('longmodel_kml', 'longmodel_gcm2km', 'longmodel_gbtm2', 'longmodel_gmm2', 'longmodel_mixtvem_nugget', 'longmodel_mixtvem_fe'),
                                    numtraj=c(200, 500, 1000),
                                    numobs=c(4, 10, 25),
                                    numclus=3,
                                    re=c(RE_NORM_LOW, RE_NORM_MED, RE_NORM_HIGH),
                                    noise=c(.01, .1),
                                    dataseed=1:100,
                                    seed=1) %>% as.data.table %T>% print
experiment_submit(name='normal_known', cases=cases_normal3)

cases_normal4 = expand.grid(data=c('longdata_randquad4'),
                                    model=c('longmodel_kml', 'longmodel_gcm2km', 'longmodel_gbtm2', 'longmodel_gmm2', 'longmodel_mixtvem_nugget'),
                                    numtraj=c(200, 500, 1000),
                                    numobs=c(4, 10, 25),
                                    numclus=4,
                                    re=c(RE_NORM_LOW, RE_NORM_MED, RE_NORM_HIGH),
                                    noise=c(.01, .1),
                                    dataseed=1:100,
                                    seed=1) %>% as.data.table %T>% print
experiment_submit(name='normal_known', cases=cases_normal4)

cases_normal5 = expand.grid(data=c('longdata_randquad5'),
                                    model=c('longmodel_kml', 'longmodel_gcm2km', 'longmodel_gbtm2', 'longmodel_gmm2', 'longmodel_mixtvem_nugget'),
                                    numtraj=c(200, 500, 1000),
                                    numobs=c(4, 10, 25),
                                    numclus=5,
                                    re=c(RE_NORM_LOW, RE_NORM_MED, RE_NORM_HIGH),
                                    noise=c(.01, .1),
                                    dataseed=1:100,
                                    seed=1) %>% as.data.table %T>% print
experiment_submit(name='normal_known', cases=cases_normal5)

cases_normal6 = expand.grid(data=c('longdata_randquad6'),
                                    model=c('longmodel_kml', 'longmodel_gcm2km', 'longmodel_gbtm2', 'longmodel_gmm2', 'longmodel_mixtvem_nugget'),
                                    numtraj=c(200, 500, 1000),
                                    numobs=c(4, 10, 25),
                                    numclus=6,
                                    re=c(RE_NORM_LOW, RE_NORM_MED, RE_NORM_HIGH),
                                    noise=c(.01, .1),
                                    dataseed=1:100,
                                    seed=1) %>% as.data.table %T>% print
experiment_submit(name='normal_known', cases=cases_normal6)


# Results ####
library(effects)
results_normal = experiment_getOutputTable('normal_known') %>%
    .[, NSJ := sjTotal / (2 * numtraj)] %>%
    .[, model := factor(model, levels=c('longmodel_kml', 'longmodel_gcm2km', 'longmodel_gbtm2', 'longmodel_gmm2', 'longmodel_mixtvem_nugget'), labels=c('KML', 'GCKM', 'GBTM', 'GMM', 'MixTVEM'))] %>%
    .[, Valid := converged == TRUE & emptyClusters == 0 & loneClusters == 0] %>%
    .[, numgroups := substr(data, nchar('longdata_randquad1'), nchar('longdata_randquad1')) %>% as.integer]

rx_normal = results_normal[numgroups == numclus]
rx_aggr = rx_normal[, .(NSJ=mean(NSJ, na.rm=TRUE), MWSSE=mean(trendWRSS, na.rm=TRUE)), by=.(data, model, numtraj, numobs, numclus, re, noise)]
# Overview ####
# NSJ per model
ggplot(rx_normal, aes(x=NSJ, color=model)) +
    geom_density(adjust=.5) +
    facet_wrap(~numclus, scales='free_y')

# NSJ ecdf
ggplot(rx_normal, aes(x=NSJ, color=model)) +
    stat_ecdf(geom='step') +
    facet_wrap(~numclus + numtraj)

ggplot(rx_aggr, aes(x=NSJ, color=model)) +
    stat_ecdf(geom='step') +
    facet_wrap(~numclus)


# NSJ mean + error plot
rx_normal[, .(NSJ=mean(NSJ, na.rm=TRUE), sd=sd(NSJ, na.rm=TRUE), N=.N), by=.(model)] %>%
    .[, ':='(ymin=NSJ - sd, ymax=NSJ + sd)] %>%
    ggplot(aes(x=model, y=NSJ, ymin=ymin, ymax=ymax)) +
    geom_pointrange() +
    xrot45

# MWRSS density
ggplot(rx_normal, aes(x=trendWRSS, color=model)) +
    geom_density() +
    scale_x_continuous(limits=c(0, 2))

# MWRSS ecdf
ggplot(rx_normal, aes(x=trendWRSS, color=model)) +
    stat_ecdf(geom='step') +
    coord_cartesian(xlim=c(0, 3)) +
    facet_wrap(~numclus)

# MWRSS mean + error
rx_normal[, .(MinWRSS=median(trendWRSS, na.rm=TRUE), lower=quantile(trendWRSS, .025, na.rm=TRUE), upper=quantile(trendWRSS, .975, na.rm=TRUE), N=.N), by=.(model)] %>%
    ggplot(aes(x=model, y=MinWRSS, ymin=lower, ymax=upper)) +
    geom_pointrange() +
    coord_cartesian(ylim=c(0, 1.5)) +
    xrot45

# NSJ ####
mod_all_nsj = lm(NSJ ~ numtraj + poly(re,2) * model + poly(numobs,2) * model + noise * model + numclus * model, data=rx_normal); print(BIC(mod_all_nsj))
mod_aggr_nsj = lm(NSJ ~ numtraj + numclus * model + noise * model + poly(re,2) * model + poly(numobs,2) * model, data=rx_aggr); print(BIC(mod_aggr_nsj))
mod_nsj = mod_all_nsj
mod_nsj = mod_aggr_nsj
summary(mod_nsj)

xlevels = list(noise=c(.01, .1), numobs=c(4,10,25), numclus=3:7, re=c(.1,.2,.3), numtraj=c(200, 500, 1000))

# Average effects
rbindlist(idcol='effect', list(
        numclus=Effect('numclus', mod_nsj) %>% as.data.table,
        re=Effect('re', mod_nsj) %>% as.data.table,
        numobs=Effect('numobs', mod_nsj) %>% as.data.table,
        numtraj=Effect('numtraj', mod_nsj) %>% as.data.table,
        noise=Effect('noise', mod_nsj) %>% as.data.table)) %>%
    ggplot(mapping=aes(x=numclus, y=fit, ymin=lower, ymax=upper)) +
        geom_ribbon(fill='red') +
        geom_line() +
        facet_wrap(~ effect, scales='free_x')

Effect(c('numtraj', 'model'), mod_nsj, xlevels=xlevels) %>% plot(x.var='numtraj', rug=FALSE)
Effect(c('numobs', 'model'), mod_nsj, xlevels=xlevels) %>% plot(x.var='numobs', rug=FALSE)
Effect(c('re', 'model'), mod_nsj, xlevels=xlevels) %>% plot(x.var='re', rug=FALSE)
Effect(c('noise', 'model'), mod_nsj, xlevels=xlevels) %>% plot(x.var='noise', rug=FALSE)
Effect(c('numclus', 'model'), mod_nsj, xlevels=xlevels) %>% plot(x.var='numclus', rug=FALSE)

Effect(c('numobs', 'noise', 'model'), mod_nsj, xlevels=xlevels) %>% plot(x.var='numobs', rug=FALSE)
Effect(c('numobs', 'noise', 'model'), mod_nsj, xlevels=xlevels) %>% plot(x.var='noise', rug=FALSE)
Effect(c('numclus', 'noise', 're', 'model'), mod_nsj, xlevels=xlevels) %>% plot(x.var='noise', rug=FALSE)
Effect(c('numclus', 'noise', 're', 'model'), mod_nsj, xlevels=xlevels) %>% plot(x.var='re', rug=FALSE)

# Model-specific effects
rbindlist(idcol='effect', list(
        numclus=Effect(c('numclus', 'model'), mod_nsj) %>% as.data.table,
        re=Effect(c('re', 'model'), mod_nsj) %>% as.data.table,
        numobs=Effect(c('numobs', 'model'), mod_nsj) %>% as.data.table,
        numtraj=Effect(c('numtraj', 'model'), mod_nsj) %>% as.data.table,
        noise=Effect(c('noise', 'model'), mod_nsj) %>% as.data.table)) %>%
    ggplot(aes(x=numclus, y=fit, ymin=lower, ymax=upper, color=model)) +
        geom_line() +
        scale_y_continuous(breaks=seq(0,1,by=.1)) +
        coord_cartesian(ylim=c(0, .6)) +
        facet_wrap(~ effect, scales='free_x') +
        labs(x='Effect', y='NSJ')



# MWRSS ####
mod_wrss = lm(trendWRSS ~ numclus*model + poly(numobs, 2)*model + numtraj*model + re*model + noise*model, data=rx_normal[is.finite(refSilhouette),])

# model-specific
rbindlist(idcol='effect', list(
    numclus=Effect(c('numclus', 'model'), mod_wrss) %>% as.data.table,
    re=Effect(c('re', 'model'), mod_wrss) %>% as.data.table,
    numobs=Effect(c('numobs', 'model'), mod_wrss) %>% as.data.table,
    noise=Effect(c('noise', 'model'), mod_wrss) %>% as.data.table)) %>%
    ggplot(aes(x=numclus, y=fit, ymin=lower, ymax=upper, color=model)) +
    geom_line() +
    facet_wrap(~ effect, scales='free_x') +
    labs(x='Effect', y='MWSSE')

# refSilhouette ####
ggplot(rx_normal, aes(x=refSilhouette, color=factor(numclus))) +
    stat_ecdf()

modref_nsj = lm(NSJ ~ poly(refSilhouette, 3)*model*numtraj + numclus * re * model * poly(numobs,2) * noise, data=rx_normal[is.finite(refSilhouette),]); BIC(modref_nsj)

rbindlist(idcol='effect', list(
    difficulty=Effect(c('refSilhouette', 'model'), modref_nsj, xlevels=list(refSilhouette=seq(0,.8,by=.05))) %>% as.data.table,
    numobs=Effect(c('numobs', 'model'), modref_nsj) %>% as.data.table,
    numtraj=Effect(c('numtraj', 'model'), modref_nsj) %>% as.data.table,
    re=Effect(c('re', 'model'), modref_nsj) %>% as.data.table,
    noise=Effect(c('noise', 'model'), modref_nsj) %>% as.data.table,
    numclus=Effect(c('numclus', 'model'), modref_nsj) %>% as.data.table
    )) %>%
    ggplot(aes(x=refSilhouette, y=fit, ymin=lower, ymax=upper, color=model)) +
    geom_ribbon(fill=NA) +
    geom_line() +
    scale_y_continuous(breaks=seq(0,1,by=.1)) +
    coord_cartesian(ylim=c(0, .6)) +
    facet_wrap(~ effect, scales='free_x') +
    labs(x='Effect', y='NSJ')
