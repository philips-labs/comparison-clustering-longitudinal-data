redis_connect()
xlevels = list(noise=c(.01, .1), numobs=c(4,10,25), numclus=2:6, re=c(.1,.2,.3), numtraj=c(200, 500, 1000))

# Retrieve results
results_normal_all = experiment_getOutputTable('normal_known')
results_normal = results_normal_all %>%
    .[numclus %between% c(2,6)] %>%
    .[, NSJ := sjTotal / (2 * numtraj)] %>%
    .[, WMMSE := trendWRSS / numobs * 10] %>%
    .[, model := factor(model, levels=c('longmodel_kml', 'longmodel_gcm2km', 'longmodel_gbtm2', 'longmodel_gmm2', 'longmodel_mixtvem_nugget', 'longmodel_mixtvem_fe'), labels=c('KML', 'GCKM', 'GBTM', 'GMM', 'MixTVEM', 'MixTVEM.FE'))] %>%
    .[, Valid := converged == TRUE & emptyClusters == 0 & loneClusters == 0] %>%
    .[, EmptyClusters := as.numeric(emptyClusters > 0)] %>%
    .[, SolitaryClusters := as.numeric(loneClusters > 0)] %>%
    setkey(numtraj, numobs, numclus, re, noise, dataseed, model)

# Case overview
results_normal[, .N, keyby=.(model, numclus, numtraj)]

# Filter non-converged cases (except for mixtvem under T=10 and T=25)
rxn = results_normal[converged == TRUE]; messagef('%s/%s cases are considered', comma(nrow(rxn)), comma(nrow(results_normal)))

rxn_aggr = rxn[model != 'MixTVEM.FE', .(NSJ=mean(NSJ, na.rm=TRUE),
                   WMMSE=mean(WMMSE, na.rm=TRUE)), by=.(data, model, numtraj, numobs, numclus, re, noise)]

#
## NSJ ####
#
# Overall averages
rxn_aggr[, .(NSJ=mean(NSJ, na.rm=TRUE), WMMSE=mean(WMMSE, na.rm=TRUE)), keyby=model]

rxn_aggr[, .(NSJ=mean(NSJ, na.rm=TRUE)), keyby=.(model, numtraj, numclus)]
# Regression ###
# significant numclus*re interation
mod_nsj = lm(NSJ ~ numtraj + poly(numclus,2) * model + poly(re,2) * model + poly(numobs,2) * model + noise * model, data=rxn_aggr) %T>% printBIC
lsmeans(mod_nsj, ~ model, at=xlevels) %>% CLD
lsmeans(mod_nsj, ~ numclus + model, at=xlevels)
lsmeans(mod_nsj, ~ numtraj + model, at=xlevels)
lsmeans(mod_nsj, ~ numobs + model, at=xlevels)
lsmeans(mod_nsj, ~ re + model, at=xlevels)
lsmeans(mod_nsj, ~ noise + model, at=xlevels)
lstrends(mod_nsj, ~ model, var='numclus', at=xlevels) %>% CLD
lstrends(mod_nsj, ~ model, var='numclus', at=xlevels) %>% CLD
lstrends(mod_nsj, ~ model, var='numobs', at=xlevels) %>% CLD
lstrends(mod_nsj, ~ model, var='re', at=xlevels) %>% CLD
lstrends(mod_nsj, ~ model + numobs, var='noise', at=xlevels) %>% CLD # significant interaction between noise and numobs for GMM, GCKM, and MixTVEM
lstrends(mod_nsj, ~ model + numclus, var='numtraj', at=xlevels) %>% CLD # no significant interaction between numtraj and numclus
lstrends(mod_nsj, ~ model + re, var='numtraj', at=xlevels) %>% CLD # no significant interaction between numtraj and RE for any model

# Model-specific
# re:numclus interaction significiant for KML, GBTM, GCKM, GMM (by BIC)
# numclus:numobs for GCKM (by BIC)
# numclus:numobs for TVEM (by BIC)
# numobs:noise for GCKM, GMM (by BIC)
mod_nsj_kml = lm(NSJ ~  numtraj + poly(re,2) * poly(numclus,2) + poly(numobs,2) + noise, data=rxn_aggr[model == 'KML']) %T>% printBIC
mod_nsj_gbtm = lm(NSJ ~ numtraj + poly(re,2) * poly(numclus,2) + poly(numobs,2) + noise, data=rxn_aggr[model == 'GBTM']) %T>% printBIC
mod_nsj_tvem = lm(NSJ ~ numtraj + poly(re,2) + poly(numclus,2) * poly(numobs,2) + noise, data=rxn_aggr[model == 'MixTVEM']) %T>% printBIC
mod_nsj_gckm = lm(NSJ ~ numtraj + poly(re,2) * numclus + numclus * numobs + numclus * noise + numobs * noise, data=rxn_aggr[model == 'GCKM']) %T>% printBIC
mod_nsj_gmm = lm(NSJ ~  numtraj + poly(re,2) * numclus + numobs * noise + numclus * noise, data=rxn_aggr[model == 'GMM']) %T>% printBIC

lstrends(mod_nsj_kml,  ~ re, var='numclus', at=xlevels) %>% CLD # signif re:numclus
lstrends(mod_nsj_gbtm, ~ re, var='numclus', at=xlevels) %>% CLD # signif re:numclus
lstrends(mod_nsj_gckm, ~ re, var='numclus', at=xlevels) %>% CLD # signif re:numclus
lstrends(mod_nsj_gmm,  ~ re, var='numclus', at=xlevels) %>% CLD # signif re:numclus

lstrends(mod_nsj_tvem, ~ numobs,  var='numclus', at=xlevels) %>% CLD # signif numobs:numclus for T=25
lstrends(mod_nsj_gckm, ~ numobs,  var='numclus', at=xlevels) %>% CLD # signif numobs:numclus
lstrends(mod_nsj_gmm,  ~ numobs,  var='numclus', at=xlevels) %>% CLD # no numobs:numclus

lstrends(mod_nsj_gckm, ~ noise,  var='numclus', at=xlevels) %>% CLD # signif noise:numclus
lstrends(mod_nsj_gmm,  ~ noise,  var='numclus', at=xlevels) %>% CLD # signif noise:numclus

lstrends(mod_nsj_gckm, ~ noise,  var='numobs', at=xlevels) %>% CLD # signif noise:numobs
lstrends(mod_nsj_gmm,  ~ noise,  var='numobs', at=xlevels) %>% CLD # signif noise:numobs

#
# WMMSE ####
#
mod_wmmse = lm(WMMSE ~ numtraj * model + numclus * model + noise * model + poly(re,2) * model + poly(numobs,2) * model, data=rxn_aggr) %T>% printBIC
lsmeans(mod_wmmse, ~ model, at=list(numobs=25)) %>% CLD
lsmeans(mod_wmmse, ~ numclus + model, at=xlevels)
lsmeans(mod_wmmse, ~ numtraj + model, at=xlevels)
lsmeans(mod_wmmse, ~ numobs + model, at=xlevels)
lsmeans(mod_wmmse, ~ re + model, at=xlevels)
lsmeans(mod_wmmse, ~ noise + model, at=xlevels)
lstrends(mod_wmmse, ~ model, var='numclus') %>% CLD
lstrends(mod_wmmse, ~ model, var='numtraj') %>% CLD

# Model-specific
# BIC re:numclus for KML, GBTM, GCKM, GMM
# BIC numobs:noise for GCKM, GMM, TVEM
# BIC noise:re for GCKM
# BIC numobs:re for TVEM
mod_wmmse_kml = lm(WMMSE ~  numtraj + poly(re,2) * poly(numclus,2) + numobs + noise, data=rxn_aggr[model == 'KML']) %T>% printBIC
mod_wmmse_gbtm = lm(WMMSE ~ numtraj + poly(re,2) * poly(numclus,2) + numobs + noise, data=rxn_aggr[model == 'GBTM']) %T>% printBIC
mod_wmmse_tvem = lm(WMMSE ~ numtraj + numclus + poly(numobs,2) * re + numobs * noise, data=rxn_aggr[model == 'MixTVEM']) %T>% printBIC
mod_wmmse_gckm = lm(WMMSE ~ numtraj + poly(re,2) * numclus + numobs * noise + noise * re, data=rxn_aggr[model == 'GCKM']) %T>% printBIC
mod_wmmse_gmm = lm(WMMSE ~  numtraj + poly(re,2) * numclus + numobs * noise, data=rxn_aggr[model == 'GMM']) %T>% printBIC
lsmeans(mod_wmmse_gckm, ~ numobs, at=xlevels) %>% CLD # worse performance lower observations (especially T=4)
lsmeans(mod_wmmse_gckm, ~ numtraj, at=xlevels) %>% CLD # significantly better performance for greater N
lsmeans(mod_wmmse_gckm, ~ noise + numobs, at=xlevels) %>% CLD # worse performance for noise

lsmeans(mod_wmmse_gckm, ~ numobs + noise, at=xlevels) %>% CLD # significant noise-numobs interaction: highest error for T=4 & noise
lsmeans(mod_wmmse_gckm, ~ re + numclus, at=xlevels) %>% CLD

lstrends(mod_wmmse_kml,  ~ re, var='numclus', at=xlevels) %>% CLD # signif re:numclus
lstrends(mod_wmmse_gbtm, ~ re, var='numclus', at=xlevels) %>% CLD # no re:numclus
lstrends(mod_wmmse_gckm, ~ re, var='numclus', at=xlevels) %>% CLD # signif re:numclus
lstrends(mod_wmmse_gmm,  ~ re, var='numclus', at=xlevels) %>% CLD # signif re:numclus

lstrends(mod_wmmse_gckm, ~ noise, var='numobs', at=xlevels) %>% CLD # signif noise:numobs
lstrends(mod_wmmse_gmm,  ~ noise, var='numobs', at=xlevels) %>% CLD # signif noise:numobs
lstrends(mod_wmmse_tvem, ~ noise, var='numobs', at=xlevels) %>% CLD # signif noise:numobs

lstrends(mod_wmmse_tvem, ~ re, var='numobs', at=xlevels) %>% CLD # signif re:numobs

lstrends(mod_wmmse_gckm, ~ noise, var='re', at=xlevels) %>% CLD # signif noise:re

#
# Computation time ####
#
rxn_cv = results_normal[numclus %between% c(2,6) & converged == TRUE]
mod_time = lm(time ~ poly(numtraj,2) * model + poly(numclus,2) * model + poly(numobs,2) * model, data=rxn_cv) %T>% printBIC
allEffects(mod_time, xlevels=xlevels) %>% plot(rug=FALSE)

rxn_cv[, mean(time), keyby=model]
lsmeans(mod_time, ~ model, at=xlevels)
lsmeans(mod_time, ~ numclus + model, at=xlevels)
lsmeans(mod_time, ~ numtraj + model, at=xlevels)
lsmeans(mod_time, ~ numobs + model, at=xlevels)

# time over numclus and numobs plot
dt_time_clusobs = rbind(numclus=lsmeans(mod_time, ~ numclus + model, at=xlevels) %>% summary %>% as.data.table %>% setnames('numclus', 'value'),
                        numobs=lsmeans(mod_time, ~ numobs + model, at=xlevels) %>% summary %>% as.data.table  %>% setnames('numobs', 'value'),
                        idcol='var') %>% setnames('model', 'Method')

p = ggplot(dt_time_clusobs[Method != 'MixTVEM.FE'], aes(x=value, y=lsmean, shape=Method)) +
    geom_line(size=.5) +
    geom_point(size=3) +
    facet_grid(~ var, scales='free_x', switch='both') +
    scale_y_log10(breaks=c(.5, 1, 5, 10, 50, 1e2, 5e2, 1e3, 5e3, 1e4),
        minor_breaks = 1:9 * 10^rep(-1:4, each = 9),
        labels=function(x) ifelse(log10(x) %% 1 == 0 | log10(round(x,2) / 5) %% 1 == 0, prettyNum(x, big.mark=','), '')) +
    guides(fill = guide_legend(override.aes = list(linetype = 0)),
           color = guide_legend(override.aes = list(linetype = 0))) +
    theme(
        legend.position = "top",
        legend.direction = "horizontal",
        legend.margin=margin(-.1, .5, 0, 0, unit='cm'), 
        strip.background = element_blank(), 
        strip.text.x = element_blank()) +
    labs(x='Number of groups                   Observations', y='Computation time (s)'); print(p)
ggsave(filename=file.path(FIG_DIR, 'time.pdf'), plot=p, width=9, height=7, units='cm')


# relative increase in time
mod_logtime = lm(I(log(time)) ~ numtraj * model + poly(numclus,2) * model + poly(numobs,2) * model, data=rxn) %T>% printBIC
Effect(c('numtraj', 'model'), mod_logtime, xlevels=xlevels) %>% plot(x.var='numtraj', rug=FALSE)
Effect(c('numclus', 'model'), mod_logtime, xlevels=xlevels) %>% plot(x.var='numclus', rug=FALSE)
Effect(c('numobs', 'model'), mod_logtime, xlevels=xlevels) %>% plot(x.var='numobs', rug=FALSE)

lstrends(mod_logtime, pairwise ~ model, var='numtraj') %>% CLD
lstrends(mod_logtime, pairwise ~ model, var='numclus') %>% CLD
lstrends(mod_logtime, pairwise ~ model, var='numobs') %>% CLD
