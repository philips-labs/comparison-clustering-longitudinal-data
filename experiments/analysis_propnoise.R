library(lsmeans)
xlevels = list(propnoise=c(.01, .03, .05), numobs=10, numclus=2:6, re=c(RE_NORM_LOW, RE_NORM_HIGH), numtraj=500, noise=c(.01, .1))

raw_results_propnoise = experiment_getOutputTable('propnoise_known')
results_propnoise = copy(raw_results_propnoise) %>%
    .[, NSJ := sjTotal / (2 * numtraj)] %>%
    .[, WMMSE := trendWRSS / numobs * 10] %>%
    .[, model := factor(model, levels=c('longmodel_kml', 'longmodel_gcm2km', 'longmodel_gbtm2', 'longmodel_gmm2', 'longmodel_mixtvem_nugget'), labels=c('KML', 'GCKM', 'GBTM', 'GMM', 'MixTVEM'))] %>%
    .[, Valid := converged == TRUE & emptyClusters == 0 & loneClusters == 0] %>%
    setkey(numgroups, re, propnoise, dataseed)

results_propnoise[, .N, keyby=model]

# Filter non-converged cases
valid_cases = dcast(results_propnoise, dataseed + numgroups + re + propnoise ~ model, value.var='converged') %>%
    .[!isFALSE(KML) & !isFALSE(GCKM) & !isFALSE(GBTM) & !isFALSE(GMM)] %>%
    setkey(numgroups, re, propnoise, dataseed)

rxp = results_propnoise[converged == TRUE]; messagef('%s/%s cases are considered', comma(nrow(rxp)), comma(nrow(results_propnoise)))

rxp_aggr = rxp[, .(NSJ=mean(NSJ, na.rm=TRUE),
                   WMMSE=mean(trendWRSS/numobs*10, na.rm=TRUE)), keyby=.(data, model, numgroups, numclus, re, propnoise)]

#
## NSJ ####
#
# Overall averages
rxp_aggr[, .(NSJ=mean(NSJ, na.rm=TRUE), WMMSE=mean(WMMSE, na.rm=TRUE)), keyby=model]

# Regression ###
# significant numclus*re interation
mod_nsj = lm(NSJ ~ poly(numclus,2) * model + re * model + propnoise * model, data=rxp_aggr) %T>% printBIC
lsmeans(mod_nsj, ~ model, at=xlevels) %>% CLD
lsmeans(mod_nsj, ~ numclus + model, at=xlevels)
lsmeans(mod_nsj, ~ re + model, at=xlevels)
lsmeans(mod_nsj, ~ propnoise + model, at=xlevels)
lstrends(mod_nsj, ~ model, var='numclus', at=xlevels) %>% CLD
lstrends(mod_nsj, ~ model, var='numclus', at=xlevels) %>% CLD
lstrends(mod_nsj, ~ model, var='re', at=xlevels) %>% CLD
lstrends(mod_nsj, ~ model, var='propnoise', at=xlevels) %>% CLD
lstrends(mod_nsj, ~ model + numclus, var='numtraj', at=xlevels) %>% CLD # no significant interaction between numtraj and numclus
lstrends(mod_nsj, ~ model + re, var='numtraj', at=xlevels) %>% CLD # no significant interaction between numtraj and RE for any model

#
# WMMSE ####
#
rxp_aggr[, .(NSJ=mean(NSJ, na.rm=TRUE), WMMSE=mean(WMMSE, na.rm=TRUE)), keyby=.(model, re)]
mod_wmmse = lm(WMMSE ~ numclus * model + propnoise * model + re * model, data=rxp_aggr) %T>% printBIC
lsmeans(mod_wmmse, ~ model) %>% CLD
lsmeans(mod_wmmse, ~ numclus + model, at=xlevels)
lsmeans(mod_wmmse, ~ re + model, at=xlevels)
lsmeans(mod_wmmse, ~ propnoise + model, at=xlevels)


# Compare to normal case ####
mod_nsj = lm(NSJ ~ numtraj + poly(numclus,2) * model + poly(re,2) * model + poly(numobs,2) * model + noise * model, data=rxn_aggr) %T>% printBIC
lsmeans(mod_nsj, ~ re + model, at=xlevels)
lsmeans(mod_nsj, ~ noise + model, at=xlevels)

rxn_aggr[numtraj == 500 & numobs == 10 & re != .2, .(NSJ=mean(NSJ, na.rm=TRUE), WMMSE=mean(WMMSE, na.rm=TRUE)), keyby=.(model, re)]
mod_wmmse = lm(WMMSE ~ numtraj * model + numclus * model + noise * model + poly(re,2) * model + poly(numobs,2) * model, data=rxn_aggr) %T>% printBIC
lsmeans(mod_wmmse, ~ re + model, at=xlevels)
lsmeans(mod_wmmse, ~ noise + model, at=xlevels)
