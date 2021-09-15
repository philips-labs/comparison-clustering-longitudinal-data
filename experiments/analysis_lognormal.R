xlevels = list(noise=c(.01, .1), numobs=10, numclus=2:6, re=c(RE_LNORM_LOW, RE_LNORM_MED, RE_LNORM_HIGH), numtraj=500)
xlevels_normal = list(noise=c(.01, .1), numobs=10, numclus=2:6, re=c(.1, .2, .3), numtraj=500)

# Retrieve results
raw_results_log = experiment_getOutputTable('lognormal_known')
results_log = copy(raw_results_log) %>%
    .[, NSJ := sjTotal / (2 * numtraj)] %>%
    .[, WMMSE := trendWRSS / numobs * 10] %>%
    .[, model := factor(model, levels=c('longmodel_kml', 'longmodel_gcm2km', 'longmodel_gbtm2', 'longmodel_gmm2', 'longmodel_mixtvem_nugget', 'longmodel_mixtvem_fe'), labels=c('KML', 'GCKM', 'GBTM', 'GMM', 'MixTVEM', 'MixTVEM.FE'))] %>%
    .[, Valid := converged == TRUE & emptyClusters == 0 & loneClusters == 0] %>%
    .[, reF := factor(re, levels=c(RE_LNORM_LOW, RE_LNORM_MED, RE_LNORM_HIGH), labels=c('Low', 'Medium', 'High'))] %>%
    setkey(numgroups, reF, noise, dataseed, model)

# compare
results_normal_all = experiment_getOutputTable('normal_known')
results_normal = results_normal_all %>%
    .[numclus %between% c(2,6)] %>%
    .[model != 'longmodel_mixtvem_fe'] %>%
    .[, NSJ := sjTotal / (2 * numtraj)] %>%
    .[, WMMSE := trendWRSS / numobs * 10] %>%
    .[, model := factor(model, levels=c('longmodel_kml', 'longmodel_gcm2km', 'longmodel_gbtm2', 'longmodel_gmm2', 'longmodel_mixtvem_nugget'), labels=c('KML', 'GCKM', 'GBTM', 'GMM', 'MixTVEM'))] %>%
    .[, Valid := converged == TRUE & emptyClusters == 0 & loneClusters == 0] %>%
    .[, EmptyClusters := as.numeric(emptyClusters > 0)] %>%
    .[, SolitaryClusters := as.numeric(loneClusters > 0)] %>%
    setkey(numtraj, numobs, numclus, re, noise, dataseed, model)
rxn = results_normal[converged == TRUE]; messagef('%s/%s cases are considered', comma(nrow(rxn)), comma(nrow(results_normal)))
rxn_aggr = rxn[, .(NSJ=mean(NSJ, na.rm=TRUE),
                                        WMMSE=mean(WMMSE, na.rm=TRUE)), by=.(data, model, numtraj, numobs, numclus, re, noise)]


# Case overview
results_log[, .N, keyby=.(model, numgroups, reF)]

# Filter non-converged cases (except for mixtvem under T=10 and T=25)
rxl = results_log[converged == TRUE]; messagef('%s/%s cases are considered', comma(nrow(rxl)), comma(nrow(results_log)))
rxl_aggr = rxl[, .(NSJ=mean(NSJ, na.rm=TRUE),
                   WMMSE=mean(trendWRSS/numobs * 10, na.rm=TRUE)), by=.(numclus, model, reF, noise)]

#
## NSJ ####
#
# Overall averages
rxl_aggr[, .(NSJ=mean(NSJ, na.rm=TRUE), WMMSE=mean(WMMSE, na.rm=TRUE)), keyby=model]

# Regression ###
# significant numclus*re interation
mod_nsj = lm(NSJ ~ poly(numclus,2) * model + reF * model + noise * model, data=rxl_aggr) %T>% printBIC
lsmeans(mod_nsj, ~ model, at=xlevels) %>% CLD
lsmeans(mod_nsj, ~ numclus + model, at=xlevels)
lsmeans(mod_nsj, ~ reF + model, at=xlevels)
lsmeans(mod_nsj, ~ noise + model, at=xlevels)

# compare to normal
mod_nsj_normal = lm(NSJ ~ numtraj + poly(numclus,2) * model + poly(re,2) * model + poly(numobs,2) * model + noise * model, data=rxn_aggr) %T>% printBIC
lsmeans(mod_nsj_normal, ~ re + model, at=xlevels_normal)
#
# WMMSE ####
#
mod_wmmse = lm(WMMSE ~ numclus * model + noise * model + reF * model, data=rxl_aggr) %T>% printBIC
lsmeans(mod_wmmse, ~ model, at=xlevels)
lsmeans(mod_wmmse, ~ numclus + model, at=xlevels)
lsmeans(mod_wmmse, ~ reF + model, at=xlevels)
lsmeans(mod_wmmse, ~ noise + model, at=xlevels)
