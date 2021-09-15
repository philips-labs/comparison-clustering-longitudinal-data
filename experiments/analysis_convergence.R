# Gather all results
res_normal = experiment_getOutputTable('normal_known')[numclus <= 6]
res_unknown = experiment_getOutputTable('unknown_groups_normal')[numclus <= 6]
res_log = experiment_getOutputTable('lognormal_known')
res_prop = experiment_getOutputTable('propnoise_known')

rxc = rbindlist(list(
    Normal = res_normal,
    Unknown = res_unknown,
    Prop = res_prop,
    LogKnown = res_log), idcol='Experiment', fill=TRUE) %>%
    .[, model := factor(model, levels=c('longmodel_kml', 'longmodel_gcm2km', 'longmodel_gbtm2', 'longmodel_gmm2', 'longmodel_mixtvem_nugget'), labels=c('KML', 'GCKM', 'GBTM', 'GMM', 'MixTVEM'))] %>%
    .[, EmptyClusters := emptyClusters > 0] %>%
    .[, SolitaryClusters := loneClusters > 0] %>%
    .[, Valid := converged & !EmptyClusters & !SolitaryClusters] %>%
    .[, Experiment := factor(Experiment)] %>%
    .[, numobsF := factor(numobs)] %>%
    .[, reF :=  replace(re, re == RE_LNORM_LOW, RE_NORM_LOW) %>%
                replace(re == RE_LNORM_MED, RE_NORM_MED) %>%
                replace(re == RE_LNORM_HIGH, RE_NORM_HIGH) %>%
            factor(levels=c(RE_NORM_LOW, RE_NORM_MED, RE_NORM_HIGH), labels=c('Low', 'Medium', 'High'))]

#rxc[, ExpRe := interaction(Experiment, reF)] this way, effect outputs the correct estimates
rxc_noprop = rxc[Experiment != 'Prop'] %>% .[, Experiment := factor(Experiment)]
xlevels = list(Experiment=levels(rxc$Experiment), reF=levels(rxc$reF), noise=c(.01, .1), numobs=c(4,10,25), numclus=2:6, numtraj=c(200, 500, 1000))

# Results
rxc[, .(NotConverged=1-mean(converged)), keyby=model]
rxc[converged == TRUE, mean(EmptyClusters), keyby=model]
rxc[converged == TRUE, mean(SolitaryClusters), keyby=model]
rxc[, 1-mean(Valid), keyby=model]



# Convergence problems ####
# MixTVEM ##
# MixTVEM hardly ever converges for T=25
modc_mix = glm(I(1-converged) ~ Experiment + reF + poly(numobs,2) + poly(numclus,3), data=rxc[model == 'MixTVEM'], family=binomial(link='logit')); BIC(modc_mix)
allEffects(modc_mix, xlevels=xlevels) %>% plot(rug=FALSE)
lsmeans(modc_mix, ~ numobs, type='response', at=list(numobs=c(4,10,25)))
lsmeans(modc_mix, ~ numclus, type='response', at=list(numclus=2:6, numobs=10))
lsmeans(modc_mix, ~ Experiment, type='response')
rxc[model == 'MixTVEM', mean(1-converged), keyby=numobs]
rxc[model == 'MixTVEM' & numobs == 10, mean(1-converged), keyby=.(numclus)]

modc_gbtm = glm(converged ~ numobsF, data=rxc_noprop[model == 'GBTM'], family=binomial(link='logit')); BIC(modc)
lsmeans(modc_gbtm, ~ numobsF, type='response', at=xlevels) %>% cld

#
# Empty clusters ####
#
# MixTVEM ##
# MixTVEM empty clusters largely occur in numobs=25
mod_empty_mix = glm(EmptyClusters ~ Experiment + reF + poly(numobs, 2) + numtraj + poly(numclus,3), data=rxc[converged == TRUE & model == 'MixTVEM'], family=binomial(link='logit')); BIC(mod_empty_mix)
allEffects(mod_empty_mix, xlevels=xlevels) %>% plot(rug=FALSE)
lsmeans(mod_empty_mix, ~ numobs, type='response', at=list(numobs=c(4,10,25)))
lsmeans(mod_empty_mix, ~ Experiment, type='response', at=xlevels) %>% cld
lsmeans(mod_empty_mix, ~ Experiment + reF, type='response', at=xlevels)
lsmeans(mod_empty_mix, ~ numclus, type='response', at=list(numclus=2:6, numobs=10))
lsmeans(mod_empty_mix, ~ Experiment + numclus, type='response', at=xlevels) %>% cld
lsmeans(mod_empty_mix, ~ Experiment + numobsF, type='response', at=xlevels) %>% cld
rxc[converged == TRUE & model == 'MixTVEM', mean(EmptyClusters), keyby=numobs]
rxc[converged == TRUE & model == 'MixTVEM', mean(EmptyClusters), keyby=Experiment]

# GMM ##
# empty clusters occur mostly for high numobs (>=10), numclus (>=5), lower noise, and more likely in lognorm data
# lognorm: more likely for higher re
# norm: more likely for lower re
mod_empty_gmm = glm(EmptyClusters ~ Experiment : reF + poly(numobs,2) + poly(numclus,3) * noise + numtraj, data=rxc_noprop[converged == TRUE & model == 'GMM'], family=binomial(link='logit')); BIC(mod_empty_gmm)
allEffects(mod_empty_gmm, xlevels=xlevels) %>% plot(rug=FALSE)
lsmeans(mod_empty_gmm, ~ numclus, type='response', at=list(numclus=2:6, numtraj=500, numobs=10, reF=c('Low', 'High')))
lsmeans(mod_empty_gmm, ~ noise, type='response', at=list(numclus=6, numtraj=500, numobs=10, noise=c(.01, .1), reF=c('Low', 'High')))
lsmeans(mod_empty_gmm, ~ Experiment : reF, type='response', at=list(numclus=6, numobs=10, noise=.01, numtraj=500, reF=c('Low', 'High')))
lsmeans(mod_empty_gmm, ~ Experiment, type='response', at=xlevels)
lsmeans(mod_empty_gmm, ~ Experiment + numclus + reF + noise, type='response', at=list(numclus=6, noise=c(.01, .1)))

rxc[converged == TRUE & model == 'GMM', mean(EmptyClusters), keyby=numclus]
rxc[converged == TRUE & model == 'GMM', mean(EmptyClusters), keyby=Experiment]
rxc[converged == TRUE & model == 'GMM' & Experiment == 'LogKnown' & reF == 'High' & numobs == 10, mean(EmptyClusters), keyby=.(numclus, noise)]
rxc[converged == TRUE & model == 'GMM' & numclus == 6 & reF == 'Low' & numobs == 10, .(Mean=mean(EmptyClusters), N=.N), keyby=.(Experiment, noise)]
rxc[converged == TRUE & model == 'GMM' & Experiment == 'Normal' & reF == 'Low' & numobs == 10, mean(EmptyClusters), keyby=.(numclus, noise)]

# Solitary clusters ####
#
# No significant occurrence for KML, GCKM, GBTM
#
# MixTVEM ##
# MixTVEM solitary clusters largely occur in lognorm data, high clus, and low numtraj
mod_soli_mix = glm(SolitaryClusters ~ Experiment + reF + poly(numobs,2) + poly(numclus,3) + numtraj, data=rxc[converged == TRUE & model == 'MixTVEM'], family=binomial(link='logit')); BIC(mod_soli_mix)
allEffects(mod_soli_mix, xlevels=xlevels) %>% plot(rug=FALSE)
lsmeans(mod_soli_mix, ~ Experiment, type='response', at=list(numobs=10, numtraj=500))
lsmeans(mod_soli_mix, ~ numobs, type='response', at=list(numobs=c(4,10,25), numtraj=500))

lsmeans(mod_soli_mix, ~ numclus, type='response', at=list(numclus=2:6, numtraj=500))
lsmeans(mod_soli_mix, ~ numclus + Experiment, type='response', at=list(numclus=2:6, numtraj=500))
lsmeans(mod_soli_mix, ~ numtraj, type='response', at=list(numtraj=c(200, 500, 1000)))
rxc[converged == TRUE & model == 'MixTVEM', mean(SolitaryClusters), keyby=numobs]
rxc[converged == TRUE & model == 'MixTVEM', mean(SolitaryClusters), keyby=Experiment]
rxc[converged == TRUE & model == 'MixTVEM', mean(SolitaryClusters), keyby=numclus]
rxc[converged == TRUE & model == 'MixTVEM', mean(SolitaryClusters), keyby=.(Experiment, numtraj)]

# GMM ##
# GMM: most likely to occur with high numclus and lognorm data (for higher clusters + reF) and unknown data (higher clus)
mod_soli_gmm = glm(SolitaryClusters ~ Experiment + reF + poly(numobs,2) + poly(numclus,3) + numtraj, data=rxc[converged == TRUE & model == 'GMM'], family=binomial(link='logit')); BIC(mod_soli_gmm)
allEffects(mod_soli_gmm, xlevels=xlevels) %>% plot(rug=FALSE)
lsmeans(mod_soli_gmm, ~ numclus, type='response', at=list(numclus=2:6))
lsmeans(mod_soli_gmm, ~ Experiment, type='response')
lsmeans(mod_soli_gmm, ~ numclus + Experiment, type='response', at=list(numclus=2:6))
lsmeans(mod_soli_gmm, ~ numobs, type='response', at=list(numobs=c(4,10,25)))
lsmeans(mod_soli_gmm, ~ Experiment : numclus, type='response', at=xlevels) %>% cld
lsmeans(mod_soli_gmm, ~ Experiment + reF, type='response', at=xlevels) %>% cld
rxc[converged == TRUE & model == 'GMM', mean(SolitaryClusters), keyby=.(Experiment, reF, numclus)]
rxc[converged == TRUE & model == 'GMM', mean(SolitaryClusters), keyby=Experiment]
