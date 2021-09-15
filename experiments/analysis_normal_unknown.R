out = experiment_computeResultWithData('unknown_groups_normal', resultfun=function(tsdata, output) {
    if(output$converged == FALSE) {
        return(c(NA, NA))
    }
    clusters = output$clusters
    trends = output$trends
    c(MAE=trendEval_mae(tsdata, trends, clusters), MSE=trendEval_mse(tsdata, trends, clusters))
})

result_unknown_raw = experiment_getOutputTable('unknown_groups_normal')
stopifnot(length(out) == nrow(result_unknown_raw))
results_unknown = cbind(result_unknown_raw, as.data.table(do.call(rbind, out)))

rxu = copy(results_unknown) %>%
    .[numgroups >= 3] %>%
    .[numclus <= 6] %>%
    .[, NSJ := sjTotal / (2 * numtraj)] %>%
    .[, WMMSE := trendWRSS / numobs * 10] %>%
    .[, model := factor(model, levels=c('longmodel_kml', 'longmodel_gcm2km', 'longmodel_gbtm2', 'longmodel_gmm2', 'longmodel_mixtvem_nugget'), labels=c('KML', 'GCKM', 'GBTM', 'GMM', 'MixTVEM'))] %>%
    .[is.infinite(BIC), BIC := NaN] %>%
    .[model == 'GCKM', BIC := -BIC] %>% # bug in GCKM code
    .[, NBIC := -BIC] %>%
    setkey(numgroups, numclus, re, dataseed, model)


# Case overview
results_unknown[, .N, keyby=.(model, numclus, numgroups)]

# Selected group
dtcrit = rxu[, .(
    clusMinNSJ=numclus[which.min(NSJ)],
    clusMinWMMSE=numclus[which.min(WMMSE)],
    clusMinBIC=numclus[which.min(BIC)],
    clusTBIC=numclus[which.bic(BIC, 10)],
    clusElbowBIC=numclus[which.elbow(BIC)],
    N=.N), keyby=.(numgroups, dataseed, model, re)]

# All
melt(dtcrit, id=c('numgroups', 'dataseed', 'model'), measure=c('clusMinNSJ', 'clusMinWMMSE', 'clusMinBIC', 'clusTBIC' ,'clusElbowBIC')) %>%
    .[, .(MinusMany=mean(value < numgroups - 1) %>% round(4),
          MinusOne=mean(value == numgroups - 1) %>% round(4),
          Correct=mean(value == numgroups) %>% round(4),
          PlusOne=mean(value == numgroups + 1) %>% round(4),
          PlusMany=mean(value > numgroups + 1) %>% round(4)), keyby=.(model, variable)] %>%
    dcast(model ~ variable, value.var=c('MinusOne', 'Correct', 'PlusOne')) %>% t

# Per RE
melt(dtcrit, id=c('numgroups', 'dataseed', 'model', 're'), measure=c('clusMinNSJ', 'clusMinWMMSE', 'clusMinBIC', 'clusTBIC' ,'clusElbowBIC')) %>%
    .[, .(MinusMany=mean(value < numgroups - 1) %>% round(4),
            MinusOne=mean(value == numgroups - 1) %>% round(4),
            Correct=mean(value == numgroups) %>% round(4),
            PlusOne=mean(value == numgroups + 1) %>% round(4),
            PlusMany=mean(value > numgroups + 1) %>% round(4)), keyby=.(model, re, variable)] %>%
    dcast(model + re ~ variable, value.var=c('MinusMany', 'MinusOne', 'Correct', 'PlusOne', 'PlusMany')) %>% t

# All criteria ####
dt_allCrit = rxu[, .(
        clusMinBIC=numclus[which.min(BIC)],
        clusMinAbsBIC=numclus[which.min(abs(BIC))],
        clusTBIC=numclus[which.bic(BIC, 10)],
        clusElbowBIC=numclus[which.elbow(BIC)],
        clusMinNBIC=numclus[which.bic(NBIC)],
        clusTMSE=numclus[which.bic(MSE, .01)],
        clusMinMAE=numclus[which.min(MAE)],
        clusTMAE=numclus[which.bic(MAE, .01)],
        clusElbowMAE=numclus[which.elbow(MAE)],
        clusElbowSil=numclus[which.elbow(silhouette)],
        clusMaxSil=numclus[which.max(silhouette)],
        maxSil=max(silhouette),
        clusMinNSJ=numclus[which.min(NSJ)],
        groupNSJ=NSJ[numclus == numgroups],
        minNSJ=min(NSJ),
        clusMinWMMSE=numclus[which.min(WMMSE)],
        minWMMSE=min(WMMSE),
        clusMinDunn=numclus[which.min(Dunn)],
        N=.N), keyby=.(numgroups, dataseed, model, re)]

dtsel_correct = dt_allCrit[, .(NSJClusC=mean(clusMinNSJ == numgroups),
                     WMMSEClusC=mean(clusMinWMMSE == numgroups),
                     BICClusC=mean(clusTBIC == numgroups),
                     minBICClusC=mean(clusMinBIC == numgroups),
                     minAbsBICClusC=mean(clusMinAbsBIC == numgroups),
                     NBICClusC=mean(clusMinNBIC == numgroups),
                     ElbowBICClusC=mean(clusElbowBIC == numgroups),
                     MSEClusC=mean(clusTMSE == numgroups),
                     MinMAEClusC=mean(clusMinMAE == numgroups),
                     MAEClusC=mean(clusTMAE == numgroups),
                     ElbowMAEClusC=mean(clusElbowMAE == numgroups),
                     SilClusC=mean(clusElbowSil == numgroups)), by=.(model, re)] %T>% print

ggplot(dtsel, aes(x=clusElbowBIC - numgroups, fill=model)) +
    geom_histogram(binwidth=1, position=position_dodge(width=.7)) +
    facet_wrap(~ re)

dt_constantref = dtsel[, .(ClusAAD.2=mean(abs(2 - numgroups), na.rm=TRUE),
                           ClusAAD.3=mean(abs(3 - numgroups), na.rm=TRUE),
                           ClusAAD.4=mean(abs(4 - numgroups), na.rm=TRUE),
                           ClusAAD.5=mean(abs(5 - numgroups), na.rm=TRUE),
                           ClusAAD.6=mean(abs(6 - numgroups), na.rm=TRUE))] %T>% print

dtsel[clusMinNSJ == numgroups, .(minNSJ=mean(minNSJ), groupNSJ=mean(groupNSJ)), keyby=.(model, re)]
dtsel[clusMinNSJ != numgroups, .(minNSJ=mean(minNSJ), groupNSJ=mean(groupNSJ)), keyby=.(model, re)]

dtsel_ad = dtsel[, .(NSJClusAD=mean(clusMinNSJ - numgroups),
                     WMMSEClusAD=mean(clusMinWMMSE - numgroups),
                     BICClusAD=mean(clusMinBIC - numgroups)), by=model] %T>% print

dtsel_aad = dtsel[, .(NSJClusAAD=mean(abs(clusMinNSJ - numgroups)),
                      WMMSEClusAAD=mean(abs(clusMinWMMSE - numgroups)),
                      BICClusAAD=mean(abs(clusMinBIC - numgroups)),
                      SilClusAAD=mean(abs(clusMaxSil - numgroups)),
                      DunnClusAAD=mean(abs(clusMinDunn - numgroups))), by=model] %T>% print

# AAD excluding correct cases
dtsel_aad_mis = dtsel[, .(NSJClusAAD=abs(clusMinNSJ - numgroups) %>% replaceValue(0, NA) %>% mean(na.rm=TRUE),
                      WMMSEClusAAD=abs(clusMinWMMSE - numgroups) %>% replaceValue(0, NA) %>% mean(na.rm=TRUE),
                      BICClusAAD=abs(clusMinBIC - numgroups) %>% replaceValue(0, NA) %>% mean(na.rm=TRUE),
                      SilClusAAD=abs(clusMaxSil - numgroups) %>% replaceValue(0, NA) %>% mean(na.rm=TRUE),
                      DunnClusAAD=abs(clusMinDunn - numgroups) %>% replaceValue(0, NA) %>% mean(na.rm=TRUE)), by=.(model,re)] %T>% print

