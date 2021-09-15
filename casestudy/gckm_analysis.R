set.seed(1)
tsdata = longdata_cpap()

## GCM
system.time({
    gcm = lmer(Value ~ 1 + (poly(Time, 3) | Id), data=tsdata, REML=FALSE, control = lmerControl(optimizer = 'bobyqa'))
})
saveRDS(gcm, file.path(RESULTS_DIR, 'gcm3.rds'))
gcm = readRDS(file.path(RESULTS_DIR, 'gcm3.rds'))

{
    R = coef(gcm)$Id
    pred_ss = predict(gcm)
    ids = rownames(R)
    coef_fe = lme4:::fixef.merMod(gcm) %>% as.list %>% as.data.table
    coef_re = data.table(Id=ids, R)
    std_coef_re = data.table(Id=ids, scale(R))
    coefs_traj = cbind(Id=ids, coef_re[,-'Id'])
    std_coefs_traj = cbind(Id=ids, std_coef_re[,-'Id'])
    pred_traj = data.table(tsdata[, .(Id, Time)], Pred=pred_ss)

    cld = clusterLongData(traj=scale(R), idAll=rownames(R), time=seq_len(ncol(R)), varNames='Value')
    par = parALGO(saveFreq=1e99, scale=TRUE, startingCond='kmeans++')
}

Ks = 1:8; names(Ks) = Ks; set.seed(1)
gcmkm_results = lapply(Ks, function(k) {
        start = Sys.time()
        kml(cld, nbClusters=k, nbRedrawing=25, toPlot='none', parAlgo=par)
        modk = slot(cld, paste0('c', k))[[1]]

        ## Trends
        clusters = getClusters(cld, k)
        coefs_traj[, Cluster := clusters]
        pred_traj[, Cluster := clusters[.GRP], by=Id]

        postProbs = modk@postProba
        colnames(postProbs) = levels(clusters)

        # compute trends
        dt_trends = pred_traj[, .(Value=mean(Pred)), by=.(Cluster, Time)]

        result = result_summary(tsdata, clusters=clusters, dt_trends=dt_trends, start=start, postProbs=postProbs, numclus=k)
        result$model = modk
        result$coefCenters = melt(coefs_traj, id.vars = c('Id', 'Cluster'), variable.name='Coef') %>% 
            .[, .(Mean = mean(value)), keyby=.(Cluster, Coef)]
        result$BIC = -modk@criterionValues[['BIC']]
        result$AIC = modk@criterionValues[['AIC']]
        
        # special case for k = 1 since kml outputs NA
        if (is.na(result$BIC) && k == 1) {
            std_coefs_traj[, Cluster := clusters]
            dtcoefs = melt(std_coefs_traj, id.vars = c('Id', 'Cluster'), variable.name = 'Coef') %>%
                .[, Time := as.integer(Coef)]
            dt_coefTrends = dtcoefs[, .(Mean=mean(value)), by=.(Cluster, Time)]
            alldata = merge(dtcoefs, dt_coefTrends, by = c('Cluster', 'Time'))
            sigma = alldata[, longitudinalData:::sdcNA(value - Mean)]
            ll = dnorm(alldata$value, alldata$Mean, sigma, log = TRUE) %>% sum()
            np = modk@nbClusters * uniqueN(dtcoefs$Time) + 1
            result$BIC = -2 * ll + np * log(length(modk@clusters))
        }
        
        return(result)
    })

saveRDS(gcmkm_results, file.path(RESULTS_DIR, 'gcm3km.rds'))
gcmkm_results = readRDS(file.path(RESULTS_DIR, 'gcm3km.rds'))

# Computation time
elbow_plot(names(gcmkm_results) %>% as.integer, sapply(gcmkm_results, '[[', 'time'), ystr='Computation time (s)') + expand_limits(y=0)

## BIC
elbow_plot(names(gcmkm_results) %>% as.integer, sapply(gcmkm_results, '[[', 'BIC'))
# ggsave(filename=file.path(FIG_DIR, 'gckm_bic.pdf'), width=FIG_BIC_W, height=FIG_BIC_H, units='cm')
sapply(gcmkm_results, '[[', 'BIC')

# Interval cluster evaluation criteria
elbow_plot(names(gcmkm_results) %>% as.integer, sapply(gcmkm_results, '[[', 'silhouette'), ystr='Silhouette')

# Internal error
elbow_plot(names(gcmkm_results) %>% as.integer, sapply(gcmkm_results, '[[', 'MSE'), ystr='MSE (h^2)') + expand_limits(y=0)
elbow_plot(names(gcmkm_results) %>% as.integer, sapply(gcmkm_results, '[[', 'MSE') %>% sqrt, ystr='RMSE (h)') + expand_limits(y=0)
elbow_plot(names(gcmkm_results) %>% as.integer, sapply(gcmkm_results, '[[', 'MAE'), ystr='MAE (h)') + expand_limits(y=0)

## Trends
gckm_result = gcmkm_results[['7']]
plot_usage_trends(gckm_result, tsdata)
# ggsave(filename=file.path(FIG_DIR, 'gckm_trends.pdf'), width=FIG_TREND_W, height=FIG_TREND_H, units='cm')

plot_trends(gckm_result$trends, tsdata, gckm_result$clusters, traj.alpha=.2)

# rel entropy
relativeEntropy(gckm_result$model@postProba)

gckmTable = gckm_result$coefCenters %>% 
    copy() %>%
    .[, Mean := sigfig(Mean, 2)] %>%
    dcast(Cluster ~ Coef) %>%
    .[, Prop := percent(gckm_result$model@percentEachCluster, 1)] %>%
    setnames('Cluster', 'Group') %>%
    setcolorder(c('Group', 'Prop')) %T>% 
    print()

# write.csv(t(gckmTable), file.path(TAB_DIR, 'gckm.csv'), quote = TRUE, row.names = TRUE)