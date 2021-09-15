set.seed(1)
tsdata = longdata_cpap()

tvem_results = list()
for(k in 1:8) {
    message('- Evaluating for k=', k, ' -')
    tvem_results[[as.character(k)]] = longclus_mixtvem(tsdata, numclus=k, numruns=50, value='Value', seed=1,
                                                          model.numknots=6,
                                                          model.degree=3,
                                                          model.autocor=FALSE)
}
saveRDS(tvem_results, file.path(RESULTS_DIR, 'mixtvem.rds'))
tvem_results = readRDS(file.path(RESULTS_DIR, 'mixtvem.rds'))

# Computation time
elbow_plot(names(tvem_results) %>% as.integer, sapply(tvem_results, '[[', 'time') / 3600, ystr='Computation time (h)')

# BIC
elbow_plot(names(tvem_results) %>% as.integer, sapply(tvem_results, '[[', 'BIC')) %T>% print
# ggsave(filename=file.path(FIG_DIR, 'mixtvem_bic.pdf'), width=FIG_BIC_W, height=FIG_BIC_H, units='cm')
sapply(tvem_results, '[[', 'BIC')

# Silhouette
elbow_plot(names(tvem_results) %>% as.integer, sapply(tvem_results, '[[', 'silhouette'), ystr='Silhouette') %T>% print
elbow_plot(names(tvem_results) %>% as.integer, sapply(tvem_results, '[[', 'CalinskiHarabasz'), ystr='Calinski-Harabasz (higher is better)')
elbow_plot(names(tvem_results) %>% as.integer, sapply(tvem_results, '[[', 'DaviesBouldin'), ystr='Davies-Bouldin (lower is better)')

# Internal error
elbow_plot(names(tvem_results) %>% as.integer, sapply(tvem_results, '[[', 'WMSE'), ystr='Weighted MSE (h^2)') + expand_limits(y=0)
elbow_plot(names(tvem_results) %>% as.integer, sapply(tvem_results, '[[', 'WMSE') %>% sqrt, ystr='Weighted RMSE (h)') + expand_limits(y=0)
elbow_plot(names(tvem_results) %>% as.integer, sapply(tvem_results, '[[', 'WMAE'), ystr='Weighted MAE (h)') + expand_limits(y=0)

# Analyze cluster solution
tvem_result = tvem_results[['8']]
plot_usage_trends(tvem_result, tsdata) %T>% print
# ggsave(filename=file.path(FIG_DIR, 'mixtvem_trends.pdf'), width=FIG_TREND_W, height=FIG_TREND_H, units='cm')

# rel entropy
relativeEntropy(tvem_result$model$postProbsBySub)

tvemTable = rbind(
        prop = percent(colMeans(tvem_result$model$postProbsBySub), 1),
        sigfig(tvem_result$model$theta)) %>%
    set_colnames(levels(tvem_result$clusters)) %T>%
    print()

# write.csv(tvemTable, file.path(TAB_DIR, 'mixtvem.csv'), quote = TRUE, row.names = TRUE)