set.seed(1)
tsdata = longdata_cpap()

kml_results = list()
kml_results[['1']] = longclus_kml(tsdata, numclus=1, numruns=25, seed=1)
kml_results[['2']] = longclus_kml(tsdata, numclus=2, numruns=25, seed=1)
kml_results[['3']] = longclus_kml(tsdata, numclus=3, numruns=25, seed=1)
kml_results[['4']] = longclus_kml(tsdata, numclus=4, numruns=25, seed=1)
kml_results[['5']] = longclus_kml(tsdata, numclus=5, numruns=25, seed=1)
kml_results[['6']] = longclus_kml(tsdata, numclus=6, numruns=25, seed=1)
kml_results[['7']] = longclus_kml(tsdata, numclus=7, numruns=25, seed=1)
kml_results[['8']] = longclus_kml(tsdata, numclus=8, numruns=25, seed=1)
kml_results = kml_results[order(as.integer(names(kml_results)))]

saveRDS(kml_results, file.path(RESULTS_DIR, 'kml.rds'))
kml_results = readRDS(file.path(RESULTS_DIR, 'kml.rds'))

# Computation time
elbow_plot(names(kml_results) %>% as.integer, sapply(kml_results, '[[', 'time'), ystr='Computation time (s)') + expand_limits(y=0)

 # BIC
elbow_plot(names(kml_results) %>% as.integer, sapply(kml_results, '[[', 'BIC')) %T>% print
# ggsave(filename=file.path(FIG_DIR, 'kml_bic.pdf'), width=FIG_BIC_W, height=FIG_BIC_H, units='cm')
sapply(kml_results, '[[', 'BIC') %>% diff

# Silhouette
elbow_plot(names(kml_results) %>% as.integer, sapply(kml_results, '[[', 'silhouette'))

# Internal error
elbow_plot(names(kml_results) %>% as.integer, sapply(kml_results, '[[', 'MSE'), ystr='MSE (h^2)') + expand_limits(y=0)
elbow_plot(names(kml_results) %>% as.integer, sapply(kml_results, '[[', 'MSE') %>% sqrt, ystr='RMSE (h)') + expand_limits(y=0)
elbow_plot(names(kml_results) %>% as.integer, sapply(kml_results, '[[', 'MAE'), ystr='MAE (h)') + expand_limits(y=0)

# Final result ####
kml_result = kml_results[['6']]
# Analyze cluster solution
plot_usage_trends(kml_result, tsdata) %T>% print
# ggsave(filename=file.path(FIG_DIR, 'kml_trends.pdf'), width=FIG_TREND_W, height=FIG_TREND_H, units='cm')

# rel entropy
relativeEntropy(kml_result$model@postProba)

# Generate model table
kmlTable = kml_result$trends %>% 
    copy() %>%
        .[, Value := sigfig(Value, 2)] %>%
    .[, Time := factor(Time, unique(Time), 1:25)] %>%
    dcast(Cluster ~ Time) %>%
    .[, Prop := percent(kml_result$model@percentEachCluster, 1)] %>%
    setnames('Cluster', 'Group') %>%
    setcolorder(c('Group', 'Prop')) %T>% 
    print()

# write.csv(t(kmlTable), file.path(TAB_DIR, 'kml.csv'), quote = FALSE, row.names = TRUE)
