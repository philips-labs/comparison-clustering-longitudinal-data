set.seed(1)
tsdata = longdata_cpap()

gbtm_results = list()
for(k in 1:8) {
    message('- Evaluating for k=', k, ' -')
    gbtm_results[[as.character(k)]] = longclus_gbtm(tsdata, numclus=k, numruns=50, maxiter=5, value='Value', seed=1,
                                               model.fixed=Value ~ poly(Time, 3),
                                               model.mixture= ~ poly(Time, 3))
}
saveRDS(gbtm_results, file.path(RESULTS_DIR, 'gbtm3.rds'))
gbtm_results = readRDS(file.path(RESULTS_DIR, 'gbtm3.rds'))

# Computation time
elbow_plot(names(gbtm_results) %>% as.integer, sapply(gbtm_results, '[[', 'time'), ystr='Computation time (s)') + expand_limits(y=0)


# BIC
elbow_plot(names(gbtm_results) %>% as.integer, sapply(gbtm_results, '[[', 'BIC')) %T>% print
# ggsave(filename=file.path(FIG_DIR, 'gbtm_bic.pdf'), width=FIG_BIC_W, height=FIG_BIC_H, units='cm')
sapply(gbtm_results, '[[', 'BIC')

# Silhouette
elbow_plot(names(gbtm_results) %>% as.integer, sapply(gbtm_results, '[[', 'silhouette'), ystr='Silhouette') %T>% print

# Internal error
elbow_plot(names(gbtm_results) %>% as.integer, sapply(gbtm_results, '[[', 'WMSE'), ystr='Weighted MSE (h^2)') + expand_limits(y=0)
elbow_plot(names(gbtm_results) %>% as.integer, sapply(gbtm_results, '[[', 'WMSE') %>% sqrt, ystr='Weighted RMSE (h)') + expand_limits(y=0)
elbow_plot(names(gbtm_results) %>% as.integer, sapply(gbtm_results, '[[', 'WMAE'), ystr='Weighted MAE (h)') + expand_limits(y=0)

# Analyze cluster solution
gbtm_model = gmm_result(gbtm_results[['6']]$model)

plot_usage_trends(gbtm_model, tsdata) %T>% print
# ggsave(filename=file.path(FIG_DIR, 'gbtm_trends.pdf'), width=FIG_TREND_W, height=FIG_TREND_H, units='cm')

# gbtm_models = lapply(gbtm_models, function(gbtm_model) {
#     clusters = gbtm_model$clusters
#     trends = gbtm_model$trends
#     postProbs = as.matrix(gbtm_model$model$pprob[, -1:-2])
#     gbtm_model$MAE = trendEval_mae(tsdata, trends, clusters)
#     gbtm_model$MSE = trendEval_mse(tsdata, trends, clusters)
#     gbtm_model$WMAE = trendEval_wmae(tsdata, trends, postProbs)
#     gbtm_model$WMSE = trendEval_wmse(tsdata, trends, postProbs)
#     return(gbtm_model)
# })

gbtm_model$relativeEntropy

# Export table
gbtmCoefs = summary(gbtm_model$model)[, 'coef']
gbtmSes = summary(gbtm_model$model)[, 'Se']
gbtmProps = colMeans(gbtm_model$model$pprob[c(-1, -2)])

gbtmCoefTable = matrix(gbtmCoefs, nrow=gbtm_model$model$ng) %>% 
    set_rownames(levels(gbtm_model$clusters)) %>%
    set_colnames(names(gbtmCoefs)[seq(1, length(gbtmCoefs), by = gbtm_model$model$ng)]) %>% 
    t() %T>% 
    print()
gbtmSeTable = matrix(gbtmSes, nrow=gbtm_model$model$ng) %>% 
    set_rownames(levels(gbtm_model$clusters)) %>%
    set_colnames(names(gbtmSes)[seq(1, length(gbtmSes), by = gbtm_model$model$ng)]) %>% 
    t() %T>% 
    print()

# compute rbinded matrix indices to interweave coefs and se rows
gbtmTIdx = c(1, 
    matrix(
        c( 2:(1+nrow(gbtmCoefTable)), 
          (2+nrow(gbtmCoefTable)):(1+2*nrow(gbtmCoefTable))), 
        byrow=TRUE, nrow=2) %>% as.numeric()
    )

gbtmTable = rbind(props=percent(gbtmProps, 1), 
                        sigfig(gbtmCoefTable),
                        sub('(.*)', '(\\1)', sigfig(gbtmSeTable))) %>%
    set_colnames(colnames(gbtmCoefTable)) %>%
    .[gbtmTIdx, ] %T>% # interweave coef and se
    print()

# write.csv(gbtmTable, file.path(TAB_DIR, 'gbtm.csv'), quote = TRUE, row.names = TRUE)