set.seed(1)
tsdata = longdata_cpap()

gmm_results = list()
for(k in 8:1) {
    message('- Evaluating for k=', k, ' -')
    gmm_results[[as.character(k)]] = longclus_gmm(tsdata, numclus=k, numruns=50, maxiter=5, value='Value', seed=1,
                                                   model.fixed=Value~poly(Time, 3),
                                                   model.mixture=~poly(Time, 3),
                                                   model.random=~1,
                                                   model.diagcov=TRUE,
                                                   model.classcov=FALSE,
                                                   parallel=FALSE)
}
saveRDS(gmm_results, file.path(RESULTS_DIR, 'gmm3.rds'))
gmm_results = readRDS(file.path(RESULTS_DIR, 'gmm3.rds'))

# Computation time
elbow_plot(names(gmm_results) %>% as.integer, sapply(gmm_results, '[[', 'time') / 3600, ystr='Computation time (h)')

# BIC
elbow_plot(names(gmm_results) %>% as.integer, sapply(gmm_results, '[[', 'BIC')) %T>% print
# ggsave(filename=file.path(FIG_DIR, 'gmm_bic.pdf'), width=FIG_BIC_W, height=FIG_BIC_H, units='cm')
sapply(gmm_results, '[[', 'BIC')

# Silhouette
elbow_plot(names(gmm_results) %>% as.integer, sapply(gmm_results, '[[', 'silhouette'), ystr='Silhouette') %T>% print

# Internal error
elbow_plot(names(gmm_results) %>% as.integer, sapply(gmm_results, '[[', 'WMSE'), ystr='Weighted MSE (h^2)') + expand_limits(y=0)
elbow_plot(names(gmm_results) %>% as.integer, sapply(gmm_results, '[[', 'WMSE') %>% sqrt, ystr='Weighted RMSE (h)') + expand_limits(y=0)
elbow_plot(names(gmm_results) %>% as.integer, sapply(gmm_results, '[[', 'WMAE'), ystr='Weighted MAE (h)') + expand_limits(y=0)
elbow_plot(names(gmm_results) %>% as.integer, sapply(gmm_results, '[[', 'MAE'), ystr='MAE (h)') + expand_limits(y=0)

# Analyze cluster solution
gmm_model = gmm_result(gmm_results[['7']]$model)

plot_usage_trends(gmm_model, tsdata) %T>% print
# ggsave(filename=file.path(FIG_DIR, 'gmm_trends.pdf'), width=FIG_TREND_W, height=FIG_TREND_H, units='cm')

# Relative entropy


# Export table
gmmCoefs = summary(gmm_model$model)[, 'coef']
gmmSes = summary(gmm_model$model)[, 'Se']
gmmProps = colMeans(gmm_model$model$pprob[c(-1, -2)])

gmmCoefTable = matrix(gmmCoefs, nrow=gmm_model$model$ng) %>% 
    set_rownames(levels(gmm_model$clusters)) %>%
    set_colnames(names(gmmCoefs)[seq(1, length(gmmCoefs), by = gmm_model$model$ng)]) %>% 
    t() %T>% 
    print()
gmmSeTable = matrix(gmmSes, nrow=gmm_model$model$ng) %>% 
    set_rownames(levels(gmm_model$clusters)) %>%
    set_colnames(names(gmmSes)[seq(1, length(gmmSes), by = gmm_model$model$ng)]) %>% 
    t() %T>% 
    print()

# compute rbinded matrix indices to interweave coefs and se rows
gmmTIdx = c(1, 
    matrix(
        c( 2:(1+nrow(gmmCoefTable)), 
            (2+nrow(gmmCoefTable)):(1+2*nrow(gmmCoefTable))), 
        byrow=TRUE, nrow=2) %>% as.numeric()
)

gmmTable = rbind(props=percent(gmmProps, 1), 
    sigfig(gmmCoefTable),
    sub('(.*)', '(\\1)', sigfig(gmmSeTable))) %>%
    set_colnames(colnames(gmmCoefTable)) %>%
    .[gmmTIdx, ] %T>% # interweave coef and se
    print()

# write.csv(gmmTable, file.path(TAB_DIR, 'gmm.csv'), quote = TRUE, row.names = TRUE)