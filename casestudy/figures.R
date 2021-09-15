set.seed(1)
tsdata = longdata_cpap()

kml_results = readRDS(file.path(RESULTS_DIR, 'kml.rds'))
elbow_plot(names(kml_results) %>% as.integer, sapply(kml_results, '[[', 'BIC')) %T>% print
ggsave(filename=file.path(FIG_DIR, 'kml_bic.pdf'), width=FIG_BIC_W, height=FIG_BIC_H, units='cm')
plot_usage_trends(kml_results[['6']], tsdata) %T>% print
ggsave(filename=file.path(FIG_DIR, 'kml_trends.pdf'), width=FIG_TREND_W, height=FIG_TREND_H, units='cm')

gcmkm_results = readRDS(file.path(RESULTS_DIR, 'gcm3km.rds'))
elbow_plot(names(gcmkm_results) %>% as.integer, sapply(gcmkm_results, '[[', 'BIC')) %T>% print
ggsave(filename=file.path(FIG_DIR, 'gckm_bic.pdf'), width=FIG_BIC_W, height=FIG_BIC_H, units='cm')
plot_usage_trends(gcmkm_results[['8']], tsdata) %T>% print
ggsave(filename=file.path(FIG_DIR, 'gckm_trends.pdf'), width=FIG_TREND_W, height=FIG_TREND_H, units='cm')

gbtm_results = readRDS(file.path(RESULTS_DIR, 'gbtm3.rds'))
elbow_plot(names(gbtm_results) %>% as.integer, sapply(gbtm_results, '[[', 'BIC')) %T>% print
ggsave(filename=file.path(FIG_DIR, 'gbtm_bic.pdf'), width=FIG_BIC_W, height=FIG_BIC_H, units='cm')
plot_usage_trends(gbtm_results[['6']], tsdata) %T>% print
ggsave(filename=file.path(FIG_DIR, 'gbtm_trends.pdf'), width=FIG_TREND_W, height=FIG_TREND_H, units='cm')

gmm_results = readRDS(file.path(RESULTS_DIR, 'gmm3.rds'))
elbow_plot(names(gmm_results) %>% as.integer, sapply(gmm_results, '[[', 'BIC')) %T>% print
ggsave(filename=file.path(FIG_DIR, 'gmm_bic.pdf'), width=FIG_BIC_W, height=FIG_BIC_H, units='cm')
plot_usage_trends(gmm_results[['7']], tsdata) %T>% print
ggsave(filename=file.path(FIG_DIR, 'gmm_trends.pdf'), width=FIG_TREND_W, height=FIG_TREND_H, units='cm')

tvem_results = readRDS(file.path(RESULTS_DIR, 'mixtvem.rds'))
elbow_plot(names(tvem_results) %>% as.integer, sapply(tvem_results, '[[', 'BIC')) %T>% print
ggsave(filename=file.path(FIG_DIR, 'mixtvem_bic.pdf'), width=FIG_BIC_W, height=FIG_BIC_H, units='cm')
plot_usage_trends(tvem_results[['8']], tsdata) %T>% print
ggsave(filename=file.path(FIG_DIR, 'mixtvem_trends.pdf'), width=FIG_TREND_W, height=FIG_TREND_H, units='cm')
