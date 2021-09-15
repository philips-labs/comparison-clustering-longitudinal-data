set.seed(1)
tsdata = longdata_cpap()

kml_results = readRDS(file.path(RESULTS_DIR, 'kml.rds'))
gcmkm_results = readRDS(file.path(RESULTS_DIR, 'gcm3km.rds'))
gbtm_results = readRDS(file.path(RESULTS_DIR, 'gbtm3.rds'))
gmm_results = readRDS(file.path(RESULTS_DIR, 'gmm3.rds'))

labelSimilarity_nsj_min(kml_results$'6'$clusters, gbtm_results$'6'$clusters)
labelSimilarity_nsj_min(kml_results$'6'$clusters, gcmkm_results$'8'$clusters)
labelSimilarity_nsj_min(kml_results$'6'$clusters, gmm_results$'7'$clusters)

labelSimilarity_nsj_min(gcmkm_results$'8'$clusters, kml_results$'6'$clusters)
labelSimilarity_nsj_min(gcmkm_results$'8'$clusters, gmm_results$'7'$clusters)
trendSimilarity_wmmse(gcmkm_results$'7'$trends, gmm_results$'7'$trends, gmm_results$'7'$clusters)



trendSimilarity_wmmse(kml_results$'6'$trends, gbtm_results$'6'$trends, gbtm_results$'6'$clusters)
trendSimilarity_wmmse(gcmkm_results$'8'$trends, gcmkm_results$'7'$trends, gmm_results$'7'$clusters)
trendSimilarity_wmmse(kml_results$'6'$trends, gcmkm_results$'7'$trends, gmm_results$'7'$clusters)
