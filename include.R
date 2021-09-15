Sys.setenv(TZ='UTC')

# in order to reproduce results from R 3.6.1
if (utils::compareVersion(paste(R.version$major, R.version$minor, sep = '.'), '3.6') > -1) {
    RNGkind(sample.kind = 'Rounding')
}

TIME = 'Time'
VALUE = 'Value'
GROUP = 'Group'
CLUSTER = 'Cluster'
ID = 'Id'

suppressPackageStartupMessages({
	require(nlme)
	require(lme4)

	require(clusterCrit)
	require(clusteval)
    library(assertthat)
	library(data.table)
	require(entropy)
	require(foreach)
	require(ggfortify)
	library(ggplot2)
    library(mvnfast)
	require(glmnet)
	require(kml)
	require(lcmm)
	require(lsmeans)
	library(magrittr)
    library(multcompView)
	require(matrixStats)
	require(mclust)
	require(psych)
	library(scales)
	require(zoo)
})

source('util.R')

source(file.path('redis', 'redis.R'))

source(file.path('data', 'builder.R'))
source(file.path('data', 'transform.R'))
source(file.path('data', 'datasets.R'))
source(file.path('data', 'datasets_randlinear_normal.R'))
source(file.path('data', 'datasets_randquad_normal.R'))
source(file.path('data', 'datasets_randquad_lognormal.R'))
source(file.path('data', 'datasets_randquad_propnoise.R'))
source(file.path('data', 'casestudy.R'))
source(file.path('data', 'visualization.R'))

source(file.path('methods', 'results.R'))
source(file.path('methods', 'gbtm.R'))
source(file.path('methods', 'gcm_kmeans.R'))
source(file.path('methods', 'gmm.R'))
source(file.path('methods', 'kml.R'))
source(file.path('methods', 'lm_kmeans.R'))
source(file.path('methods', 'mixtvem', 'MixTVEM.R'))
source(file.path('methods', 'mixtvem.R'))

source(file.path('models', 'kml_models.R'))
source(file.path('models', 'twostep_models.R'))
source(file.path('models', 'gbtm_models.R'))
source(file.path('models', 'gmm_models.R'))
source(file.path('models', 'mixtvem_models.R'))

source(file.path('metrics', 'similarity_label.R'))
source(file.path('metrics', 'similarity_trend.R'))
source(file.path('metrics', 'internal_cluster.R'))
source(file.path('metrics', 'internal_trend.R'))
source(file.path('metrics', 'internal_label.R'))

messagef = function(...) message(sprintf(...))


message('Initialized simulation scripts.')

RE_NORM_LOW = .1
RE_NORM_MED = .2
RE_NORM_HIGH = .3

RE_LNORM_LOW = log(.15)
RE_LNORM_MED = log(.3)
RE_LNORM_HIGH = log(.45)

FIG_BIC_W = 6
FIG_BIC_H = 4
FIG_TREND_W = 10
FIG_TREND_H = 4

theme_minimal(base_size = 9) %+replace%
    theme(
        plot.background = element_rect(colour = NA),
        plot.margin = unit(c(2,1,1,1), 'mm'),
        panel.background = element_rect(colour = NA),
        panel.spacing = unit(1, 'mm'),
        strip.background = element_rect(colour=NA, fill=NA),
        strip.text = element_text(face='plain', size=7, margin=margin()),
        legend.text = element_text(size=7),
        legend.title = element_text(size=9),
        legend.position = 'right',
        legend.spacing = unit(1, 'cm'),
        legend.margin = margin(0,0,0,0),
        legend.key.size = unit(9, 'pt'),
        legend.box.margin = margin(0,0,-5,0),
        axis.line.x = element_line(color='black', size = .1),
        axis.line.y = element_line(color='black', size = .1)
    ) %>%
    theme_set()

message('Ensure that the Redis server is running in order to access simulation results. Call redis_connect() to connect to the server.')
