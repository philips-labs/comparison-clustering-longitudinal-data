# Mean absolute error (MAE) ####
# Averaged across individuals
trendEval_mae = function(tsdata, trends, clusters) {
    xdata = copy(tsdata)[, Cluster := clusters[.GRP], by=Id]
    dtid_mae = merge(xdata, trends, by=c('Cluster', 'Time'))[, .(MAE = mean(abs(Value.x - Value.y))), by=Id]
    mean(dtid_mae$MAE)
}

trendEval_mse = function(tsdata, trends, clusters) {
    xdata = copy(tsdata)[, Cluster := clusters[.GRP], by=Id]
    dtid_mse = merge(xdata, trends, by=c('Cluster', 'Time'))[, .(MSE = mean((Value.x - Value.y)^2)), by=Id]
    mean(dtid_mse$MSE)
}

trendEval_wmae = function(tsdata, trends, postProbs) {
    numtraj = uniqueN(tsdata$Id)
    numobs = uniqueN(tsdata$Time)

    dt_diff = merge(tsdata[, .(Id, Time, Value)], trends, by=c('Time'), all.x=TRUE, allow.cartesian=TRUE)
    idSaeMat = dt_diff[, .(SAE=sum(abs(Value.y - Value.x))), by=.(Cluster, Id)] %>%
        dcast(Id ~ Cluster, value.var='SAE') %>%
        .[,-'Id'] %>% as.matrix()
    sum(postProbs * idSaeMat) / numtraj / numobs
}

# Weighted residual sum of squares (WRSS) ####
# Computes the RSS per individual to each trend, weighted by the posterior probability
trendEval_wrss = function(tsdata, trends, postProbs) {
    dt_diff = merge(tsdata[, .(Id, Time, Value)], trends, by=c('Time'), all.x=TRUE, allow.cartesian=TRUE)
    idRssMat = dt_diff[, .(RSS=sum((Value.y - Value.x)^2)), by=.(Cluster, Id)] %>%
        dcast(Id ~ Cluster, value.var='RSS') %>%
        .[,-'Id'] %>% as.matrix()
    sum(postProbs * idRssMat)
}

# Weighted mean square error (WMSE) ####
trendEval_wmse = function(tsdata, trends, postProbs) {
    numtraj = uniqueN(tsdata$Id)
    numobs = uniqueN(tsdata$Time)
    trendEval_wrss(tsdata, trends, postProbs) / numtraj / numobs
}
