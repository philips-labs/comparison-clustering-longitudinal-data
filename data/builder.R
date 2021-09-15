library(data.table)
library(polynom)

# Create dataset comprising repeated measures data (regularly-spaced time series)
tsdata_create_repmeas = function(group='ts', numtraj, numobs, t.start=0, t.end=numobs-1) {
    stopifnot(numobs > 1)
    ts_names = factor(paste0(group, '.', 1:numtraj), levels=paste0(group, '.', 1:numtraj))

    dt_meas = data.table(
        Group=factor(group),
        Id=ts_names %>% rep(each=numobs),
        Time=seq(t.start*1.0, t.end*1.0, length.out=numobs) %>% rep(numtraj),
        Value=0,
        key=c('Group', 'Id', 'Time'))

    #group trend
    dt_trend = data.table(Group=factor(group),
                          Time=seq(t.start*1.0, t.end*1.0, length.out=numobs),
                          Value=0.0)
    setattr(dt_meas, 'trend', dt_trend)
    return(dt_meas)
}

#### Builder functions ####
# @param distr Distribution. E.g. norm, lnorm
# @param ... arguments passed to r{distr}()
tsdata_add_poly = function(tsdata, coefs, distr='norm', ...) {
    rfun = get(paste0('r', distr)); stopifnot(is.function(rfun))
    qfun = get(paste0('q', distr)); stopifnot(is.function(qfun))

    ts_coefs = tsdata[, .(
            Order=seq_along(coefs),
            Coef=coefs + rfun(length(coefs), ...) - qfun(.5, ...)),
        by=Id] %>%
        setkey(Id, Order)

    tsdata[, Value := Value + polynomial(ts_coefs[.BY]$Coef) %>% predict(Time), by=Id]

    # trend
    dt_trend = attr(tsdata, 'trend')
    dt_trend[, Value := Value + polynomial(coefs) %>% predict(Time)]
    setattr(tsdata, 'trend', dt_trend)

    return(tsdata)
}

tsdata_add_noise = function(tsdata, sd=1) {
    stopifnot(sd >= 0)
    tsdata[, Value := Value + rnorm(.N, mean=0, sd=sd)]
}

tsdata_add_propnoise = function(tsdata, prop=1, sd.min=.01) {
    stopifnot(prop >= 0)
    sdx = tsdata$Value * prop
    sdx[sdx < sd.min] = sd.min
    tsdata[, Value := Value + rnorm(.N, mean=0, sd=sdx)]
}

tsdata_add_lognoise = function(tsdata, meanlog, sdlog, center=FALSE) {
    stopifnot(sdlog >= 0)
    tsdata[, Value := Value + rlnorm(.N, meanlog=meanlog, sdlog=sdlog)]
    if(center) {
        tsdata[, Value := Value - qlnorm(.5, meanlog=meanlog, sdlog=sdlog)]
    }
}

tsdata_add_noise_arima = function(tsdata, ar=NULL, ma=NULL, sd=1, order=NULL) {
    tsdata[, Value := Value + arima.sim(n=.N, list(ar=ar, ma=ma, sd=sd)) %>% as.numeric, by=Id]
}


transformToRepeatedMeasures = function(data) {
    assert_that(is.data.frame(data), has_name(data, c('Id', 'Time', 'Value')))
    dtWide = dcast(data, Id ~ Time, value.var='Value')
    
    dataMat = as.matrix(dtWide[, -'Id'])
    assert_that(nrow(dataMat) == uniqueN(data$Id), ncol(dataMat) == uniqueN(data$Time))
    rownames(dataMat) = dtWide$Id
    colnames(dataMat) = names(dtWide)[-1]
    return(dataMat)
}