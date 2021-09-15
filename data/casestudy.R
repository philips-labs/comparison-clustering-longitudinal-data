longdata_cpap = function(maxday=175, minday=175, binsize=7, tmax=1, maxuse=13, maxpatients=Inf) {
    numbins = maxday / binsize
    stopifnot(numbins %% 1 == 0) #check for evenly sized bins

    dt_all = readRDS(USAGE_DATA_FILE)
    stopifnot(max(dt_all$Day) >= maxday)

    dtpats = dt_all[, .N, by=Id][N >= minday, .(Id)]
    dt = dt_all[dtpats][Day <= maxday]

    # insert missing days (up to maxday)
    idx = dt[, .(Day=1:maxday), by=Id]
    dtimp = dt[idx, on=c('Id', 'Day')] %>%
        .[is.na(Value), Value := 0] %>%
        .[Value > maxuse, Value := maxuse]

    stopifnot(!anyNA(dtimp$Value))
    messagef('Usage data of %d individuals with %d-%d days of therapy summarized into %d bins', uniqueN(dt$Id), minday, maxday, numbins)

    # create bins
    tsdata = dtimp[, mean(Value), by=.(Id, cut(Day, maxday/binsize) %>% as.numeric)]
    stopifnot(all(tsdata[, .N, by=Id]$N == maxday/binsize))
    setnames(tsdata, 'V1', VALUE)
    tsdata[, c(TIME) := (cut - 1) / (numbins - 1) * tmax]
    tsdata[, Week := as.integer(cut)]
    tsdata[, Day := as.integer(cut) * 7]
    tsdata[, Month := Day / 30]
    tsdata[, cut := NULL]
    stopifnot(identical(range(tsdata[[TIME]]), c(0,tmax)))
    setcolorder(tsdata, c(ID, TIME, VALUE, 'Month', 'Week', 'Day'))
    setkeyv(tsdata, c(ID, TIME))

    if(uniqueN(tsdata$Id) > maxpatients) {
        tsdata = tsdata[.(Id = unique(Id) %>% sample(maxpatients))]
        messagef('  selected %d patients at random', maxpatients)
        setkeyv(tsdata, c(ID, TIME)) #needed because selection can mess up the interal keys
    }

    return(tsdata)
}
