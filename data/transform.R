tsdata_standardize = function(tsdata, time=TIME, value=VALUE, id=ID, group=GROUP) {
    xdata = copy(tsdata)

    newnames = c(GROUP, ID, TIME, VALUE)
    oldnames = c(group, id, time, value)
    mask = oldnames %in% names(xdata) & newnames != oldnames

    # remove already existing columns
    delnames = intersect(newnames[mask], names(xdata))
    if(length(delnames) > 0) {
        xdata[, c(delnames) := NULL]
    }

    setnames(xdata, oldnames[mask], newnames[mask])

    # restore key because this tends to go wrong elsewhere... sometimes
    if('Id' %in% names(xdata)) {
        setkey(xdata, Id, Time)
    }

    return(xdata)
}

# @param datasets list of tsdata tables
tsdata_merge = function(datasets) {
    stopifnot(!is.data.frame(datasets))
    stopifnot(is.list(datasets))

    # construct group trends table
    dt_trends = lapply(datasets, attr, 'trend') %>% rbindlist %>% setkey(Group, Time)

    # create merged dataset
    dt_all = do.call(rbind, datasets) %>%
        setkey(Group, Id, Time) %>%
        setattr('trend', dt_trends)

    return(dt_all)
}

# returns a vector of group assignment per trajectory
tsdata_groups = function(tsdata) {
    if('Group' %in% names(tsdata)) {
        tsdata[, Group[1], by=Id]$V1
    } else {
        tsdata[, NA*0, by=Id]$V1
    }
}

# computes averaged trends over the trajectories, using the group column
tsdata_trends = function(tsdata) {
    attr(tsdata, 'trend')
}

# @return matrix with 1 trajectory per row
tsdata_matrix = function(tsdata) {
    stopifnot(uniqueN(tsdata[, .N, by=Id]$N) == 1) # expecting time series of equal length
    time_ref = tsdata[Id == tsdata[, Id[1]], Time]
    stopifnot(all(tsdata[, all.equal(Time, time_ref), by=Id]$V1))

    dt_wide = dcast(tsdata, Id ~ Time, value.var='Value')
    tsmat = as.matrix(dt_wide[,-'Id'])
    rownames(tsmat) = dt_wide$Id

    stopifnot(all(rownames(tsmat) == unique(tsdata$Id)))
    return(tsmat)
}

# sets the TIME column of trends to the column in tsdata specified by 'newtime'
trends_adopt_time = function(trends, tsdata, newtime) {
    stopifnot(is.data.table(trends))
    stopifnot(is.data.table(tsdata))
    stopifnot(TIME %in% names(trends))

    if(newtime == TIME) {
        return(copy(trends))
    }

    stopifnot(c(TIME, newtime) %in% names(tsdata))
    xtrends = trends[unique(tsdata[, c(TIME, newtime), with=FALSE]), on=TIME] %>%
        .[, c(TIME) := NULL] %>%
        setnames(newtime, TIME) %>%
        setkeyv(c(CLUSTER, TIME))
    return(xtrends)
}
