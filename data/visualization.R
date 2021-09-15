# @param trends whether to plot associated trends alongside
plot_trajectories = function(tsdata,
                             grouped=GROUP %in% names(tsdata),
                             props=TRUE,
                             trends=grouped,
                             traj.value=VALUE,
                             trend.time=TIME,
                             trend.value=VALUE) {
    xdata = tsdata_standardize(tsdata, value=traj.value)

    if(grouped) {
        group_levels = levels(xdata$Group)
        group_labels = sprintf('%s (%d%%)', levels(xdata$Group), round(prop.table(table(xdata$Group)) * 100))
        xdata[, Group := factor(Group, levels=group_levels, labels=group_labels)]

        p = ggplot() +
            geom_line(data=xdata, aes(x=Time, y=Value, group=Id), size=.1)

        if(trends && !is.null(attr(tsdata, 'trend'))) {
            dt_trends = tsdata_trends(tsdata) %>% copy
            dt_trends[, Group := factor(Group, levels=group_levels, labels=group_labels)]
            p = p + geom_line(data=dt_trends, aes_string(x=trend.time, y=trend.value), color='red', size=1)
        }
    }
    else if(GROUP %in% names(xdata)) {
        p = ggplot(xdata, aes(x=Time, y=Value, color=Group, group=Id)) + geom_line()
    }
    else {
        p = ggplot(xdata, aes(x=Time, y=Value, group=Id)) + geom_line()
    }

    if(grouped) {
        p = p + facet_wrap(~Group)
    }

    return(p)
}

plot_trends = function(trends, tsdata=NULL, clusters=NULL,
                       facet=!is.null(tsdata),
                       facet.nrow=NULL,
                       ribbon=FALSE,
                       ribbon.fill='steelblue1',
                       ribbon.alpha=1,
                       ribbon.q=c(.05, .95),
                       trend.value=VALUE,
                       trend.size=1.5,
                       traj.time=TIME,
                       traj.value=VALUE,
                       traj.color=TRUE,
                       traj.alpha=1,
                       traj.size=.1,
                       clusfmt='%s (%d%%)') {
    p = ggplot() +
        labs(x=traj.time, y=traj.value)

    if(!is.null(attr(trends, 'trend'))) {
        #tsdata object
        xtrends = tsdata_trends(trends) %>% tsdata_standardize(value=trend.value)
    }
    else {
        xtrends = tsdata_standardize(trends, value=trend.value)
    }
    xtrends = trends_adopt_time(xtrends, tsdata, newtime=traj.time)

    stopifnot(any(c(CLUSTER, GROUP, TIME) %in% names(xtrends)))
    if(!is.null(xtrends$Group)) {
        setnames(xtrends, GROUP, CLUSTER)
    }

    if(!is.null(clusters)) {
        stopifnot(is.factor(clusters))
        stopifnot(length(clusters) == uniqueN(tsdata$Id))
        clusNames = sapply(levels(clusters), function(clusname) sprintf(clusfmt, clusname, round(sum(clusters == clusname) / length(clusters) * 100)))
        clusters = factor(clusters, levels=levels(clusters), labels=clusNames)
        xtrends[, Cluster := factor(Cluster, levels=levels(Cluster), labels=clusNames)]
    } else {
        clusNames = levels(xtrends$Cluster)
    }

    if(!is.null(tsdata)) {
        stopifnot(!is.null(clusters)) #need clusters info
        xdata = tsdata_standardize(tsdata, time=traj.time, value=traj.value) %>%
            .[, Cluster := clusters[.GRP], by=Id]

        stopifnot(!anyNA(xdata$Cluster))

        if(ribbon) {
            dt_ribbon = xdata[, as.list(quantile(Value, ribbon.q)), by=.(Cluster, Time)] %>% setnames(c('Cluster', 'Time', 'ymin', 'ymax'))
            p = p + geom_ribbon(data=dt_ribbon, aes(x=Time, ymin=ymin, ymax=ymax), fill=ribbon.fill, alpha=ribbon.alpha)
        }
        else {
            if(traj.color == TRUE) {
                p = p + geom_line(data=xdata, aes(x=Time, y=Value, group=Id, color=Cluster), size=traj.size, alpha=traj.alpha)
            } else {
                p = p + geom_line(data=xdata, aes(x=Time, y=Value, group=Id), size=traj.size, color=traj.color, alpha=traj.alpha)
            }
        }
    }

    if(facet) {
        p = p + geom_line(data=xtrends, aes(x=Time, y=Value), color='black', size=trend.size) +
            facet_wrap(~Cluster, nrow=facet.nrow)
    } else {
        p = p + geom_line(data=xtrends, aes(x=Time, y=Value, color=Cluster), size=trend.size)
    }
    return(p)
}


plotGroupTrajectories = function(data, props) {
    if(has_attr(data, 'groupTrajs')) {
        groupTrajs = attr(data, 'groupTrajs') %>% copy()
        if(missing(props)) {
            props = data[, .(N=uniqueN(Id)), keyby=Group]$N / uniqueN(data$Id)
        }
    } else {
        groupTrajs = copy(data)
    }
    assert_that(uniqueN(data$Group) == nlevels(groupTrajs$Group))
    
    if(!missing(props)) {
        groupTrajs[, GroupLabel := factor(Group, levels=levels(Group), labels=sprintf('%s (%s%%)', gsub(' ', '\n', levels(Group)), round(props * 100)))]
    } else {
        groupTrajs[, GroupLabel := Group]
    }
    
    # Point positions
    times = sort(unique(groupTrajs$Time))
    nPoints = min(length(times), 10)
    pointIdx = seq(1, length(times), length.out=nPoints) %>% round
    pointData = groupTrajs[Time %in% times[pointIdx]]
    # Compute label positions
    library(lpSolve)
    groupTrajs[, Id := Group]
    groupMat = transformToRepeatedMeasures(groupTrajs)
    k = nlevels(groupTrajs$Group)
    labelIdx = seq(1, length(times), length.out=k+2)[2:(k+1)] %>% round
    m = apply(groupMat[, labelIdx], 2, function(x) vapply(1:k, function(i) min(abs(x[i] - x[-i])), FUN.VALUE=0))
    lp = lp.assign(m, direction='max')
    groupOrder = apply(lp$solution, 2, which.max)
    
    textData = groupTrajs[data.frame(Group=levels(groupTrajs$Group)[groupOrder],
        Time=times[labelIdx]), on=c('Group', 'Time')]
    
    # Plot
    ggplot(groupTrajs, aes(x=Time, y=Value, group=Group, color=GroupLabel)) +
        geom_line(size=.5, aes(linetype = GroupLabel)) +
        # geom_point(data=pointData, aes(x=Time, y=Value), size=2) +
        geom_label(data=textData, aes(x=Time, y=Value, label=Group),
            color='gray20',
            alpha=.9,
            label.padding = unit(.5, 'mm'),
            label.r = unit(1, 'mm'),
            size=ifelse(nchar(levels(groupTrajs$Group)) == 1, 2.5, 1.5)) +
        scale_shape_manual(name='Group', values=1:k) +
        scale_linetype_discrete(name='Group')
}