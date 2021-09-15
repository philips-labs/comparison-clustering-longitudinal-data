isFALSE = function(x) {
    identical(x, FALSE)
}

printBIC = function(model) {
    print(BIC(model))
    return(model)
}

which.bic = function(x, threshold=10) {
    x[is.na(x)] = Inf
    dt_bics = data.table(i=seq_along(x), BIC=x, key='BIC')
    idx = dt_bics[which(diff(BIC) >= threshold)[1], i]
    ifelse(is.na(idx), 1, idx)
}

min.bic = function(x, threshold=10) {
    x[which.bic(x, threshold=threshold)]
}

# R-squared of a piecewise-linear fit for the given threshold
pwlR2 = function(x, y, threshold) {
    left = function(x) ifelse(x < threshold, threshold - x, 0)
    right = function(x) ifelse(x >= threshold, x - threshold, 0)
    mod = lm(y ~ left(x) + right(x))
    return(summary(mod)$r.squared)
}

pwlR2v = Vectorize(pwlR2, 'threshold')

elbow = function(x) {
    i = seq_along(x)
    pwlR2v(i, x, i)
}

which.elbow = function(x) {
    i = seq_along(x)
    which.max(pwlR2v(i, x, i))
}

replaceValue = function(x, value, newValue) {
    x[x == value] = newValue
    return(x)
}

sigfig = function(x, digits = 2) {
    library(weights)
    out = formatC(signif(x, digits = digits), digits = digits, format = 'fg', flag = '#')
    # remove trailing zero
    out = sub('^(-)?0[.]', '\\1.', out)
    # removing ending zero
    out = sub('\\.$', '\\1', out)
    return(out)
}

# Plots ####
elbow_plot = function(x, y, xstr='Number of clusters', ystr='BIC') {
    data.table(x=x, y=y) %>% ggplot(aes(x=x, y=y)) +
        geom_line(size=.5) +
        geom_point(color='white', size=2) +
        geom_point(size=1) + 
        scale_x_continuous(breaks=x) +
        scale_y_continuous(label=comma) +
        theme(panel.grid.minor.x = element_blank()) +
        labs(x=xstr, y=ystr)
}

plot_usage_trends = function(result, tsdata) {
    trendData = copy(result$trends)
    trendData[, Time := Time * 6]
    setnames(trendData, 'Cluster', 'Group')
    props = table(result$clusters) %>% prop.table()
    
    p = plotGroupTrajectories(trendData, props) +
        scale_x_continuous(breaks=seq(0, 6, by=2)) +
        scale_y_continuous(breaks=seq(0, 10, by=2)) +
        scale_color_manual(name='Group', values = rep(c('grey70', 'grey40', 'black'), 10)) +
        coord_cartesian(ylim=c(0, 9.35)) +
        labs(x = 'Month of therapy', y = 'Usage (h)')
    
    message('Trend averages:')
    result$trends[, .(TrendAvg=mean(Value) %>% round(3)), keyby=Cluster] %T>% print
    
    message('Within-cluster variability:')
    tsdata[data.table(Id=unique(Id), Cluster=result$clusters), on='Id'] %>%
        .[, .(ObsMean=mean(Value) %>% round(3),
            ObsVar=var(Value) %>% round(3)), keyby=Cluster] %T>% print
    
    return(p)
}
