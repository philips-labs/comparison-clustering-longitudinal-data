# construct table
results_blrt = experiment_getResults('normal_unknown_blrt')
dt_blrt = lapply(results_blrt, '[[', 'case') %>%
    lapply(as.data.table) %>%
    rbindlist(fill=TRUE)

outlist = lapply(results_blrt, '[[', 'output')
dt_blrt[, numclus := sapply(outlist, function(m) uniqueN(m$clusters))]
dt_blrt[, time := sapply(outlist, '[[', 'blrt_time')]
dt_blrt[, pvalue := mapply(function(m, k, kmin) m$blrt_pValues[k - kmin + 1], outlist, numclus, minclus)]
dt_blrt[, pvalue0 := mapply(function(m, k, kmin) m$blrt_pValues[k - kmin], outlist, numclus, minclus)]


# All
melt(dt_blrt, id=c('numgroups', 'dataseed', 'model'), measure=c('numclus')) %>%
    .[, .(MinusMany=mean(value < numgroups - 1) %>% round(4),
        MinusOne=mean(value == numgroups - 1) %>% round(4),
        Correct=mean(value == numgroups) %>% round(4),
        PlusOne=mean(value == numgroups + 1) %>% round(4),
        PlusMany=mean(value > numgroups + 1) %>% round(4)), keyby=.(model, variable)] %>%
    dcast(model ~ variable, value.var=c('MinusMany', 'MinusOne', 'Correct', 'PlusOne', 'PlusMany')) %>% t

# Per RE
melt(dt_blrt, id=c('numgroups', 'dataseed', 'model', 're'), measure=c('numclus')) %>%
    .[, .(MinusMany=mean(value < numgroups - 1) %>% round(4),
        MinusOne=mean(value == numgroups - 1) %>% round(4),
        Correct=mean(value == numgroups) %>% round(4),
        PlusOne=mean(value == numgroups + 1) %>% round(4),
        PlusMany=mean(value > numgroups + 1) %>% round(4)), keyby=.(model, re, variable)] %>%
    dcast(model + re ~ variable, value.var=c('MinusMany', 'MinusOne', 'Correct', 'PlusOne', 'PlusMany')) %>% t
