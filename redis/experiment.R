experiment_key = function(name) {
    sprintf('experiment-%s', name)
}

experiment_names = function() {
    stopifnot(redis_isConnected())
    keys = unlist(redisKeys('experiment-*'))
    pat = 'experiment-(.+)'
    sub(pat, '\\1', keys[grepl(pat, keys)])
}

experiment_getResults = function(name) {
    stopifnot(redis_isConnected())
    key = experiment_key(name)
    if(redisExists(key)) {
        results = redisHGetAll(key)
        return(results)
    }
    else {
        stop(sprintf('No results for experiment "%s"', name))
    }
}

experiment_getDatasets = function(name, cases, columns=names(cases)) {
    key = experiment_key(name)
    casenames = getCaseNames(cases, columns)
    names(casenames) = casenames
    lapply(casenames, function(name) {
        if(!redisHExists(key, name)) {
            stop(sprintf('case results of "%s" not found', name))
        }
        case = redisHGet(key, name)$case
        datafun = get(as.character(case$data))
        do.call(datafun, case)
    })
}

experiment_isEvaluated = function(name, case_names) {
    key = experiment_key(name)
    if(redisExists(key)) {
        result_names = unlist(redisHFields(key))
        return(case_names %in% result_names)
    } else {
        return(rep(FALSE, length(case_names)))
    }
}

experiment_getOutputs = function(name) {
    stopifnot(redis_isConnected())
    results = experiment_getResults(name)
    lapply(results, '[[', 'output')
}

# @param outfun optional function that is called on the output of each field, for returning additional scalar columns
# e.g. outfun = function(x) c(Jaccard=x$Jaccard)
experiment_getOutputTableOf = function(name, outfun) {
    stopifnot(redis_isConnected())
    results = experiment_getResults(name)
    cases = lapply(results, '[[', 'case') %>% lapply(as.data.table)
    dt_cases = do.call(rbind, c(cases, fill=TRUE))
    if(!missing(outfun)) {
        outlist = lapply(results, '[[', 'output') %>% lapply(outfun) %>% lapply(function(x) as.data.table(as.list(x)))
        outframe = rbindlist(outlist, fill=TRUE)
        stopifnot(nrow(outframe) == nrow(dt_cases))
        dt_cases = cbind(dt_cases, outframe)
    }
    setorderv(dt_cases, names(dt_cases))
    setkey(dt_cases, model)
    return(dt_cases)
}

experiment_getOutputTable = function(name) {
    stopifnot(redis_isConnected())
    results = experiment_getResults(name)

    dt_cases = lapply(results, '[[', 'case') %>%
        lapply(as.data.table) %>%
        rbindlist(fill=TRUE)

    outlist = lapply(results, '[[', 'output')
    fields = c('start', 'time', 'converged', 'WRSS', 'trendWRSS', 'silhouette', 'refSilhouette', 'BIC', 'ARI', 'Mirkin', 'sj1', 'sj2', 'sjMin', 'sjTotal', 'VI', 'KL', 'Dunn', 'refDunn', 'emptyClusters', 'loneClusters')
    dt_main = lapply(outlist, '[', fields) %>%
        lapply(as.data.table) %>%
        rbindlist(fill=TRUE)

    dtout = cbind(dt_cases, dt_main)
    return(dtout)
}

#@param experiment name
#@param inorder whether to submit the cases in order. Scrambling the order results in more reliable time estimates
experiment_submit = function(name, cases, inorder=FALSE, queue='') {
    stopifnot(redis_isConnected())
    stopifnot(is.data.frame(cases))
    stopifnot(nrow(cases) > 0)
    stopifnot('model' %in% names(cases))
    stopifnot('data' %in% names(cases))
    stopifnot(is.factor(cases$model))
    stopifnot(is.factor(cases$data))
    stopifnot(all(sapply(levels(cases$model), function(m) is.call(get(m)))))
    stopifnot(all(sapply(levels(cases$data), function(m) is.function(get(m)))))

    key = experiment_key(name)
    message(sprintf('Received %d cases for experiment "%s" to evaluate. Identifying new cases...', nrow(cases), name))
    case_names = getCaseNames(cases)
    # check results
    mask.result = !experiment_isEvaluated(name, case_names)
    # check jobs
    mask.job = !job_isQueued(name, case_names, queue=queue)

    # select new cases to be submitted
    new_cases = cases[mask.result & mask.job,]
    new_case_names = getCaseNames(new_cases)
    job_names = paste(key, new_case_names, sep='-')

    # Submit jobs
    if(inorder) {
        message(sprintf('Submitting %d jobs... %s', nrow(new_cases), queue))
        case_indices = seq_len(nrow(new_cases))
    } else {
        message(sprintf('Submitting %d jobs in random order... %s', nrow(new_cases), queue))
        case_indices = seq_len(nrow(new_cases)) %>% sample()
    }

    for(i in case_indices) {
        case = new_cases[i,] %>% as.list
        jobdata = list(
            experiment = name,
            casename = new_case_names[i],
            case = case
        )
        redisLPush(jobqueue_key(queue), jobdata)
    }
    message('Done.')
}

experiment_rename = function(name, newname) {
    stopifnot(redis_isConnected())
    key = experiment_key(name)
    newkey = experiment_key(newname)

    stopifnot(redisExists(key))
    if(redisExists(newkey)) {
        stop('Cannot rename experiment. New name already in use. Try experiment_merge')
    } else {
        redisRename(key, newkey)
        message('Successfully renamed experiment.')
    }
}

# @description Merge another experiment into the given one.
experiment_merge = function(name, addname, pattern='*') {
    stopifnot(redis_isConnected())
    key = experiment_key(name)
    addkey = experiment_key(addname)
    case_names = experiment_caseNames(addname) %>% .[grep(pattern, .)]
    newcase_names = setdiff(case_names, experiment_caseNames(name))
    message(sprintf('Adding %d new cases to experiment %s.', length(newcase_names), name))
    lapply(newcase_names, function(newcase) redisHSet(key, newcase, redisHGet(addkey, newcase)))
}

experiment_delete = function(name) {
    key = experiment_key(name)
    if(redisExists(key)) {
        redisDelete(key)
        message(sprintf('Deleted results of experiment "%s".', name))
    }
    else {
        message(sprintf('Nothing to delete for experiment "%s".', name))
    }
}


# @description Generate case names for the given data.frame of cases
getCaseNames = function(cases, columns=names(cases)) {
    stopifnot(is.data.frame(cases))
    if(nrow(cases) == 0) {
        return(c())
    }

    cases = data.table(cases)[, columns, with=FALSE]
    setcolorder(cases, order(names(cases))) #sort by column name
    casemat = sapply(cases, as.character) %>% t
    namemat = mapply(paste, rep(names(cases), nrow(cases)), casemat, MoreArgs=list(sep='='), USE.NAMES=FALSE) %>% matrix(nrow=length(cases), ncol=nrow(cases))
    apply(namemat, 2, paste, collapse=';')
}

addCaseParameters = function(dt_cases, dt_cases2) {
    stopifnot(!any(names(dt_cases2) %in% names(dt_cases)))

    data.table(dt_cases[rep(1:.N, nrow(dt_cases2))], dt_cases2[rep(1:.N, each=nrow(dt_cases))])
}

# @description Names of the cases stored under the given experiment
experiment_caseNames = function(name) {
    key = experiment_key(name)
    stopifnot(redisExists(key))
    redisHFields(key) %>% unlist
}

# @description Count the number of cases that match the given pattern. Useful to check before running a deletion
experiment_numcases = function(name, pattern='*') {
    case_names = experiment_caseNames(name)
    length(case_names[grep(pattern, case_names)])
}

# @description Delete cases from the experiment that match the given name pattern
# @param name experiment
# @param case pattern
experiment_deleteCases = function(name, pattern, sim=TRUE) {
    key = experiment_key(name)
    if(redisExists(key)) {
        case_names = redisHFields(key) %>% unlist
        del_cases = case_names[grep(pattern, case_names)]
        if(length(del_cases) == 0) {
            message(sprintf('No cases for experiment "%s" match pattern "%s", nothing to delete.', name, pattern))
        }
        else {
            if (sim) {
                message(sprintf('This would delete %d cases from experiment "%s", with pattern "%s". Run again with sim=FALSE to actually delete the cases.', length(del_cases), name, pattern))
            } else {
                foreach(field=del_cases) %do% {redisHDel(key, field)}
                message(sprintf('Deleted %d cases from experiment "%s", with pattern "%s"', length(del_cases), name, pattern))
            }
        }
    }
    else {
        message(sprintf('No results for experiment "%s", nothing to delete.', name))
    }
}

# Evaluate the result with access to the case-related dataset
# Call arguments: resultfun(tsdata, output)
experiment_computeResultWithData = function(name, resultfun, verbose=1,
                                            dataArgs=c('data', 'dataseed', 'numgroups', 're', 'numobs', 'numtraj', 'noise', 'propnoise', 'int.sd', 'slo.sd', 'q.sd', 'props')) {
    library(memoise)
    stopifnot(redis_isConnected())
    key = experiment_key(name)
    if(!redisExists(key)) {
        stop(sprintf('No results for experiment "%s"', name))
    }

    results = redisHGetAll(key)
    case_names = experiment_caseNames(name)

    create_dataset = function(...) {
        args = list(...)
        do.call(args$data %>% as.character, args)
    }
    datafun = memoise(create_dataset) # cached function

    out = vector('list', length(results))
    for (i in seq(4400, length(results))) {
        if(verbose && i %% verbose == 0) {
            messagef('[%d/%d] "%s"...', i, length(results), case_names[i])
        }
        store = results[[i]]
        tsdata = do.call(datafun, store$case[dataArgs])
        out[[i]] = resultfun(tsdata, store$output)
    }
    return(out)
}
