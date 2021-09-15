#
longclus_gmm = function(tsdata, numclus, numruns, maxiter,
                         model.fixed=Value~Time, #fixed effects
                         model.random=~1, #random effects
                         model.mixture=~Time, #class-specific effects
                         model.diagcov=TRUE, #diagonal covariance matrix
                         model.classcov=FALSE, #class-specific covariance matrix
                         seed=NULL,
                         store='all',
                         value=VALUE,
                         parallel=FALSE) {
    set.seed(seed)
    xdata = tsdata_standardize(tsdata, value=value)
    xdata[, Id := as.integer(Id)]
    start = Sys.time()

    message('\tFitting GCM... ')
    gcm = hlme(fixed=model.fixed, random=model.random, subject='Id', ng=1, idiag=model.diagcov, data=xdata, verbose=FALSE, returndata= TRUE)
    
    if (numclus == 1) {
        gmm = gcm
    } else {
        # Cannot figure this out. Calling longclus_gmm directly works fine, but when calling via longclus_gbtm sudenly things break. Pass all variables through list to avoid this
        expr = substitute(expr=hlme(fixed=model.fixed, random=model.random, mixture=model.mixture, subject='Id', ng=numclus, idiag=model.diagcov, nwg=model.classcov, data=xdata, returndata=TRUE),
                          env=list(model.fixed=model.fixed, model.random=model.random, model.mixture=model.mixture, numclus=numclus, model.diagcov=model.diagcov, model.classcov=model.classcov, xdata=xdata))
        gmm = do.call(lcmm_gridsearch, list(rep=numruns, maxiter=maxiter, minit=gcm, m=expr, parallel=parallel, verbose=FALSE))
    }

    gmm_result(gmm, start, store)
}

gmm_result = function(model, start = 0, store = 'all') {
    stopifnot(!is.null(model$data))
    xdata = model$data
    
    if(nrow(model$pred) != nrow(xdata)) {
        stop('assumption violated. Please check')
    }
    
    clusnames = LETTERS[1:model$ng]
    clusters = factor(model$pprob$class, levels=1:model$ng, labels=clusnames)
    
    postProbs = as.matrix(model$pprob[, -1:-2])
    colnames(postProbs) = clusnames
    
    # Compute trends
    stopifnot(all(model$pred$Id == as.numeric(xdata$Id)))
    dt_trajmarg = data.table(model$pred[grep('Id|pred_m\\d', names(model$pred))], Time=xdata$Time) %>%
        melt(id=c('Id', 'Time'), variable.name='Cluster', value.name='Value')
    dt_trends = unique(dt_trajmarg[, -'Id'], by=c('Time', 'Cluster')) %>%
        .[, Cluster := factor(as.integer(Cluster), levels=1:model$ng, labels=clusnames)] %>%
        setcolorder(c('Cluster', 'Time', 'Value'))
    
    # Results
    result = result_summary(xdata, clusters=clusters, dt_trends=dt_trends, start=start, postProbs=postProbs, numclus=model$ng)
    result$BIC = model$BIC
    result$AIC = model$AIC
    result$relativeEntropy = relativeEntropy(as.matrix(model$pprob[c(-1, -2)]))
    result$converged = model$conv == 1
    result[c('R2m', 'R2c')] = pseudoRsquared_hlme(model)
    result$model = switch(store, all=model,
        core={gmm_clean = model; gmm_clean[c('pred', 'call', 'dataset', 'pprob', 'predRE')] = NULL; gmm_clean},
        none=NULL)
    return(result)
}

longclus_gmm_blrt = function(tsdata, minclus, maxclus, numruns, maxiter,
    model.fixed=Value~Time, #fixed effects
    model.random=~1, #random effects
    model.mixture=~Time, #class-specific effects
    model.diagcov=TRUE, #diagonal covariance matrix
    model.classcov=FALSE, #class-specific covariance matrix
    seed=NULL,
    samples=500,
    store='all',
    value=VALUE,
    parallel=FALSE) 
{
    set.seed(seed)
    xdata = tsdata_standardize(tsdata, value=value)
    xdata[, Id := as.integer(Id)]
    allstart = Sys.time()
    
    message('\tFitting GCM... ')
    gcm = hlme(fixed=model.fixed, random=model.random, subject='Id', ng=1, idiag=model.diagcov, data=xdata, verbose=FALSE)
    expr = substitute(expr=hlme(fixed=model.fixed, random=model.random, mixture=model.mixture, subject='Id', ng=2, idiag=model.diagcov, nwg=model.classcov, data=xdata, returndata=TRUE),
        env=list(model.fixed=model.fixed, model.random=model.random, model.mixture=model.mixture, model.diagcov=model.diagcov, model.classcov=model.classcov, xdata=xdata))
    fit_model = function(k) {
        expr$ng = k
        do.call(lcmm_gridsearch, list(rep=numruns, maxiter=maxiter, minit=gcm, m=expr, parallel=FALSE, verbose=FALSE))        
    }
    
    k = minclus
    models = list()
    results = list()
    p = rep(NaN, maxclus - 1)
    b = rep(NaN, maxclus - 1)
    start = Sys.time()
    models[[k]] = fit_model(k)
    results[[k]] = gmm_result(models[[k]], start, store)
    
    while (k < maxclus) {
        message('\tComputing BLRT for k = (', k, ', ', k + 1, ')')
        start = Sys.time()
        models[[k + 1]] = fit_model(k + 1)
        results[[k + 1]] = gmm_result(models[[k + 1]], start, store)
        nullModel = models[[k]]
        altModel = models[[k + 1]]
        
        pk = blrt_gmm(nullModel, altModel, samples = samples)
        p[k] = pk
        b[k] = attr(pk, 'b')
        
        if (p[k] >= .05) {
            message('\t\tBLRT failed to reject null with k0=', k)
            break()
        }
        k = k + 1
    }
    
    # final model
    bestResult = results[[k]]
    
    blrt_idx = minclus:min(maxclus, k + 1)
    bestResult$blrt_logLiks = sapply(models[blrt_idx], '[[', 'loglik')
    bestResult$blrt_bics = sapply(models[blrt_idx], '[[', 'BIC')
    bestResult$blrt_pValues = p[minclus:min(maxclus - 1, k)]
    bestResult$blrt_time = as.numeric(Sys.time() - allstart, 'secs')
    bestResult$blrt_b = b[minclus:min(maxclus - 1, k)]
    
    names(bestResult$blrt_logLiks) = paste0('k=', blrt_idx)
    names(bestResult$blrt_bics) = paste0('k=', blrt_idx)
    names(bestResult$blrt_pValues) = paste0('k=(', minclus:min(maxclus - 1, k), '-', minclus:min(maxclus - 1, k) + 1, ')')
    names(bestResult$blrt_b) = names(bestResult$blrt_pValues)
    
    return(bestResult)
}

lcmm_gridsearch = function(m, rep, maxiter, minit, parallel=FALSE, verbose=FALSE) {
    start = Sys.time()

    message('\tGMM gridsearch... ', appendLF=FALSE)
    mc <- match.call()$m
    mc$maxiter <- maxiter
    mc$verbose <- verbose
    models <- vector(mode = "list", length = rep)
    assign("minit", eval(minit))
    models = foreach(k = 1:rep) %dopar% {
        e = environment()
        stopifnot(exists('minit', envir=e))
        mc$B <- substitute(random(minit), env=e)
        k_res = do.call(as.character(mc[[1]]), as.list(mc[-1]))
        message('|', appendLF=FALSE)
        return(k_res)
    }
    message(sprintf('done after %gs.', as.numeric(Sys.time() - start, 'secs') %>% round(1)))

    llmodels <- sapply(models, function(x) {
        return(x$loglik)
    })
    kmax <- which.max(llmodels)
    mc$B <- models[[kmax]]$best
    mc$maxiter <- NULL
    message('\tFinal model optimization... ', appendLF=FALSE)
    start = Sys.time()
    out = do.call(as.character(mc[[1]]), as.list(mc[-1]))
    message(sprintf('done after %gs.', as.numeric(Sys.time() - start, 'secs') %>% round(1)))
    return(out)
}

# Generate simulated data from the hlme model
sim_gmm = function(model) {
    clusProps = colMeans(model$pprob[,c(-1, -2)])
    sdRes = abs(coef(model)['stderr'])
    stopifnot(!is.null(model$data))
    stopifnot(hasName(model$data, TIME))
    times = sort(unique(model$data[[TIME]]))
    numobs = length(times)
    numtraj = model$ns
    numclus = model$ng

    dtGroup = predictY(model, newdata = data.frame(Time = times))$pred %>% 
        reshape2::melt(value.name = 'Mu') %>% 
        as.data.table() %>%
        .[, Time := times[Var1]] %>%
        .[, Class := as.integer(Var2)] %>%
        .[, c('Var1', 'Var2') := NULL] %>% 
        .[]
    
    newdata = data.table(
        Id = seq_len(numtraj) %>% rep(each = numobs), 
        Time = rep(times, numtraj),
        Class = sample.int(n = numclus, size = numtraj, replace = TRUE, prob = clusProps) %>% rep(each = numobs)) %>%
        merge(dtGroup, by = c('Class', 'Time')) %>%
        setkey(Id, Time)
    
    capture.output({ 
        vcmat <- VarCovRE(model)
    })
    
    if(is.matrix(vcmat)) {
        vcr = vcmat[, 1]
        vcr <- ifelse(vcr == 0, min(vcr[vcr > 0]) / 1e3, vcr)
        covMat = diag(vcr)
        Xre = model.matrix(model$call$random, data = newdata)
        tsCoefs = rmvn(n = numtraj, mu = rep(0, nrow(covMat)), sigma = covMat)
        rowCoefMat = tsCoefs[rep(seq_len(numtraj), each = numobs),]
        newdata[, TsMu := Mu + rowSums(rowCoefMat * Xre)]
        newdata[, Value := rnorm(.N, mean = TsMu, sd = sdRes)]
    } else {
        newdata[, Value := rnorm(.N, mean = Mu, sd = sdRes)]
    }
    
    return(newdata[])
}

blrt_gmm = function(nullModel, altModel, samples = 500) {
    k0 = nullModel$ng
    k1 = altModel$ng
    stopifnot(k1 == k0 + 1)
    
    refLrts = -2 * (nullModel$loglik - altModel$loglik)
    lrts = rep(NaN, samples)
    
    b = 1
    while (b <= samples) {
        cat('Boot sample', b)
        bootdata = sim_gmm(nullModel)
        
        mcNull = nullModel$call
        mcNull$data = bootdata
        mcNull$B = nullModel$best
        nullFit = do.call(hlme, as.list(mcNull[-1]))
        
        mcAlt = altModel$call
        mcAlt$data = bootdata
        mcAlt$B = altModel$best
        altFit = do.call(hlme, as.list(mcAlt[-1]))
        
        lrts[b] = -2 * (nullFit$loglik - altFit$loglik)
        
        lowTest = binom.test(1 + sum(lrts[1:b] >= refLrts), 1 + b, p = .05, alternative = 'less')
        highTest = binom.test(1 + sum(lrts[1:b] >= refLrts), 1 + b, p = .05, alternative = 'greater')
        
        cat(' conf.int on p-value: [', lowTest$conf.int, ']\n')
        if (is.na(lrts[b])) {
            cat('\tDid not converge. Retrying.\n')
            next()
        } else if (lowTest$p.value < .05) {
            cat('\tPrematurely stopping bootstrap sampling due to high confidence in successful rejection of null: p =', lowTest$p.value, '\n')
            break()
        } else if (highTest$p.value < .005) {
            cat('\tPrematurely stopping bootstrap sampling due to high confidence in failure to reject null: p =', highTest$p.value, '\n')
            break()
        } else {
            b = b + 1
        }
    }
    b = min(samples, b)
    
    p = (1 + sum(lrts[1:b] >= refLrts)) / (1 + b)
    attr(p, 'b') = b
    return(p)
}

# Adopted from MuMIn::r.squaredGLMM, to be used with hlme objects
pseudoRsquared_hlme = function(x) {
    VarFx = var(x$pred$pred_m)
    sigma2 = coef(x)['stderr']^2 #Residual variance
    varRe = coef(x)['cholesky 1']^2
    varTot = sum(VarFx, varRe, na.rm=TRUE) # total variance
    res = c(VarFx, varTot) / (varTot + sigma2)
    names(res) = c('R2m', 'R2c')
    res
}
