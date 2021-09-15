cases_test = expand.grid(
    data='longdata_randquadG',
    numgroups=c(2),
    model = c('blrt_longmodel_gbtm2'),
    numtraj=c(200), 
    numobs=c(10), 
    numruns=c(20), 
    minclus=2,
    maxclus=5,
    samples=500,
    noise=.1, 
    dataseed=1, 
    seed=1,
    re=c(RE_NORM_LOW)) %>% 
    as.data.table() %T>% 
    print()

{
    job_clear(queue = 'test')
    experiment_delete('test')
    experiment_submit(name='test', queue = 'test', cases=cases_test)
}

# Worker
{
    jobdata = job_getNext(queue='test')
    stopifnot(!is.null(jobdata))
    expname = jobdata$experiment
    expkey = experiment_key(expname)
    casename = jobdata$casename
    case = jobdata$case
    datafun = get(as.character(case$data))
    modelcall = get(as.character(case$model))
    message(sprintf('Evaluating job %s::%s...', expname, casename))

    # carry out job
    tsdata = do.call(datafun, case)
    case_env = c(case, tsdata=list(tsdata))
    out = eval(modelcall, envir=case_env)
    
    result = list(case=case, output=out)
    # redisHSet(expkey, casename, result)
}

# Result