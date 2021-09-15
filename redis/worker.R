message('Hello.')
message(sprintf('Working directory: %s', getwd()))

if(.Platform$OS.type == 'unix') {
    workerId = as.integer(Sys.getenv('PBS_ARRAYID'))
    message(sprintf('-- Worker %d --', workerId))
} else {
    
}

queue = Sys.getenv('JOBQUEUE')

source('.Rprofile')
source('include.R')

if(!redis_isActive()) {
    stop('Redis host file missing. The Redis server is probably not running. Stop.')
}

redis_connect()
message(sprintf('Active on job queue "%s"', queue))


while(job_count(queue=queue) > 0) {
    jobdata = job_getNext(queue=queue)
    if(is.null(jobdata)) {
        warning('Got NULL job. Time for a break()')
        break()
    }

    expname = jobdata$experiment
    expkey = experiment_key(expname)
    casename = jobdata$casename
    case = jobdata$case
    datafun = get(as.character(case$data))
    modelcall = get(as.character(case$model))
    message(sprintf('Evaluating job %s::%s...', expname, casename))

    if(experiment_isEvaluated(expname, casename)) {
        warning(sprintf('Experiment "%s", case "%s": already evaluated! Skip.', expname, casename))
        redisPublish('jobs', charToRaw('error'))
        next()
    }

    err = tryCatch({
        # carry out job
        tsdata = do.call(datafun, case)
        case_env = c(case, tsdata=list(tsdata))
        out = eval(modelcall, envir=case_env)

        result = list(case=case, output=out)
        redisHSet(expkey, casename, result)
        NULL
    }, error=return)

    if(is.null(err)) {
        # finalize
        redisIncr(jobqueue_success_key(queue))
    } else {
        # error handling
        err$casename = casename
        err$case = case
        message(err)
        redisLPush(jobqueue_errors_key(queue), err)
    }
}

message('No jobs left.')
withTimeout(redisClose(), timeout=5)
message('Goodbye.')
