jobqueue_key = function(queue) {
    paste0('jobs', queue)
}

jobqueue_errors_key = function(queue) {
    paste0('errors', queue)
}

jobqueue_success_key = function(queue) {
    paste0('success', queue)
}

job_key = function(name, case_names) {
    key = experiment_key(name)
    paste(key, case_names, sep='-')
}

job_getExperimentName = function(jobname) {
    pat = '^(.+?)-.+'
    sub(pat, '\\1', jobname[grepl(pat, jobname)])
}

job_getCaseName = function(jobname) {
    pat = '^.+?-(.+)'
    sub(pat, '\\1', jobname[grepl(pat, jobname)])
}

job_count = function(queue='') {
    redisLLen(jobqueue_key(queue))
}

# returns NULL when no more jobs are available
job_getNext = function(queue='') {
    redisRPop(jobqueue_key(queue))
}

job_isQueued = function(name, case_names, queue='') {
    jobqname = jobqueue_key(queue)
    if(redisExists(jobqname)) {
        ls_jobdata = redisLRange(jobqname, 0, redisLLen(jobqname)-1)
        expmask = lapply(ls_jobdata, '[[', 'experiment') == name
        job_names = sapply(ls_jobdata[expmask], '[[', 'casename')
        return(case_names %in% job_names)
    } else {
        return(rep(FALSE, length(case_names)))
    }
}

job_clear = function(queue='') {
    jobqname = jobqueue_key(queue)
    if(redisExists(jobqname)) {
        redisDelete(jobqname)
        message(sprintf('Job queue "%s" cleared.', queue))
    } else {
        message('Nothing to clear. No jobs scheduled.')
    }
}

job_clear_errors = function(queue='') {
    redisDelete(jobqueue_errors_key(queue=queue))
}

# note: subscribe/publish is buggy and breaks any further communication, even inbetween sessions
job_monitor = function(queue='') {
    errorskey = jobqueue_errors_key(queue=queue)
    successkey = jobqueue_success_key(queue=queue)
    cat(sprintf('%d open jobs\n', job_count(queue=queue)))
    cat('Hit <Esc> to stop monitoring.\n')
    redisSet(successkey, charToRaw('0'))
    total_count = job_count(queue=queue)
    if(redisExists(errorskey)) {
        start_err_count = redisLLen(errorskey)
    } else {
        start_err_count = 0
    }
    last_success_count = 0
    last_err_count = start_err_count
    timekeeping = NULL

    while(job_count(queue=queue) > 0 || as.integer(redisInfo()$connected_clients) > 1L) {
        new_success_count = redisGet(successkey) %>% as.integer
        new_err_count = redisLLen(errorskey)
        if(last_success_count != new_success_count) {
            if(is.null(timekeeping)) {
                timekeeping = list(start=Sys.time(), startcount=new_success_count)
                sec_per_case = 0
            } else {
                elapsed_time = as.numeric(Sys.time() - timekeeping$start, 'secs')
                sec_per_case = elapsed_time / (new_success_count - timekeeping$startcount)
            }
            printf(strftime(Sys.time() + 7200, '%b-%d %H:%M:%S |'))
            printf('%6s  %-10s %4d errors %6.1f s/case,  %4d min left [%d workers]\n',
                   paste0(round(new_success_count/total_count*100, 1), '%'),
                   paste0('(', new_success_count, '/', total_count, ')'),
                   new_err_count - start_err_count,
                   round(sec_per_case,1),
                   round((total_count - new_success_count) * sec_per_case / 60),
                   as.integer(redisInfo()$connected_clients)-1L)
            last_success_count = new_success_count
        }
        if(new_err_count != last_err_count) {
            errors = redisLRange(errorskey, last_err_count, new_err_count-1)
            print(errors)
            last_err_count = new_err_count
        }

        Sys.sleep(1)
    }
    cat('Ended monitoring because no other clients are connected.')
}

job_overview = function(queue='') {
    cat(sprintf('Retrieving overview of %d jobs...\n', job_count(queue=queue)))
    jobs = redisLRange(jobqueue_key(queue=queue), 0, job_count(queue=queue))
    expseq = sapply(jobs, '[[', 'experiment')
    exprle = rle(expseq)
    cat('Job queue:\n')
    print(data.table(Experiment=exprle$values, Jobs=exprle$lengths))
}

job_queues = function() {
    redisKeys('jobs*')
}

job = job_monitor
jobq = job_overview
