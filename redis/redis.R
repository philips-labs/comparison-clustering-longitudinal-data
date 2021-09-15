redis_isActive = function() {
    return(file.exists(REDIS_HOST_FILE))
}

redis_isConnected = function() {
    tryCatch({redisInfo(); TRUE}, error=function(e) FALSE)
}

redis_connect = function() {
    suppressPackageStartupMessages({
        library(rredis)
        library(R.utils)
    })

    source('redis/experiment.R')
    source('redis/job.R')

    if(!redis_isActive()) {
        stop('Cannot connect to server:  host-port file not found. Server is probably not running.')
    }
    host_info = read.table(REDIS_HOST_FILE, stringsAsFactors=FALSE)
    redisConnect(host=host_info$V1, port=as.integer(host_info$V2), password=REDIS_PWD)
    message(sprintf('Connected to Redis at %s:%s.', host_info$V1, host_info$V2))

    options('redis:num'=TRUE)
}

redis_close = function(stop=TRUE) {
    redisBgSave()
    redisClose()
    message('Redis connection closed.')
}

redis_shutdown = function() {
    message('Shutting down Redis server...')
    redisSetPipeline(TRUE)
    redisShutdown()
    redisSetContext(NULL)
    message('Redis connection closed.')
}
