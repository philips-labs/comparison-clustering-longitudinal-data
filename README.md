# comparison-clustering-longitudinal-data
This repository contains all R code used in running and analyzing the simulation study and case study reported in the manuscript.

As the simulation study involves many simulation settings (over 27,000) and the estimation time of some methods was rather long, a custom parallel simulation framework was implemented for use on a computation cluster. While a computational cluster is not strictly needed if you are only interested in replicating a subset of the simulation scenarios or methods, you will need to configure a Redis database server (https://redis.io/) in order to run any simulations. The instructions are provided below.

The complete database of simulation results (600 MB) is available upon request.

# Getting started
1. Either load the Rstudio project file `comparison.Rproj`, or start an R session with the working directory set to the root repository directory.
2. Install required packages and dependencies
```
install.packages(c("assertthat", "data.table", "ggplot2", "mvnfast", "magrittr", "multcompView", "rredis", "scales"), dependencies = TRUE)
```
3. Create an `.Rprofile` file with the following content:
```
FIG_DIR <- 'figs' # directory to export figures to
TAB_DIR <- 'tabs' # directory to export model coefficient tables to
OSU_USAGE_DATA_FILE <- '../data/<rds file name>'
CASE_OSU_RESULTS_DIR <- '../caseresults' # directory where to store the models

REDIS_HOST_FILE <- 'redis/localhost.txt' # file specifying hostname and port
REDIS_PWD <- 'password' # server AUTH password

source('include.R')
```
Change file and directory paths as needed.

4. Restart the R session. This should now automatically run the `.Rprofile` file, which you can tell by the output in the console on start-up. The `include.R` script loads all required packages and functions.

You should now be able to run all functions and scripts. Running simulation studies requires a Redis database server to be configured.


# Redis database
The Redis database stores the open jobs as well as the results of completed jobs. Parallel workers fetch jobs from the Redis queue, and store result in the respective experiment set. The benefit of storing results in the database is that it avoids the rather large file system overhead from saving thousands of small result files.

## Installing Redis server
### Windows
1. Download the Redis binaries. Older binaries are available at https://github.com/microsoftarchive/redis/ ([download link](https://github.com/microsoftarchive/redis/releases/download/win-3.2.100/Redis-x64-3.2.100.msi))
2. Install Redis
    1. Make sure Redis is added to your system's `PATH` environment variable.
    2. Let Redis use the default port (6379).

### Unix
WIP
1. set `BASEDIR` in `redis.ksh`

## Starting Redis server
You need to start the Redis server before you can run simulations or retrieve simulation results.

The Redis configuration file included in the repository [here](https://github.com/philips-labs/comparison-clustering-longitudinal-data/blob/main/redis/redis.conf) configures a server on port 6379 with password "password" and database saved to `redis/database.rdb`. A server password is required because the simulation R code connects to Redis using authentication.

### Windows
In order to start the Redis server on Windows, run `redis.bat`. Alternatively, you can open the command line in the root repository directory and execute `redis-server redis/redis.conf`
If everything is configured correctly, you should see the following window:
![image](https://user-images.githubusercontent.com/8193083/133419959-81d09c0d-2d8d-4392-8d66-ef09d95d8fb4.png)

If no window shows up, that indicates the Redis server failed to start. First check if the database directory path exists.

### Unix
From the root directory of the repository, run
```
redis-server redis/redis.conf
```

### Connect to Redis
After you have confirmed that the Redis server is running and you have opened an R session with all scripts loaded, connect to Redis in R by running `redis_connect()`. You should see the message "_Connected to Redis at localhost:6379._".

# Running simulations
To start a simulation worker on Windows, run `worker.bat`. 
However, for this to work, `R` needs to be in your `PATH` environment variable so Windows can locate the R executable file.
On Linux, in the command line from the repository directory, run
```
R --slave -f redis/worker.R
```
On computational clusters, you can start worker batch jobs in a similar manner.

You can start as many workers as your system allows. The workers will pull jobs from the queue and evaluate them. When no more jobs are open, the workers will terminate.

