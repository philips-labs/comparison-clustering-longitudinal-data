# comparison-clustering-longitudinal-data
This repository contains all R code used in running and analyzing the simulation study and case study reported in the manuscript.

As the simulation study involves many simulation settings (over 27,000) and the estimation time of some methods was rather long, a custom parallel simulation framework was implemented for use on a computation cluster. While a computational cluster is not strictly needed if you are only interested in replicating a subset of the simulation scenarios or methods, you will need to configure a Redis database server (https://redis.io/) in order to run any simulations. The instructions are provided below.

The complete database of simulation results (600 MB) is available upon request.

# Getting started
1. Create an `.Rprofile` file specifying the paths:
```
FIG_DIR <- 'figs' # directory to export figures to
TAB_DIR <- 'tabs' # directory to export model coefficient tables to
OSU_USAGE_DATA_FILE <- '../data/<rds file name>'
CASE_OSU_RESULTS_DIR <- '../caseresults' # directory where to store the models
```
2. Either load the Rstudio project file `comparison.Rproj`, or start an R session with the working directory set to the root repository directory.
3. Load the required packages and functions by running `source('include.R')` in R. Likely you will need to install missing packages first.

You should now be able to run all functions and scripts. Running simulation studies requires a Redis database server to be configured.


# Redis database
WIP

The Redis database stores the open jobs as well as the results of completed jobs. Parallel workers fetch jobs from the Redis queue, and store result in the respective experiment set. The benefit of storing results in the database is that it avoids the rather large file system overhead from saving thousands of small result files.

## Installing Redis
### Windows
WIP
### Unix
WIP
1. set `BASEDIR` in `redis.ksh`

## Redis configuration
WIP
1. in `redis.conf`, update the dir variable to the directory containing the database
2a. Create text file with "HOSTNAME PORT" content
2b. set the `REDIS_HOST_FILE` variable in R to point to the text file
3. set the `REDIS_PWD` variable in R

To start a simulation worker, run `R --slave -f redis/worker.R` in the command line, from the code directory
