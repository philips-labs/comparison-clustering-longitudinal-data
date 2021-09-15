# comparison-clustering-longitudinal-data
To load required packages and functions, run source('include.R') in R

See /redis/readme.txt for instructions on setting up the simulation framework.

Global settings to specify:
FIG_DIR: directory where to export figures to
TAB_DIR: directory where to export model coefficient tables to

OSU_USAGE_DATA_FILE: the path to the case study data file
CASE_OSU_RESULTS_DIR: directory where to store the models

# Redis configuration
1. set BASEDIR in redis.ksh
2. in redis.conf, update the dir variable to the directory containing the DB
3a. Create text file with "HOSTNAME PORT" content
3b. set REDIS_HOST_FILE variable in R to point to the text file
4. set REDIS_PWD variable in R

to start a simulation worker, run: R --slave -f redis/worker.R
from the command line, from the code directory