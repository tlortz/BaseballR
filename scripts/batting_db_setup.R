library(dplyr)
library(data.table)
library(DBI)
sourceDir <- "C:/Users/Tim/Documents/OR Stuff/BaseballStats/LahmanData/lahman_2015_csv/core"
batting_raw <- fread(paste0(sourceDir,"/Batting.csv"),data.table = FALSE)
fielding <- fread(paste0(sourceDir,"/Fielding.csv"),data.table = FALSE)
master <- fread(paste0(sourceDir,"/Master.csv"),data.table = FALSE)

## MonetDBLite approach...not working well ##
'
con <- dbConnect(MonetDBLite::MonetDBLite(),"/Data")
dbWriteTable(con,"batting",batting_raw)
dbWriteTable(con,"fielding",fielding)
dbWriteTable(con,"master",master)
q1 <- dbSendQuery(con, "SELECT * FROM master LIMIT 10")
fetch(q1)
dbDisconnect(con, shutdown=TRUE)
'

## SQLite approach
### create new sqlite db and connect to it
### put tables in db
### create new table of rookie year and birth year for each playerID
### create new table of primary position for each playerID and year
### create new batting table to be post-deadball subset of original, with 
    ### age and years of experience and position added to each line
