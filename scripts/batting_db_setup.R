#sourceDir <- "C:/Users/Tim/Documents/OR Stuff/BaseballStats/LahmanData/baseballdatabank-2017.1/core"
dataDir <- "~/Data/Sports/Baseball/Lahman"
batting_raw <- fread(paste0(dataDir,"/Source/Batting.csv"),data.table = FALSE)
fielding <- fread(paste0(dataDir,"/Source/Fielding.csv"),data.table = FALSE)
master <- fread(paste0(dataDir,"/Source/Master.csv"),data.table = FALSE)

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
#md <- src_sqlite("/Data/lahman2017.sqlite3",create = T)
### put tables in db
lahman_sqlite("./data")
### create new table of rookie year and birth year for each playerID
con <- dbConnect(SQLite(),"./data/lahman.sqlite")
### create new table of primary position for each playerID and year
primaryPos <- dbGetQuery(con,"SELECT * FROM fielding") %>%
  group_by(playerID,POS) %>%
  summarise(num_games=sum(G)) %>% 
  group_by(playerID) %>%
  summarise(max_games=max(num_games),primaryPos=POS[which.max(num_games)])
dbWriteTable(con,"t_PrimaryPos",primaryPos)
### create new batting table to be post-deadball subset of original, with 
    ### age and years of experience and position added to each line
master$debutYear <- sapply(master$debut, function(x){as.integer(substr(x,1,4))})
batting_enriched <- left_join(batting_raw,select(master,playerID,birthYear,debutYear)) %>%
  left_join(select(primaryPos,playerID,primaryPos)) %>%
  mutate(age=yearID-birthYear,YOE=yearID-debutYear+1)
# make cumulative tables for each playerID + yearID (and hence age and YOE) combo
cumBatting <- data.frame(playerID="",yearID=0,primaryPos="",age=0,YOE=0,AB=0,H=0,X2B=0,
                         X3B=0,HR=0,RBI=0,SB=0,CS=0,BB=0,SO=0,IBB=0,HBP=0)
names(batting_enriched)[10:11] <- c("X2B","X3B")
for (pid in unique(batting_enriched$playerID)){
  t1 = filter(batting_enriched,playerID==pid)
  yr = unique(t1$yearID)
  pPos=unique(t1$primaryPos)[1]
  for (yid in seq(min(yr),max(yr),by=1)) {
    t2 = filter(t1,yearID<=yid) %>%
      select(playerID,yearID,age,YOE,AB,H,X2B,X3B,HR,RBI,SB,CS,BB,SO,IBB,HBP) %>%
      group_by(playerID) %>%
      summarise(yearID=yid,primaryPos=pPos,age=max(age),YOE=max(YOE),AB=sum(AB),H=sum(H),X2B=sum(X2B),
                X3B=sum(X3B),HR=sum(HR),RBI=sum(RBI),SB=sum(SB),CS=sum(CS),
                BB=sum(BB),SO=sum(SO),IBB=sum(IBB),HBP=sum(HBP)) 
    cumBatting <- rbind(cumBatting,t2)
  }
}
dbWriteTable(con,"t_CumBatting",cumBatting)
dbDisconnect(con)