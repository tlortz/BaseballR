require(dplyr)
require(data.table)

dataDir <- "~/Data/Sports/Baseball/Lahman"
batting_raw <- fread(paste0(dataDir,"/Source/baseballdatabank-2017.1/core/Batting.csv"))#,data.table = FALSE)
fielding <- fread(paste0(dataDir,"/Source/baseballdatabank-2017.1/core/Fielding.csv"))#,data.table = FALSE)
master <- fread(paste0(dataDir,"/Source/baseballdatabank-2017.1/core/Master.csv"))#,data.table = FALSE)
fangraphs <- fread(paste0(dataDir,"/Prod/FranGraphsLeaderboard.csv"))#,data.table = FALSE)



### create new table of primary position for each playerID and year
primaryPos <- fielding %>%
  group_by(playerID,POS) %>%
  summarise(num_games=sum(G)) %>% 
  group_by(playerID) %>%
  summarise(max_games=max(num_games),primaryPos=POS[which.max(num_games)])

t <- fielding[,num_games:=sum(G),.(playerID,POS)]
primaryPos <- fielding[]

### create new batting table to be post-deadball subset of original, with 
    ### age and years of experience and position added to each line
master$debutYear <- sapply(master$debut, function(x){as.integer(substr(x,1,4))})
batting_enriched <- left_join(batting_raw,select(master,playerID,birthYear,debutYear)) %>%
  left_join(select(primaryPos,playerID,primaryPos)) %>%
  mutate(age=yearID-birthYear,YOE=yearID-debutYear+1)
names(batting_enriched)[10:11] <- c("X2B","X3B")
# add in wOBA & PA for each player-year, then all the league effects for each year from 
# http://www.fangraphs.com/library/offense/wrc/
# and boil this down to wRC+ for each player-year prior to creating cum_batting table
# make cumulative tables for each playerID + yearID (and hence age and YOE) combo
cumBatting <- data.frame(playerID="",yearID=0,primaryPos="",age=0,YOE=0,AB=0,H=0,X2B=0,
                         X3B=0,HR=0,RBI=0,SB=0,CS=0,BB=0,SO=0,IBB=0,HBP=0)
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

finalYearDF <- cumBatting %>%
  group_by(playerID,yearID) %>%
  summarise(yearID=max(yearID))

tailBatting <- data.frame(playerID="",yearID=0,primaryPos="",age=0,YOE=0,AB=0,H=0,X2B=0,
                         X3B=0,HR=0,RBI=0,SB=0,CS=0,BB=0,SO=0,IBB=0,HBP=0)
for (pid in unique(batting_enriched$playerID)){
  t1 = filter(batting_enriched,playerID==pid)
  yr = unique(t1$yearID)
  pPos=unique(t1$primaryPos)[1]
  if(min(yr)<max(yr)) {
    for (yid in seq(min(yr),max(yr)-1,by=1)) {
      t2 = filter(t1,yearID>yid) %>%
        select(playerID,yearID,age,YOE,AB,H,X2B,X3B,HR,RBI,SB,CS,BB,SO,IBB,HBP) %>%
        group_by(playerID) %>%
        summarise(yearID=yid,primaryPos=pPos,age=min(age),YOE=min(YOE),AB=sum(AB),H=sum(H),X2B=sum(X2B),
                  X3B=sum(X3B),HR=sum(HR),RBI=sum(RBI),SB=sum(SB),CS=sum(CS),
                  BB=sum(BB),SO=sum(SO),IBB=sum(IBB),HBP=sum(HBP)) 
    tailBatting <- rbind(tailBatting,t2)
    }
  }
  else{
    r = data.frame(yearID=min(yr),primaryPos=pPos,age=min(age),YOE=min(YOE),AB=sum(AB),H=sum(H),X2B=sum(X2B),
                   X3B=sum(X3B),HR=sum(HR),RBI=sum(RBI),SB=sum(SB),CS=sum(CS),
                   BB=sum(BB),SO=sum(SO),IBB=sum(IBB),HBP=sum(HBP))
    tailBatting <- rbind(tailBatting,r)
  }
}

fn_computeRC <- function(H,BB,B1,B2,B3,HR,AB) {
  return((H+BB)*(B1+2*B2+3*B3+4*HR)/(AB+BB))
}

cumBatting %>% mutate(Singles=H-X2B-X3B-HR,RC=fn_computeRC(H,BB,Singles,X2B,X3B,HR,AB)) %>%
  select(playerID,yearID,AB,H,Singles,X2B,X3B,HR,AB,BB,RC) %>%
  arrange(-yearID,playerID) %>%
  head(100)