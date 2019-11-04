#player play by play detail

library(tidyverse)
library(RCurl) #URL purpose
library(rjson)

bucksID <- data.frame(Player = c("Giannis Antetokounmpo", "Kyle Korver", "Pat Connaughton", "Sterling Brown", 
                                 "Khris Middleton", "Dragan Bender", "Brook Lopez",  "Wesley Matthews", 
                                 "Ersan Ilyasova", "Eric Bledsoe", "D.J. Wilson", "George Hill", "Donte Divincenzo", 
                                 "Robin Lopez"),
                      ID = c(203507, 2594, 1626192, 1628425, 203114, 1627733, 
                             201572, 202083, 101141, 202339, 1628391, 201588, 1628978, 201577))

bucksurl <- c()
for(i in 1:length(bucksID$ID)){
  bucksurl <- append(bucksurl, paste("http://stats.nba.com/stats/shotchartdetail?CFID=33&CFPARAMS=2018-19&ContextFilter=&ContextMeasure=FGA&DateFrom=&DateTo=&GameID=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerID=",bucksID$ID[i],"&PlayerPosition=&PlusMinus=N&Position=&Rank=N&RookieYear=&Season=2018-19&SeasonSegment=&SeasonType=Regular+Season&TeamID=0&VsConference=&VsDivision=&mode=Advanced&showDetails=0&showShots=1&showZones=0", sep = ""))
}

giannis <- bucksurl[1]
korver <- bucksurl[2]
pat <- bucksurl[3]
brown <- bucksurl[4]
middleton <- bucksurl[5]
bender <- bucksurl[6]
blopez <- bucksurl[7]
wes <- bucksurl[8]
ersan <- bucksurl[9]
bledsoe <- bucksurl[10]
dj <- bucksurl[11]
hill <- bucksurl[12]
donte <- bucksurl[13]
rlopez<- bucksurl[14]

shotdataF <- c()
#giannis antetokounmpo
shotdata <- fromJSON(file = giannis, method="C")
shotdataF <- data.frame(matrix(unlist(shotdata$resultSets[[1]][[3]]), ncol = 24, byrow = TRUE))
#kyle korver
shotdata <- fromJSON(file = korver, method="C")
shotdataF <- rbind(shotdataF, data.frame(matrix(unlist(shotdata$resultSets[[1]][[3]]), ncol = 24, byrow = TRUE)))
#pat connaughton
shotdata <- fromJSON(file = pat, method="C")
shotdataF <- rbind(shotdataF, data.frame(matrix(unlist(shotdata$resultSets[[1]][[3]]), ncol = 24, byrow = TRUE)))
#sterling brown
shotdata <- fromJSON(file = brown, method="C")
shotdataF <- rbind(shotdataF, data.frame(matrix(unlist(shotdata$resultSets[[1]][[3]]), ncol = 24, byrow = TRUE)))
#khris middleton
shotdata <- fromJSON(file = middleton, method="C")
shotdataF <- rbind(shotdataF, data.frame(matrix(unlist(shotdata$resultSets[[1]][[3]]), ncol = 24, byrow = TRUE)))
#dragan bender
shotdata <- fromJSON(file = bender, method="C")
shotdataF <- rbind(shotdataF, data.frame(matrix(unlist(shotdata$resultSets[[1]][[3]]), ncol = 24, byrow = TRUE)))
#brook lopez
shotdata <- fromJSON(file = blopez, method="C")
shotdataF <- rbind(shotdataF, data.frame(matrix(unlist(shotdata$resultSets[[1]][[3]]), ncol = 24, byrow = TRUE)))
#wesley matthews
shotdata <- fromJSON(file = wes, method="C")
shotdataF <- rbind(shotdataF, data.frame(matrix(unlist(shotdata$resultSets[[1]][[3]]), ncol = 24, byrow = TRUE)))
#ersan ilyasova
shotdata <- fromJSON(file = ersan, method="C")
shotdataF <- rbind(shotdataF, data.frame(matrix(unlist(shotdata$resultSets[[1]][[3]]), ncol = 24, byrow = TRUE)))
#eric bledsoe
shotdata <- fromJSON(file = bledsoe, method="C")
shotdataF <- rbind(shotdataF, data.frame(matrix(unlist(shotdata$resultSets[[1]][[3]]), ncol = 24, byrow = TRUE)))
#dj wilson
shotdata <- fromJSON(file = dj, method="C")
shotdataF <- rbind(shotdataF, data.frame(matrix(unlist(shotdata$resultSets[[1]][[3]]), ncol = 24, byrow = TRUE)))
#george hill
shotdata <- fromJSON(file = hill, method="C")
shotdataF <- rbind(shotdataF, data.frame(matrix(unlist(shotdata$resultSets[[1]][[3]]), ncol = 24, byrow = TRUE)))
#donte divincenzo
shotdata <- fromJSON(file = donte, method="C")
shotdataF <- rbind(shotdataF, data.frame(matrix(unlist(shotdata$resultSets[[1]][[3]]), ncol = 24, byrow = TRUE)))
#robin lopez
shotdata <- fromJSON(file = rlopez, method="C")
shotdataF <- rbind(shotdataF, data.frame(matrix(unlist(shotdata$resultSets[[1]][[3]]), ncol = 24, byrow = TRUE)))
levels(shotdataF$X5)

#give colnames
shotdataHeader <- shotdata$resultSets[[1]][[2]]
colnames(shotdataF) <- shotdataHeader
shotdataF <- shotdataF %>% 
  mutate(GAME_DATE = lubridate::ymd(GAME_DATE))

#x and y coordinate
shotdataF$LOC_X <- as.numeric(as.character(shotdataF$LOC_X))
shotdataF$LOC_Y <- as.numeric(as.character(shotdataF$LOC_Y))
shotdataF$SHOT_DISTANCE <- as.numeric(as.character(shotdataF$SHOT_DISTANCE))

write.csv(shotdataF, file = "bucksdata.csv")