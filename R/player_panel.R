##################
#  PLAYER DATA   #
##################

library(tidyverse)
library(plyr)
library(grid)
library(gridExtra)
library(jpeg)


# bucks shot data
bucks <- read_csv("raw/player-feature/bucksdata.csv")

#court image
court.url <- "http://espaciofutbol.co/wp-content/uploads/2019/04/how-big-is-half-court-basketball-half-basketball-court-big-5-basketball-court-big-3-basketball-court-size.jpg"
court <- rasterGrob(readJPEG(getURLContent(court.url)),
                    width = unit(1, "npc"), height = unit(1, "npc"))

#shot percentage plot
shotdataZone <- bucks[which(!bucks$SHOT_ZONE_BASIC == "Backcourt"), ]

#summarise
percentage <- ddply(shotdataZone, .(SHOT_ZONE_BASIC), summarize,
                    SHOTS_ATTEMPED = length(SHOT_MADE_FLAG),
                    SHOTS_MADE = sum(as.numeric(as.character(SHOT_MADE_FLAG))),
                    MLOC_X = mean(LOC_X),
                    MLOC_Y = mean(LOC_Y),
                    ACC = round((SHOTS_MADE/SHOTS_ATTEMPED),2),
                    PERC_CHAR = paste(as.character(round(100*(SHOTS_MADE/SHOTS_ATTEMPED),2)),"%", sep = "")) %>% 
  mutate(SHOT_ZONE_BASIC = as.character(SHOT_ZONE_BASIC))

summary <- c("Summary", sum(percentage$SHOTS_ATTEMPED), sum(percentage$SHOTS_MADE), round(mean(percentage$ACC),2))

percentage.box <- percentage %>% 
  select(SHOT_ZONE_BASIC, SHOTS_ATTEMPED, SHOTS_MADE, ACC) %>% 
  rbind(summary)

#player performance
shot.plot <- bucks %>%
  mutate(HTM = as.character(HTM),
         VTM = as.character(VTM)) %>% 
  mutate(OPP = as.factor(ifelse(HTM != "MIL", HTM, VTM))) %>% 
  select(-HTM, -VTM) %>% 
  filter(PERIOD <= 5)

shot.recap <- shot.plot %>% 
  select(PLAYER_NAME, OPP, GAME_DATE, SHOT_ATTEMPTED_FLAG, SHOT_MADE_FLAG) %>% 
  group_by(PLAYER_NAME, OPP, GAME_DATE) %>% 
  dplyr::summarise(total.attemp = length(SHOT_ATTEMPTED_FLAG),
                   total.made = sum(as.numeric(as.character(SHOT_MADE_FLAG)))) %>% 
  mutate(PROP = round(total.made/total.attemp * 100, 2))

#shot against all team
recap <- shot.recap %>% select(PLAYER_NAME, OPP, PROP) %>% 
  group_by(PLAYER_NAME, OPP) %>% 
  dplyr::summarise(MIN = min(PROP),
                   MAX = max(PROP)) %>% 
  ungroup()

#shot distribution
shot.dist <- bucks %>% 
  select(PLAYER_NAME, SHOT_DISTANCE, EVENT_TYPE) %>% 
  group_by(PLAYER_NAME, EVENT_TYPE, SHOT_DISTANCE) %>% 
  tally()

#Shot tendency
shot <- bucks %>% #feature selection
  select(PLAYER_NAME, ACTION_TYPE) %>% 
  mutate(SHOT_TYPE = ifelse(grepl("dunk", ACTION_TYPE, ignore.case = TRUE), "Dunk",
                            ifelse(grepl("layup", ACTION_TYPE, ignore.case = TRUE), "Layup",
                                   ifelse(grepl("hook", ACTION_TYPE, ignore.case = TRUE), "Post", "Jump Shot")))) %>% 
  select(PLAYER_NAME, SHOT_TYPE) %>% 
  mutate(SHOT_TYPE = as.factor(SHOT_TYPE),
         PLAYER_NAME = as.factor(PLAYER_NAME)) %>% 
  dplyr::group_by(PLAYER_NAME, SHOT_TYPE) %>%
  tally() 

perc <- c() #get percentage
for(i in 1:nrow(shot)){
  perc <- append(perc, shot %>% filter(PLAYER_NAME == unique(shot$PLAYER_NAME[i])) 
                 %>% mutate(prop = round(n/sum(n)*100, 2)))
}

tableprop <- data.frame(matrix(unlist(perc), ncol = max(length(perc)), byrow = FALSE)) #percentage to dataframe
prop <- tableprop[, c(4, 20, 45, 61, 77, 93, 109, 125, 150, 166, 182)] #select useable feature

propbind <- data.frame(prop[1], stack(prop[1:ncol(prop)])) %>% select(values) %>% dplyr::rename("prop" = values) #columns to a column


don.prop <- shot %>% filter(PLAYER_NAME == "Donte DiVincenzo") %>% #donte
  summarise(prop = round(n/sum(n)*100, 2)) #row 45:47

kor.prop <- shot %>% filter(PLAYER_NAME == "Kyle Korver") %>% #korver
  summarise(prop = round(n/sum(n)*100, 2)) #row 48:50

wes.prop <- shot %>% filter(PLAYER_NAME == "Wesley Matthews") %>% #wes matthews
  summarise(prop = round(n/sum(n)*100, 2)) #row 51:53

propbind.final <- rbind(propbind, don.prop, kor.prop, wes.prop) #bind missing data
prop.all <- propbind.final[c(1:8, 45:47, 9:32, 48:50, 33:44, 51:53), ] #rows arrangement

shot$PROP <- prop.all


#player bio
bio <- read.xlsx("raw/player-feature/bio.xlsx", sheetIndex = 1) 
bio <- bio %>% column_to_rownames(var = "Bio") %>%
  dplyr::rename("Giannis Antetokounmpo" = Giannis.Antetokounmpo,
                "Kyle Korver" = Kyle.Korver,
                "George Hill" = George.Hill,
                "Khris Middleton" = Khris.Middleton,
                "Eric Bledsoe" = Eric.Bledsoe,
                "Wesley Matthews" = Wesley.Matthews,
                "Brook Lopez" = Brook.Lopez,
                "Robin Lopez" = Robin.Lopez,
                "Donte DiVincenzo" = Donte.DiVincenzo,
                "D.J. Wilson" = D.J..Wilson,
                "Pat Connaughton" = Pat.Connaughton,
                "Ersan Ilyasova" = Ersan.Ilyasova,
                "Dragan Bender" = Dragan.Bender,
                "Sterling Brown" = Sterling.Brown)