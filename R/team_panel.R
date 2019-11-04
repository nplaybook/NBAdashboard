#################
#   TEAM DATA   #
#################

source("R/nbateam.R")
library(tidyverse)
library(fpc) #take kmeans value

#read data
milwaukee <- read_csv("raw/team-feature/milwaukee.csv") %>% 
  arrange(desc(PTS)) %>% slice(1:12)
boston <- read_csv("raw/team-feature/boston.csv") %>% 
  arrange(desc(PTS)) %>% slice(1:12)

philadelphia <- read_csv("raw/team-feature/philadelphia.csv") %>% 
  arrange(desc(PTS)) %>% slice(1:12)

golden.state <- read_csv("raw/team-feature/golden_state.csv") %>% 
  arrange(desc(PTS)) %>% slice(1:12)

houston <- read_csv("raw/team-feature/houston.csv") %>% 
  arrange(desc(PTS)) %>% slice(1:12)

chicago <- read_csv("raw/team-feature/chicago.csv") %>% 
  arrange(desc(PTS))
chicago[[2]][7] <- "Tomas Satoransky"

lakers <- read_csv("raw/team-feature/lakers.csv") %>% 
  arrange(desc(PTS))

#data binding
player_pole <- rbind(milwaukee, philadelphia, boston, houston, golden.state, chicago, lakers) %>% 
  select(-X1, -Signed_Using) %>% 
  remove_rownames() 

#plot data
warriors.rating <- team.ability(golden.state)
milwaukee.rating <- team.ability(milwaukee)
chicago.rating <- team.ability(chicago)
lakers.rating <- team.ability(lakers)
houston.rating <- team.ability(houston)
philadelphia.rating <- team.ability(philadelphia)
boston.rating <- team.ability(boston)

rating <- rbind(lakers.rating, milwaukee.rating, houston.rating, boston.rating, philadelphia.rating, chicago.rating, warriors.rating)%>%
  sapply(standardize) %>% 
  t() %>% 
  data.frame() %>%
  rownames_to_column() %>% 
  dplyr::rename(Stats = rowname,
                Lakers = X1,
                Milwaukee = X2,
                Houston = X3,
                Boston = X4, 
                Philadelphia = X5,
                Chicago = X6,
                Golden_State = X7)#-2 to 2 

#boxscore
mil.score <- boxscore(milwaukee) %>% mutate(Team = "Milwaukee")
bos.score <- boxscore(boston) %>% mutate(Team = "Boston")
phi.score <- boxscore(philadelphia) %>% mutate(Team = "Philadelphia")
hou.score <- boxscore(houston) %>% mutate(Team = "Houston")
gsw.score <- boxscore(golden.state) %>% mutate(Team = "Golden_State")
chi.score <- boxscore(chicago) %>% mutate(Team = "Chicago")
lal.score <- boxscore(lakers) %>% mutate(Team = "Lakers")


team.score <- rbind(mil.score, bos.score, phi.score, hou.score, gsw.score, chi.score, lal.score) %>% 
  mutate(Team = as.factor(Team))

#kmeans
player <- player_pole %>% 
  select(-Age, -`2019-20`) %>% 
  remove_rownames() %>% 
  column_to_rownames(var = "Player") %>% 
  mutate(`3PP` = ifelse(is.na(`3PP`), 0, `3PP`)) %>% 
  dplyr::rename()

RNGkind()
set.seed(100)
player.kmeans <- kmeans(player, 6)
cluster.data <- as.data.frame(player.kmeans$cluster) %>% 
  dplyr::rename(group = `player.kmeans$cluster`)

dp <- discrproj(player, player.kmeans$cluster)
proj <- data.frame(dp$proj[,c(1,2)]) %>% 
  add_column(cluster = cluster.data$group) %>% 
  rownames_to_column() %>% 
  dplyr::rename(Player = rowname) %>% 
  mutate(color = ifelse(cluster == 1, "#176BEF",
                        ifelse(cluster == 2, "#FF3E30", 
                               ifelse(cluster == 3, "#F7B529",
                                      ifelse(cluster == 4, "#179C52", 
                                             ifelse(cluster == 5, "#B22F2B", "#ED553B")
                                      )
                               )
                        )
         )
  )

#cluster pole
cluster.pole <- player_pole %>% 
  left_join(proj, by = "Player") %>% 
  select(-X1, -X2, -color) %>%
  dplyr::rename(Salary = `2019-20`)

#cluster network
proj.network <- proj %>% 
  select(Player, cluster)

nodes <- data.frame(name = unique(c(proj.network$cluster, proj$Player)))

nodes$group <- nodes$name %in% proj.network$cluster
nodes$size <- 100

links <- data.frame(source = match(proj.network$cluster, nodes$name)-1,
                    target = match(proj.network$Player, nodes$name)-1)

col.ramp <- colorRampPalette(c("#FFFF00", "#FF0000"), bias = nrow(links), space = "rgb", interpolate = "linear")

colCodes <- col.ramp(length(unique(links$source)))
edges_col <- sapply(links$source, function(x) colCodes[which(sort(unique(links$source)) == x )])

#update player pole with cluster
pole <- player_pole %>% 
  add_column(Cluster = proj$cluster) %>% 
  select(-Age, -`2019-20`, -Player) %>% 
  group_by(Cluster) %>% 
  summarise_all(mean, na.rm = TRUE) %>% 
  mutate_at(vars(FGM:PTS), standardize) %>% 
  select(-FGM, -FGA, -`3PM`, -`3PA`, -FTM, -FTA) %>% 
  t() %>% 
  data.frame() %>% 
  rownames_to_column() %>% 
  dplyr::rename(Stats = rowname,
                "c1" = X1,
                "c2" = X2,
                "c3" = X3,
                "c4" = X4,
                "c5" = X5,
                "c6" = X6) %>% 
  slice(2:13)

color.code <- data.frame("#00461B", "#008347", "#003DA5", "#D31145", "#0D6BB6", "#CE1141", "#552583") %>% 
  mutate_if(is.factor, as.character) 
colnames(color.code) <- c("Milwaukee", "Boston", "Philadelphia", "Houston", "Golden_State", "Chicago", "Lakers")

#unseen data
unseen <- read_csv("raw/team-feature/unseen2019.csv") %>% 
  select(-X1, -playoff) %>% 
  dplyr::rename("Player" = player_name)

merge <- unseen %>% 
  inner_join(player_pole, by = "Player") %>% 
  select(Player:pace_impact) %>% 
  rbind(unseen %>% 
          dplyr::filter(Player == c("PJ Tucker", "Danuel House Jr.", "Otto Porter Jr.", "Wendell Carter Jr.", "James Ennis III"))) %>% 
  dplyr::group_by(Player) %>% 
  summarise_each(funs(mean))


#match statisrics
BucksMatch <- xlsx::read.xlsx("raw/team-feature/bucksmatch.xlsx", sheetIndex = 1)
BucksMatch <- BucksMatch %>% 
  dplyr::rename("FGM" = "FG",
                "FG%" = "FG.",
                "3PM" = "X3P",
                "3PA" = "X3PA",
                "3P%" = "X3P.",
                "FTM" = "FT",
                "FT%" = "FT.") %>% 
  mutate(MP = lubridate::ms(substr(as.character(BucksMatch$MP), start = 12 ,stop = 16))) %>%
  mutate_if(is.character, as.numeric) %>%
  mutate(`FG%` = as.numeric(as.character(`FG%`)),
         `3P%` = as.numeric(as.character(`3P%`)),
         `FT%` = as.numeric(as.character(`FT%`))) %>%
  mutate(`FG%` = ifelse(`FG%` == 1000.000, 1.00, `FG%`)*100,
         `3P%` = ifelse(`3P%` == 1000.000, 1.00, `3P%`)*100,
         `FT%` = ifelse(`FT%` == 1000.000, 1.00, `FT%`)*100)