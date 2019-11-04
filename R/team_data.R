source("nbateam.R")
source("nbaplayer.R")
library(NBAloveR)
library(tidyverse)

---------------------------------------------------#[Milwaukee Bucks]#--------------------------------------------------

# Mirza Teletovic error

#team
mil <- getTeamSalary("mil")
mil$Player[mil$Player == "Frank Mason"] <- "Frank Mason III" #adjust with webpage name

##update salary from ESPN
mil$`2019-20`[which(mil$Player == "Frank Mason III")] <- "$1,378,242"
mil$`2019-20`[which(mil$Player == "Cameron Reynolds")] <- "$108,953"
mil$`2019-20`[which(mil$Player == "Jaylen Adams")] <- "$1,416,852"

mil_clean <- team_clean(mil)
mil_salary_2019 <- team_salary_2019(mil_clean)

##player with 2019 data
mil_remove_list <- c("Thanasis Antetokounmpo", "Luke Maye", "Rayjon Tucker", "Larry Sanders", 
                     "Spencer Hawes", "Mirza Teletovic", "Team Totals") 

mil_salary_2019 <- mil_salary_2019[!mil_salary_2019$Player %in% mil_remove_list, ]
mil_salary_2019 <- mil_salary_2019[-c(18,20), ] #Mirza Teletovic & George Hill

#player
mil_player_stat <- data.frame()
for (i in 1:5){mil_player_stat <- rbind(mil_player_stat, player_average_01(mil_salary_2019$Player[i]))}

mil_player_stat <- rbind(mil_player_stat, player_average_01("Ersan Ilyasova"))

for(j in 7:10){mil_player_stat <- rbind(mil_player_stat, player_average_01(mil_salary_2019$Player[j]))}

mil_player_stat <- rbind(mil_player_stat, player_average_02("Wesley Matthews"))

for(k in 12:13){mil_player_stat <- rbind(mil_player_stat, player_average_01(mil_salary_2019$Player[k]))}

mil_player_stat <- rbind(mil_player_stat, player_average_02("Sterling Brown"))

for(l in 15:nrow(mil_salary_2019)){mil_player_stat <- rbind(mil_player_stat, player_average_01(mil_salary_2019$Player[l]))}

#combine 2 dataframes
milwaukee <- cbind(mil_salary_2019, mil_player_stat) 
write.csv(milwaukee, file = "milwaukee.csv")


--------------------------------------------------#[Philadelphia 76ers]#--------------------------------------------------

# (8)Matisse Thybulle - rookie
# (16)Marial Shayok - rookie
# (17)Norvel Pelle - rookie
# (18)Isaiah Miles - rookie
# (19)Christ Koumadje - rookie


#team
phi <- getTeamSalary("phi")

##update salary from ESPN
phi$`2019-20`[which(phi$Player == "Shake Milton")] <- "$1,455,697"

phi_clean <- team_clean(phi)
phi_salary_2019 <- team_salary_2019(phi_clean)

phi_remove_list <- c("Matisse Thybulle", "Marial Shayok",
                     "Norvel Pelle", "Isaiah Miles", "Christ Koumadje", 
                     "Team Totals") 

phi_salary_2019 <- phi_salary_2019[!phi_salary_2019$Player %in% phi_remove_list, ]

#player
phi_player_stat <- data.frame()
phi_player_stat <- rbind(phi_player_stat, player_average_02("Tobias Harris"))

for (i in 2:nrow(phi_salary_2019)){
  phi_player_stat <- rbind(phi_player_stat, player_average_01(phi_salary_2019$Player[i]))}

#combine 2 dataframes
philadelphia <- cbind(phi_salary_2019, phi_player_stat)
write.csv(philadelphia, file = "philadelphia.csv")


---------------------------------------------------#[Boston Celtics]#---------------------------------------------------

# (8)Romeo Langford - out of bounds
# (9)Vincent Poirier - no data
# (10)Grant Williams - out of bounds
# (11)Robert Williams - out of bounds
# (14)Carsen Edwards - no data
# (17)Tremont Waters - no data
# (19)Tacko Fall - no data
# (20)Max Strus - no data

#team
bos <- getTeamSalary("bos")

##update info from ESPN
bos$`2019-20`[which(bos$Player == "Bryce Brown")] <- "$898,310"
bos$Age[which(bos$Player == "Bryce Brown")] <- 22

bos_clean <- team_clean(bos)
bos_salary_2019 <- team_salary_2019(bos_clean)

bos_remove_list <- c("Romeo Langford","Vincent Poirier", "Grant Williams", "Robert Williams",
                     "Carsen Edwards", "Tremont Waters", "Tacko Fall","Max Strus", "Team Totals") 

bos_salary_2019 <- bos_salary_2019[!bos_salary_2019$Player %in% bos_remove_list, ]
bos_salary_2019 <- bos_salary_2019[-12, ] #2nd Bryce Brown

#player
bos_player_stat <- data.frame()

bos_player_stat <- rbind(bos_player_stat, player_average_02("Kemba Walker"))

for(i in 2:4){bos_player_stat <- rbind(bos_player_stat, player_average_01(bos_salary_2019$Player[i]))}

bos_player_stat <- rbind(bos_player_stat, player_average_02("Jaylen Brown"))

for (j in 6:nrow(bos_salary_2019)){
  bos_player_stat <- rbind(bos_player_stat, player_average_01(bos_salary_2019$Player[j]))}

#combine 2 dataframes
boston <- cbind(bos_salary_2019, bos_player_stat)
write.csv(boston, file = "boston.csv")


--------------------------------------------------#[Golden State Warriors]#---------------------------------------------

# (8)Jordan Poole - no data
# (11)Glenn Robinson III - different address
# (13)Eric Paschall - no data
# (14)Alen Smailagic - no data
# (15)Juan Toscano-Anderson - no data
# (16)Devyn Marble - no data 2019


#team
gsw <- getTeamSalary("gsw")
gsw$Player[gsw$Player == "Glenn Robinson"] <- "Glenn Robinson III"

gsw_clean <- team_clean(gsw)
gsw_salary_2019 <- team_salary_2019(gsw_clean)

gsw_remove_list <- c("Jordan Poole", "Eric Paschall", "Alen Smailagic", "Juan Toscano-Anderson", 
                     "Devyn Marble", "Damion Lee", "Ky Bowman", "Team Totals") 

gsw_salary_2019 <- gsw_salary_2019[!gsw_salary_2019$Player %in% gsw_remove_list, ]
gsw_salary_2019 <- gsw_salary_2019[-12, ] #Alen Smailagic

#player
gsw_player_stat <- data.frame()

for(i in 1:9){
  gsw_player_stat <- rbind(gsw_player_stat, player_average_01(gsw_salary_2019$Player[i]))
}

gsw_player_stat <- rbind(gsw_player_stat, player_average_02("Glenn Robinson III"))

for(j in 11:nrow(gsw_salary_2019)){
  gsw_player_stat <- rbind(gsw_player_stat, player_average_01(gsw_salary_2019$Player[j]))
}

#combine 2 dataframes
golden_state <- cbind(gsw_salary_2019, gsw_player_stat)
write.csv(golden_state, file = "golden_state.csv")


--------------------------------------------------#[Houston Rockets]#--------------------------------------------------

# (3)Clint Capela - different web address, approach with different function
# (7)Nene - wrong names, un subtitueable
# (13)Anthony Bennett - no data
# (14)Michael Frazier - no data
# (17)Chris Clemons - no data
# (18)Shamorie Ponds - no data


#team
hou <- getTeamSalary("hou")
hou_clean <- team_clean(hou)
hou_salary_2019 <- team_salary_2019(hou_clean)

hou_remove_list <- c("Clint Capela", "Anthony Bennett", "Michael Frazier",
                     "Chris Clemons", "Shamorie Ponds", "Team Totals")

hou_salary_2019 <- hou_salary_2019[!hou_salary_2019$Player %in% hou_remove_list, ]
hou_salary_2019 <- hou_salary_2019[-6, ] #Nene

#player
hou_player_stat <- data.frame()

for(i in 1:12){
  hou_player_stat <- rbind(hou_player_stat, player_average_01(hou_salary_2019$Player[i]))
}

hou_player_stat <- rbind(hou_player_stat, player_average_02("Troy Williams"))

#combine 2 dataframes
houston <- cbind(hou_salary_2019, hou_player_stat)
write.csv(houston, file = "houston.csv")


--------------------------------------------------#[Chicago Bulls]#--------------------------------------------------

chi <- getTeamSalary("chi")
chi_clean <- team_clean(chi)
chi_salary_2019 <- team_salary_2019(chi_clean)

chi.remove.list <- c("Cristiano Felício", "Coby White", "Denzel Valentine", 
                     "Daniel Gafford", "Shaquille Harrison", "Simisola Shittu", 
                     "Perrion Callandret", "Adam Mokoka", "Justin Simon", 
                     "Team Totals", "Ömer Asik")

chi_salary_2019 <- chi_salary_2019[!chi_salary_2019$Player %in% chi.remove.list, ]
chi_salary_2019 <- chi_salary_2019[-c(11,12), ]


chi.player.stat <- c()
for (i in 1:nrow(chi_salary_2019)){
  chi.player.stat <- rbind(chi.player.stat, player_average_01(chi_salary_2019$Player[i]))
}

chicago <- cbind(chi_salary_2019, chi.player.stat)
write.csv(chicago, file = "chicago.csv")

--------------------------------------------------#[Los Angeles Lakers]#--------------------------------------------------

lal <- getTeamSalary("lal")
lal_clean <- team_clean(lal)
lal_salary_2019 <- team_salary_2019(lal_clean)

lal_salary_2019 <- lal_salary_2019[-c(13, 15:22), ] #remove some players


lal.player.stat <- c()
for (i in 1){
  lal.player.stat <- rbind(lal.player.stat, player_average_01(lal_salary_2019$Player[i]))
}

for (i in 2:3){
  lal.player.stat <- rbind(lal.player.stat, player_average_02(lal_salary_2019$Player[i]))
}

for (i in 4:nrow(lal_salary_2019)){
  lal.player.stat <- rbind(lal.player.stat, player_average_01(lal_salary_2019$Player[i]))
}

lakers <- cbind(lal_salary_2019, lal.player.stat)
write.csv(lakers, file = "lakers.csv")