#team cleaning
team_clean <- function(team_code){
  team <- team_code
  team <- team %>% 
    mutate_all(function(x) gsub(pattern = "\\$", replacement = "", x)) %>% 
    mutate_all(function(x) gsub(pattern = ",", replacement = "", x)) %>% 
    dplyr::rename(Signed_Using = `Signed Using`) %>% 
    mutate(Player = factor(Player),
           Age = as.numeric(Age),
           `2019-20` = as.numeric(`2019-20`),
           `2020-21` = as.numeric(`2020-21`),
           `2021-22` = as.numeric(`2021-22`),
           `2022-21` = as.numeric(`2022-23`),
           `2023-24` = as.numeric(`2023-24`),
           `2024-25` = as.numeric(`2024-25`),
           Signed_Using = factor(Signed_Using),
           Guaranteed = as.numeric(Guaranteed))
  return(team)
}

#feature selection
team_salary_2019 <- function(team_code){
  team <- team_code
  team <- team %>% 
    select(Player, Age, Signed_Using, `2019-20`)
  return(team)
}

#top stat
boxscore <- function(team_code){
  
  pts <- team_code %>% 
    slice(which.max(PTS)) %>% 
    select(Player, PTS) %>% 
    dplyr::rename(Stats = PTS) %>% 
    mutate(Stats = paste0(as.character(Stats), " ppg")) 
  
  rebs <- team_code %>% 
    slice(which.max(TRB)) %>% 
    select(Player, TRB) %>% 
    dplyr::rename(Stats = TRB) %>% 
    mutate(Stats = paste0(as.character(Stats), " rpg"))
  
  ast <- team_code %>% 
    slice(which.max(AST)) %>% 
    select(Player, AST) %>% 
    dplyr::rename(Stats = AST) %>% 
    mutate(Stats = paste0(as.character(Stats), " apg"))
  
  stl <- team_code %>% 
    slice(which.max(STL)) %>% 
    select(Player, STL) %>% 
    dplyr::rename(Stats = STL) %>% 
    mutate(Stats = paste0(as.character(Stats), " spg"))
  
  blk <- team_code %>% 
    slice(which.max(BLK)) %>% 
    select(Player, BLK) %>%  
    dplyr::rename(Stats = BLK) %>% 
    mutate(Stats = paste0(as.character(Stats), " bpg"))
  
  box <- rbind(pts, rebs, ast, stl, blk)
  return(box)
}

#Plot data gather
team.ability <- function(team){
  team %>% 
    mutate(FGP = FGP*100, 
           `3PP` = `3PP`*100,
           FTP = FTP*100) %>% 
    select(FGP, `3PP`, FTP, ORB, AST, STL, BLK, DRB, TOV, PF) %>% 
    summarise_all(mean, na.rm = TRUE)
}

standardize <- function(x){
  x = (x - mean(x)) / pracma::std(x)
  return(x)
}