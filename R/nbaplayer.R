##generate html page address
player_key_generation_01 = function(input){
  input = gsub(".","", input, fixed = TRUE)
  input = gsub("'","", input, fixed = TRUE)
  head_url = "https://www.basketball-reference.com/players/"
  end_num = "01"
  tail_url = paste0(substr(strsplit(input, " ")[[1]][2], 0,1), "/",
                    substr(strsplit(input, " ")[[1]][2], 1,5),
                    substr(strsplit(input, " ")[[1]][1], 0,2),
                    end_num) %>% 
    stringr::str_to_lower()
  player_url = paste0(head_url, tail_url, ".html/")
  
  name_on_page = xml2::read_html(player_url) %>% 
    rvest::html_nodes("div.players") %>% 
    rvest::html_nodes("[itemprop = name]") %>% 
    rvest::html_text()
  
  if (input == name_on_page){
    player_key = tail_url
  } else {
    player_key = stringr::str_replace(tail_url, "01", "01")
  }
  
  return(player_key)
}

player_key_generation_02 = function(input){
  input = gsub(".","", input, fixed = TRUE)
  input = gsub("'","", input, fixed = TRUE)
  head_url = "https://www.basketball-reference.com/players/"
  end_num = "02"
  tail_url = paste0(substr(strsplit(input, " ")[[1]][2], 0,1), "/",
                    substr(strsplit(input, " ")[[1]][2], 1,5),
                    substr(strsplit(input, " ")[[1]][1], 0,2),
                    end_num) %>% 
    stringr::str_to_lower()
  player_url = paste0(head_url, tail_url, ".html/")
  
  name_on_page = xml2::read_html(player_url) %>% 
    rvest::html_nodes("div.players") %>% 
    rvest::html_nodes("[itemprop = name]") %>% 
    rvest::html_text()
  
  if (input == name_on_page){
    player_key = tail_url
  } else {
    player_key = stringr::str_replace(tail_url, "02", "03")
  }
  
  return(player_key)
}


##get stats per game
getStatsPerGame_01 <- function(player, season, span=1){
  
  # Setting
  head_url <- "https://www.basketball-reference.com/players/"
  player_key = player_key_generation_01(player)
  end_url <- "/gamelog/"
  url_list <- paste0(head_url, player_key, end_url, season:(season+span-1), "/")
  
  
  if (length(url_list) == 1){
    url = url_list
    tables <- xml2::read_html(url) %>%
      rvest::html_table(fill = TRUE)
    main_df = data.frame(tables[[8]]) %>%
      dplyr::filter(data.frame(tables[[8]]) $Date != "Date")
    output_table = main_df[,1:29]
  } else if (length(url_list) > 1) {
    
    dt_list = list()
    for (i in 1:length(url_list)){
      url = url_list[[i]]
      tables <- xml2::read_html(url) %>%
        rvest::html_table(fill = TRUE)
      table <- data.frame(tables[[8]]) %>%
        dplyr::filter(data.frame(tables[[8]]) $Date != "Date")
      
      dt_list[[i]] = table
    }
    
    main_df = do.call(rbind, dt_list)
    output_table = main_df[,1:29]
  } else {
    warning("span has to be greater than 0")
  }
  output_table = output_table %>% 
    dplyr::select(-'Rk', -'Var.8')
  names(output_table)[c(5, 11:14, 17)] = c("Home", "FGP", "3PM", "3PA", "3PP", "FTP")
  output_table %>% 
    dplyr::filter(output_table$GS != "Inactive",
                  output_table$GS != "Did Not Dress",
                  output_table$GS != "Did Not Play")
}

getStatsPerGame_02 <- function(player, season, span=1){
  
  # Setting
  head_url <- "https://www.basketball-reference.com/players/"
  player_key = player_key_generation_02(player)
  end_url <- "/gamelog/"
  url_list <- paste0(head_url, player_key, end_url, season:(season+span-1), "/")
  
  
  if (length(url_list) == 1){
    url = url_list
    tables <- xml2::read_html(url) %>%
      rvest::html_table(fill = TRUE)
    main_df = data.frame(tables[[8]]) %>%
      dplyr::filter(data.frame(tables[[8]]) $Date != "Date")
    output_table = main_df[,1:29]
  } else if (length(url_list) > 1) {
    
    dt_list = list()
    for (i in 1:length(url_list)){
      url = url_list[[i]]
      tables <- xml2::read_html(url) %>%
        rvest::html_table(fill = TRUE)
      table <- data.frame(tables[[8]]) %>%
        dplyr::filter(data.frame(tables[[8]]) $Date != "Date")
      
      dt_list[[i]] = table
    }
    
    main_df = do.call(rbind, dt_list)
    output_table = main_df[,1:29]
  } else {
    warning("span has to be greater than 0")
  }
  output_table = output_table %>% 
    dplyr::select(-'Rk', -'Var.8')
  names(output_table)[c(5, 11:14, 17)] = c("Home", "FGP", "3PM", "3PA", "3PP", "FTP")
  output_table %>% 
    dplyr::filter(output_table$GS != "Inactive",
                  output_table$GS != "Did Not Dress",
                  output_table$GS != "Did Not Play")
}

##player cleansing
player_clean_01 <- function(player_name) {
  player <- getStatsPerGame_01(player_name, season = 2019, span = 1)
  player$MP <- lubridate::ms(player$MP)
  player <- player %>% 
    select(-G, -Date, -Age, -Tm, -Home, -Opp, -GS, -MP, -GmSc) %>%
    dplyr::rename(FGM = FG,
           FTM = FT) %>% 
    mutate_if(is.character, as.numeric)
  return(player)
}

player_clean_02 <- function(player_name) {
  player <- getStatsPerGame_02(player_name, season = 2019, span = 1)
  player$MP <- lubridate::ms(player$MP)
  player <- player %>% 
    select(-G, -Date, -Age, -Tm, -Home, -Opp, -GS, -MP, -GmSc) %>%
    dplyr::rename(FGM = FG,
                  FTM = FT) %>% 
    mutate_if(is.character, as.numeric)
  return(player)
}

##player season average  
player_average_01 <- function(player_name){
  player <- player_clean_01(player_name)
  player <- as.data.frame(t(sapply(player, mean, na.rm = TRUE)))
  player$FGP <- player$FGM/player$FGA
  player$`3PP` <- player$`3PM`/player$`3PA`
  player$FTP <- player$FTM/player$FTA
  player <- player %>% 
    mutate_if(is.numeric, round, 2)
}

player_average_02 <- function(player_name){
  player <- player_clean_02(player_name)
  player <- as.data.frame(t(sapply(player, mean, na.rm = TRUE)))
  player$FGP <- player$FGM/player$FGA
  player$`3PP` <- player$`3PM`/player$`3PA`
  player$FTP <- player$FTM/player$FTA
  player <- player %>% 
    mutate_if(is.numeric, round, 2)
}