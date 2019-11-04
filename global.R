library(shiny)
library(shinydashboardPlus)
library(shinyWidgets)
library(shinycssloaders)
library(highcharter)
library(glue)
library(ggplot2)
library(kableExtra)
library(lubridate)
library(formattable)
library(hms)
library(DT)
library(xlsx)
library(RCurl)
source("R/team_panel.R")
source("R/player_panel.R") 
load("MLmodel.rda")

convert.money <- function(money){
  str_money <- strsplit(as.character(money), "")
  answer <- ""
  counter <- 0
  for(i in seq(length(str_money[[1]]), 1, -1)){
    if (counter > 2){
      answer <- paste0(",", answer)
      counter <- 0
    }
    counter <- counter + 1
    answer <- paste0(str_money[[1]][i], answer)
  }
  return(answer)
}

print.money <- function(x, ...) {
  print.default(paste0("$", formatC(as.numeric(x), format="f", digits=2, big.mark=",")))
}
