#-------------------------------------
#Data Prep for Markov Model
#reference: https://nbainrstats.netlify.app/post/adding-lineups-to-nba-play-by-play-data/
#------------------------------------


#Libraries----------------------------------------------------------------------
library(readr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(zoo)
library(nbastatR)   # devtools::install_github("abresler/nbastatR")
library(future)
#-------------------------------------------------------------------------------

#Extract data from nbastatR into a csv hosted in Github:
schedule_1415 <- seasons_schedule(seasons = c(2015))
play_logs_all <- play_by_play_v2(game_ids = unique(schedule_1415$idGame))
write.csv(play_logs_all,
          "C:/Users/pablo/Desktop/ITAM/Tesis/datos_de_nbapbp_1415.csv",
          row.names = FALSE)

#Read Data:
pbp_1415 <- read_csv("https://github.com/pablolopez2733/Thesis/blob/main/pbp_1415.csv?raw=true")