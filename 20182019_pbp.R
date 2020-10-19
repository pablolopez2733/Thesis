library(readr)
library(dplyr)

pbp <- read_csv("C:/Users/pablo/Desktop/ITAM/Tesis/datos_de_nba/20182019pbp.csv")

agrupar <- pbp %>%
  group_by(game_id,
           home_player_1_id,home_player_2_id,home_player_3_id,home_player_4_id,home_player_5_id,
           away_player_1_id,away_player_2_id,away_player_3_id,away_player_4_id,away_player_5_id)