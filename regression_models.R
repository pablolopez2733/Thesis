#------------------SCRIPT FOR REGRESSION MODELS-------------------------------
#Libraries:
library(tidyverse)
library(dplyr)
library(readr)
#-----------------------------------------------------------------------------

#Read Data:
lineup_stats <- read_csv("https://github.com/pablolopez2733/Thesis/blob/main/Data/lineup_stats.csv?raw=true")
grouped_pm <- read.csv("https://github.com/pablolopez2733/Thesis/blob/main/Data/grouped_lineup_stats.csv?raw=true")

X_matrix <- function(team){
  team_series <- lineup_stats %>% 
    filter(slugTeam == team) %>% 
    group_by(idGame,lineup) %>% 
    summarise(
      time_played = sum(totalTime),
      game_pm = sum(netScoreTeam)
      
    ) %>% 
    left_join(grouped_pm, by = c("lineup" = "lineup")) %>% 
    
  #this should output a tibble where we have the lineups used in every game
  #their time, and pm. Seems to work
  N_i <- nrow(as.data.frame(unique(team_series$l_id)))
  X <- matrix(0L, nrow = 82, ncol = N_i) #create matrix
  #rename matrix so that columns and rows are lineup_ids:
  colnames(X) <- unique(team_series$l_id) 
  rownames(X) <- unique(team_df$idGame)
  
  
  
  
  
}