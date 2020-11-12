#------------------SCRIPT FOR REGRESSION MODELS-------------------------------
# Libraries--------------------------------------------------------------------
library(tidyverse)
library(dplyr)
library(readr)


# Read Data--------------------------------------------------------------------
lineup_stats <- read_csv("https://github.com/pablolopez2733/Thesis/blob/main/Data/lineup_stats.csv?raw=true")
grouped_pm <- read.csv("https://github.com/pablolopez2733/Thesis/blob/main/Data/grouped_lineup_stats.csv?raw=true")

# Function x_matrix------------------------------------------------------------
# 'Returns the N_i x 82 matrix for the team given as an argument.
x_matrix <- function(team){
  team_series <- lineup_stats %>% 
    filter(slugTeam == team) %>% 
    group_by(idGame,lineup) %>% 
    summarise(
      time_played = sum(totalTime),
      game_pm = sum(netScoreTeam)
      
    ) %>% 
    left_join(grouped_pm, by = c("lineup" = "lineup"))
  # This should output a tibble where we have the lineups used in every game
  # their time, and pm.
  
  # Create matrix
  N_i <- nrow(as.data.frame(unique(team_series$l_id)))
  X <- matrix(0L, nrow = 82, ncol = N_i)
  
  #Rename matrix so that columns and rows are lineup_ids:
  colnames(X) <- unique(team_series$l_id) 
  rownames(X) <- unique(team_series$idGame)
  
  
  lineups <- colnames(X)
  games <- rownames(X)
  # Iterate through all the possible lineups and games
  for(l in lineups){
    for (g in games) {
      dato <- team_series[which(team_series$idGame == as.double(g) & team_series$l_id == as.double(l)),]$time_played
      # Because a lineup may not have appeared in a game, we need to account for
      # length zero values
      if(length(dato) == 0)
        X[g,l] <- 0
      else
        X[g,l] <- dato
    }

  }
  
  return(X)
  
}

# Function Y_Vector-------------------------------------------------------------
# 'Returns the 82 x 1 vector of point differential for every game
# 'of a given team. 
y_vector <- function(team){
  
}


#Test---------------------------------------------------------------------------
# Test with Cleveland Cavaliers:
test_clevx <- x_matrix("CLE")
