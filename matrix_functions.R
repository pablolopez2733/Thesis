#------------------SCRIPT FOR REGRESSION MODELS-------------------------------
# Libraries--------------------------------------------------------------------
library(tidyverse)
library(dplyr)
library(readr)


# Read Data--------------------------------------------------------------------
lineup_stats <- read_csv("https://github.com/pablolopez2733/Thesis/blob/main/Data/lineup_stats.csv?raw=true")
grouped_pm <- read.csv("https://github.com/pablolopez2733/Thesis/blob/main/Data/grouped_lineup_stats.csv?raw=true")

lineup_stats_playoffs <- read_csv("https://github.com/pablolopez2733/Thesis/raw/main/Data/lineup_stats_playoffs.csv")
grouped_pm_playoffs <- read_csv("https://github.com/pablolopez2733/Thesis/raw/main/Data/grouped_lineup_stats_playoffs.csv")
# Function x_matrix FOR SEASONS------------------------------------------------------------
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

# Function Y_Vector FOR SEASONS-------------------------------------------------------------
# 'Returns the 82 x 1 vector of point differential for every game
# 'of a given team. 
y_vector <- function(team){
  # game_res has results and scores for every 2014-2015 regular season game
  game_res <- lineup_stats %>% 
    group_by(idGame,slugTeam) %>%
    summarise(
      pt_dif = sum(netScoreTeam),
      pts = last(finalScoreTeam)
    )
  # team_res just has number of points scored in each game by each team
  team_res <- game_res %>% 
    filter(slugTeam == team)
  
  num_vec <- team_res$pt_dif
  
  return(num_vec)
  
}


# Function x_matrix_playoffs for playoffs
x_matrix_playoffs <- function(team){
  
  #Count games
  counter <- lineup_stats_playoffs %>% 
    filter(slugTeam == team) %>% 
    group_by(idGame)
  n_games <- nrow(as.data.frame(unique(counter$idGame)))
  
  team_series <- lineup_stats_playoffs %>% 
    filter(slugTeam == team) %>% 
    group_by(idGame,lineup) %>% 
    summarise(
      time_played = sum(totalTime),
      game_pm = sum(netScoreTeam)
      
    ) %>% 
    left_join(grouped_pm_playoffs, by = c("lineup" = "lineup"))
  # This should output a tibble where we have the lineups used in every game
  # their time, and pm.
  
  # Create matrix
  N_i <- nrow(as.data.frame(unique(team_series$l_id)))
  X <- matrix(0L, nrow = n_games, ncol = N_i)
  
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


y_vector_playoffs<- function(team){
  # game_res has results and scores for every 2014-2015 regular season game
  game_res <- lineup_stats_playoffs %>% 
    group_by(idGame,slugTeam) %>%
    summarise(
      pt_dif = sum(netScoreTeam),
      pts = last(finalScoreTeam)
    )
  # team_res just has number of points scored in each game by each team
  team_res <- game_res %>% 
    filter(slugTeam == team)
  
  num_vec <- team_res$pt_dif
  
  return(num_vec)
  
}


#Test---------------------------------------------------------------------------
# Test with Cleveland Cavaliers:
test_clevx <- x_matrix("CLE")
test_clevy <- y_vector("CLE")
test_clevx_test <- x_matrix_playoffs("CLE")
test_clevy_test <- y_vector_playoffs("CLE")


