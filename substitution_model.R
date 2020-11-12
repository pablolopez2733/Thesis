#-------------------------------------------------------------------------------
#subtitution Stochastic Model
#-------------------------------------------------------------------------------

# Libraries--------------------------------------------------------------------
library(tidyverse)
library(dplyr)
library(readr)

##Read Data---------------------------------------------------------------------
lineup_stats <- read_csv("https://github.com/pablolopez2733/Thesis/blob/main/Data/lineup_stats.csv?raw=true")
grouped_pm <- read.csv("https://github.com/pablolopez2733/Thesis/blob/main/Data/grouped_lineup_stats.csv?raw=true")


#Now every lineup for every team has a unique_id
#Thus, grouped_pm will act as a dictionary
teams <- unique(grouped_pm$slugTeam)

# Function transition_matrix----------------------------------------------------
# 'Function for calculating the N_i x N_i matrix for a given team

transition_matrix <- function(team){
  # Get team df (as sort of time series)
  team_df <- lineup_stats %>% filter(slugTeam == team) 
  team_df <- team_df %>% 
    # Add l_id for each lineup 
    left_join(grouped_pm, by = c("lineup" = "lineup")) 
  
  dimension <- nrow(as.data.frame(unique(team_df$l_id)))
  # Create matrix
  tm <- matrix(0L, nrow = dimension, ncol = dimension) 
  # Rename matrix so that columns and rows are lineup_ids:
  colnames(tm) <- unique(team_df$l_id) 
  rownames(tm) <- unique(team_df$l_id)
  
  # Create columns for the transitions
  team_df<- team_df %>% 
    mutate(
      t_1 = lag(l_id),
      t_2 = l_id
    )
  
  # Iterate through df to fill the matrix:
  for (i in 2:nrow(team_df)) {
    tm[toString(team_df$t_1[i]),toString(team_df$t_2[i])] = tm[toString(team_df$t_1[i]),toString(team_df$t_2[i])] + 1
  }
  # Divide by time spent in origin state (to get MLE)
  N <- tm
  nombres <- rownames(tm)
  for(i in nombres){
    dato <- grouped_pm[which(grouped_pm$l_id==i),]$seasonSecs #which me regresa el renglon en donde el identificador corresponde con el valor pasado.
    N[i,] <- tm[i,]/dato
  }
  
  return(N)
  
}

# Test -------------------------------------------------------------------------
test_cleveland <- transition_matrix("CLE")

#NOTES--------------------------------------------------------------------------
# Should we try with minutes instead and filter out some?
# because: absorbent states and working with bigger numbers.
