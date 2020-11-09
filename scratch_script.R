#-------------------------------------------------------------------------------
#subtitution Stochastic Model
#-------------------------------------------------------------------------------

#--------------------LIBRARIES--------------------------------------------------
library(tidyverse)
library(dplyr)
library(readr)
#-------------------------------------------------------------------------------
lineup_stats <- read_csv("https://github.com/pablolopez2733/Thesis/blob/main/Data/lineup_stats.csv?raw=true")
grouped_pm <- read.csv("https://github.com/pablolopez2733/Thesis/blob/main/Data/grouped_lineup_stats.csv?raw=true")

grouped_pm$l_id <- 1:nrow(grouped_pm)
#Now every lineup for every team has a unique_id
#Thus, grouped_pm will act as a dictionary

teams <- unique(grouped_pm$slugTeam)


#-------FUNCTION FOR CALCULATING TRANSITION MATRIX FOR GIVEN TEAM------------------

transition_matrix <- function(team){
  team_df <- lineup_stats %>% filter(slugTeam == team) #get team df (as sort of time series)
  team_df <- team_df %>% 
    left_join(grouped_pm, by = c("lineup" = "lineup")) #add l_id for each lineup
  
  dimension <- nrow(as.data.frame(unique(team_df$l_id)))
  
  tm <- matrix(0L, nrow = dimension, ncol = dimension) #create matrix
  #rename matrix so that columns and rows are lineup_ids:
  colnames(tm) <- unique(team_df$l_id) 
  rownames(tm) <- unique(team_df$l_id)
  
  #create columns for the transitions
  team_df<- team_df %>% 
    mutate(
      t_1 = lag(l_id),
      t_2 = l_id
    )
  
  #iterate through df to fill the matrix:
  for (i in 2:nrow(team_df)) {
    tm[toString(team_df$t_1[i]),toString(team_df$t_2[i])] = tm[toString(team_df$t_1[i]),toString(team_df$t_2[i])] + 1
  }
  
  return(tm)
  
}

#test it:
test_cleveland <- transition_matrix("CLE")

#IT  WORKS!!!!!!!!!! :D
