#Libraries----------------------------------------------------------------------
library(readr)
library(dplyr)
#-------------------------------------------------------------------------------

#Read Data:
pbp <- read_csv("C:/Users/pablo/Desktop/ITAM/Tesis/datos_de_nba/20142015.csv")
#-------------------------------------------------------------------------------

#Extract one game and order it by seconds_elapsed
game <- pbp %>% 
  filter(game_id == 21400016) %>% 
  arrange(seconds_elapsed)

#Calculate home and away score for each play
game <- game %>% 
  mutate(newptsHome = cumsum(home_plus),newptsAway = cumsum(away_plus)) 

#Stats for home team
lineup_stats_home <- game %>% 
  group_by(home_team_abbrev,
           home_player_1,home_player_2,home_player_3,home_player_4,home_player_5) %>% 
  summarise(initialScoreTeam = newptsHome[row_number() == min(row_number())],
            initialScoreOpp = newptsAway[row_number() == min(row_number())],
            finalScoreTeam = newptsHome[row_number() == max(row_number())],
            finalScoreOpp =  newptsAway[row_number() == max(row_number())],
            initialTime = seconds_elapsed[row_number() == min(row_number())],
            finalTime = seconds_elapsed[row_number() == max(row_number())],
            Events = n()) %>%
  mutate(totalScoreTeam = finalScoreTeam - initialScoreTeam,
         totalScoreOpp = finalScoreOpp - initialScoreOpp,
         Delta = totalScoreTeam - totalScoreOpp,
         totalTime = finalTime - initialTime) %>% 
  arrange(desc(totalTime)) %>% 
  ungroup()


#Stats for away team
lineup_stats_away <- game %>% 
  group_by(away_team_abbrev,
           away_player_1,away_player_2,away_player_3,away_player_4,away_player_5,) %>% 
  summarise(initialScoreTeam = newptsAway[row_number() == min(row_number())],
            initialScoreOpp = newptsHome[row_number() == min(row_number())],
            finalScoreTeam = newptsAway[row_number() == max(row_number())],
            finalScoreOpp =  newptsHome[row_number() == max(row_number())],
            initialTime = seconds_elapsed[row_number() == min(row_number())],
            finalTime = seconds_elapsed[row_number() == max(row_number())],
            Events = n()) %>%
  mutate(totalScoreTeam = finalScoreTeam - initialScoreTeam,
         totalScoreOpp = finalScoreOpp - initialScoreOpp,
         Delta = totalScoreTeam - totalScoreOpp,
         totalTime = finalTime - initialTime) %>% 
  arrange(desc(totalTime)) %>% 
  ungroup()

