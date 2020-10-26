#-------------------------------------
#Data Prep for Markov Model
#reference: https://nbainrstats.netlify.app/post/adding-lineups-to-nba-play-by-play-data/
#------------------------------------


#Libraries----------------------------------------------------------------------
library(readr)
library(dplyr)
#-------------------------------------------------------------------------------

#Read Data:
pbp <- read_csv("C:/Users/pablo/Desktop/ITAM/Tesis/datos_de_nba/20142015.csv")
#-------------------------------------------------------------------------------

lineups_quarters <- pbp %>% 
  arrange(game_id,period)

lineup_subs <- pbp %>% 
  filter(eventmsgtype == 8) %>% 
  select(game_id,period,seconds_elapsed, player1_team_abbreviation,
         playerOut = player1_name, playerIn = player2_name, eventnum) %>% 
  arrange(game_id, eventnum) %>%
  group_by(game_id, period, player1_team_abbreviation) %>%
  mutate(row1 = row_number()) %>%
  ungroup() %>%
  left_join(lineups_quarters %>%
              group_by(game_id, period, player1_team_abbreviation) %>%
              summarise(lineupBefore = paste(sort(unique(player1_name)), collapse = ", ")) %>%
              ungroup() %>%
              mutate(row1 = 1)) %>%
  select(-row1)

  