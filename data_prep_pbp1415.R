#-------------------------------------
#SCRIPT FOR ADDING A COLUMN TO PBP DATA IN WHICH WE HAVE THE LINEUP FOR THE HOME AND AWAY TEAM
#FOR EVERY PLAY IN THE SEASON

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

#Extract data from nbastatR into a csv and then push to Github:
games <- seasons_schedule(seasons = c(2015))
play_logs_all <- play_by_play_v2(game_ids = unique(schedule_1415$idGame))
write.csv(play_logs_all,
          "C:/Users/pablo/Desktop/ITAM/Tesis/Thesis/Data/pbp_1415.csv",
          row.names = FALSE)
#-------------------------------------------------------------------------------


#Read Data:
play_logs_all <- read_csv("https://github.com/pablolopez2733/Thesis/blob/main/Data/pbp_1415.csv?raw=true")

#manipulate so that we have home and away team for each game:
games <- games %>%
  separate(slugMatchup, c('slugTeamAway', 'slugTeamHome'), sep = ' @ | vs. ')



#Data cleansing:
new_pbp <- play_logs_all %>%
  distinct(idGame, numberEvent, .keep_all = TRUE) %>%   # remove duplicate events
  group_by(idGame) %>%
  mutate(numberEvent = row_number()) %>%  # new numberEvent column with events in the right order
  ungroup() %>%
  select(idGame, numberEventMessageType, numberEventActionType, namePlayer1, namePlayer2, namePlayer3,                   
         slugTeamPlayer1, slugTeamPlayer2,  slugTeamPlayer3, numberPeriod, timeQuarter, minuteRemainingQuarter,          
         secondsRemainingQuarter, descriptionPlayHome, numberEvent, descriptionPlayVisitor, scoreHome, scoreAway) %>%
  mutate(shotPtsHome = case_when(
    numberEventMessageType == 3 & !str_detect(descriptionPlayHome, "MISS") ~ 1,                               
    numberEventMessageType == 1 & str_detect(descriptionPlayHome, "3PT") ~ 3,                                 
    numberEventMessageType == 1 & !str_detect(descriptionPlayHome, "3PT") ~ 2,
    TRUE ~ 0
  )) %>%
  mutate(shotPtsAway = case_when(
    numberEventMessageType == 3 & !str_detect(descriptionPlayVisitor, "MISS") ~ 1,
    numberEventMessageType == 1 & str_detect(descriptionPlayVisitor, "3PT") ~ 3,
    numberEventMessageType == 1 & !str_detect(descriptionPlayVisitor, "3PT") ~ 2,
    TRUE ~ 0
  )) %>%
  group_by(idGame) %>%
  mutate(ptsHome = cumsum(shotPtsHome),
         ptsAway = cumsum(shotPtsAway)) %>%
  ungroup() %>%
  mutate(secsLeftQuarter = (minuteRemainingQuarter * 60) + secondsRemainingQuarter) %>% 
  mutate(secsStartQuarter = case_when(                                                                     
    numberPeriod %in% c(1:5) ~ (numberPeriod - 1) * 720,
    TRUE ~ 2880 + (numberPeriod - 5) * 300
  )) %>%
  mutate(secsPassedQuarter = ifelse(numberPeriod %in% c(1:4), 720 - secsLeftQuarter, 300 - secsLeftQuarter), 
         secsPassedGame = secsPassedQuarter + secsStartQuarter) %>%
  left_join(games %>%
              select(idGame, slugTeamHome, slugTeamAway)) %>%
  select(idGame, numberEventMessageType, numberEventActionType, slugTeamHome, slugTeamAway, slugTeamPlayer1, slugTeamPlayer2, 
         slugTeamPlayer3, numberPeriod, timeQuarter, secsPassedGame, numberEvent, namePlayer1, namePlayer2, namePlayer3, 
         descriptionPlayHome, descriptionPlayVisitor, ptsHome, ptsAway, shotPtsHome, shotPtsAway) %>%
  mutate(marginBeforeHome = ptsHome - ptsAway - shotPtsHome + shotPtsAway,
         marginBeforeAway = ptsAway - ptsHome - shotPtsAway + shotPtsHome,
         timeQuarter = str_pad(timeQuarter, width = 5, pad = 0))


#Subs py period:
subs_made <- new_pbp %>%
  filter(numberEventMessageType == 8) %>%        # Note 6
  mutate(slugTeamLocation = ifelse(slugTeamPlayer1 == slugTeamHome, "Home", "Away")) %>%
  select(idGame, numberPeriod, timeQuarter, secsPassedGame, slugTeamPlayer = slugTeamPlayer1,
         slugTeamLocation, playerOut = namePlayer1, playerIn = namePlayer2) %>%
  pivot_longer(cols = starts_with("player"),
               names_to = "inOut",
               names_prefix = "player",
               values_to = "namePlayer") %>%
  group_by(idGame, numberPeriod, slugTeamPlayer, namePlayer) %>%
  filter(row_number() == 1) %>%
  ungroup()

#Check for players who played all quarters and technical fouls:
others_qtr <- new_pbp %>%
  filter(numberEventMessageType != 8) %>%                             
  filter(!(numberEventMessageType == 6 & numberEventActionType %in% c(10, 11, 16, 18, 25))) %>% 
  pivot_longer(cols = starts_with("namePlayer"),
               names_to = "playerNumber",
               names_prefix = "namePlayer",
               values_to = "namePlayer") %>%
  mutate(slugTeamPlayer = case_when(playerNumber == 1 ~ slugTeamPlayer1,
                                    playerNumber == 2 ~ slugTeamPlayer2,
                                    playerNumber == 3 ~ slugTeamPlayer3,
                                    TRUE ~ "None")) %>%
  mutate(slugTeamLocation = ifelse(slugTeamPlayer == slugTeamHome, "Home", "Away")) %>%
  filter(!is.na(namePlayer),
         !is.na(slugTeamPlayer)) %>%
  anti_join(subs_made %>%
              select(idGame, numberPeriod, slugTeamPlayer, namePlayer)) %>%    # remove players that were subbed in the quarter
  distinct(idGame, numberPeriod, namePlayer, slugTeamPlayer, slugTeamLocation)

lineups_quarters <- subs_made %>%
  filter(inOut == "Out") %>%
  select(idGame, numberPeriod, slugTeamPlayer, namePlayer, slugTeamLocation) %>%
  bind_rows(others_qtr) %>%
  arrange(idGame, numberPeriod, slugTeamPlayer)


#-----NOW WE CREATE A COLUMN WITH THE FULL LINEUP IN EVERY PLAY---------------
lineup_subs <- new_pbp %>%
  filter(numberEventMessageType == 8) %>%
  select(idGame, numberPeriod, timeQuarter, secsPassedGame, slugTeamPlayer = slugTeamPlayer1, playerOut = namePlayer1, 
         playerIn = namePlayer2, numberEvent) %>%
  arrange(idGame, numberEvent) %>%
  group_by(idGame, numberPeriod, slugTeamPlayer) %>%
  mutate(row1 = row_number()) %>%
  ungroup() %>%
  left_join(lineups_quarters %>%
              group_by(idGame, numberPeriod, slugTeamPlayer) %>%
              summarise(lineupBefore = paste(sort(unique(namePlayer)), collapse = ", ")) %>%
              ungroup() %>%
              mutate(row1 = 1)) %>%
  select(-row1)

lineup_subs <- lineup_subs %>%
  mutate(lineupBefore = str_split(lineupBefore, ", ")) %>% 
  arrange(idGame, numberEvent) %>%
  group_by(idGame, numberPeriod, slugTeamPlayer) %>%
  mutate(lineupAfter = accumulate2(playerIn, playerOut, ~setdiff(c(..1, ..2), ..3), .init = lineupBefore[[1]])[-1],
         lineupBefore = coalesce(lineupBefore, lag(lineupAfter))) %>%
  ungroup() %>% 
  mutate_all(~map_chr(., ~paste(.x, collapse = ", "))) %>%
  mutate_at(vars("numberEvent", "numberPeriod", "idGame"), ~ as.integer(.)) %>%
  mutate(secsPassedGame = as.numeric(secsPassedGame)) %>%
  arrange(idGame, numberEvent) %>%
  left_join(lineups_quarters %>%
              distinct(idGame, slugTeamPlayer, slugTeamLocation)) %>%
  filter(!is.na(slugTeamLocation))


#We now have all the substitutions we need with the lineups before and after they took place
#We need to join it to our pbp data first by joining by starting lineup and then lineups after sub
lineup_game <- new_pbp %>%
  group_by(idGame, numberPeriod) %>%
  mutate(row1 = row_number()) %>%
  ungroup() %>%
  left_join(lineups_quarters %>%
              group_by(idGame, numberPeriod, slugTeamLocation) %>%
              summarise(lineupBefore = paste(sort(unique(namePlayer)), collapse = ", ")) %>%
              ungroup() %>%
              pivot_wider(names_from = slugTeamLocation,
                          names_prefix = "lineupInitial",
                          values_from = lineupBefore) %>%
              mutate(row1 = 1)) %>%
  select(-row1) %>%
  left_join(lineup_subs %>%
              mutate(lineupBeforeHome = ifelse(slugTeamLocation == "Home", lineupBefore, NA),
                     lineupAfterHome = ifelse(slugTeamLocation == "Home", lineupAfter, NA),
                     lineupBeforeAway = ifelse(slugTeamLocation == "Away", lineupBefore, NA),
                     lineupAfterAway = ifelse(slugTeamLocation == "Away", lineupAfter, NA)) %>%
              select(idGame, numberPeriod, timeQuarter, secsPassedGame, numberEvent, slugTeamPlayer1 = slugTeamPlayer,
                     contains("Home"), contains("Away"))) %>%
  mutate_at(vars(c(lineupBeforeHome, lineupAfterHome)), ~ ifelse(!is.na(lineupInitialHome), lineupInitialHome, .)) %>%
  mutate_at(vars(c(lineupBeforeAway, lineupAfterAway)), ~ ifelse(!is.na(lineupInitialAway), lineupInitialAway, .)) %>%
  select(-starts_with("lineupInitial"))

#Lineups between events:
lineup_game <- lineup_game %>%
  group_by(idGame, numberPeriod) %>%
  mutate(lineupHome = na.locf(lineupAfterHome, na.rm = FALSE),
         lineupAway = na.locf(lineupAfterAway, na.rm = FALSE),
         lineupHome = ifelse(is.na(lineupHome), na.locf(lineupBeforeHome, fromLast = TRUE, na.rm = FALSE), lineupHome),
         lineupAway = ifelse(is.na(lineupAway), na.locf(lineupBeforeAway, fromLast = TRUE, na.rm = FALSE), lineupAway),
         lineupHome = str_split(lineupHome, ", "),
         lineupAway = str_split(lineupAway, ", "),
         lineupHome = map_chr(lineupHome, ~ paste(sort(.), collapse = ", ")),
         lineupAway = map_chr(lineupAway, ~ paste(sort(.), collapse = ", "))) %>%
  ungroup() %>%
  select(-c(starts_with("lineupBefore"), starts_with("lineupAfter")))

#There it is, we now have lineups for every player in every play !!!


#-------------------------------------------------------------------------------
#Now for simplicity we will export it to a csv and start another script:
write.csv(lineup_game,
          "C:/Users/pablo/Desktop/ITAM/Tesis/Thesis/Data/lineup_pbp_1415.csv",
          row.names = FALSE)
#pushed to github:
#https://github.com/pablolopez2733/Thesis/blob/main/Data/lineup_pbp_1415.csv