#-------------------------------------------------------------------------------
#CALCULATE LINEUPS PLUS MINUS FOR EVERY TEAM IN EVERY GAME
#-------------------------------------------------------------------------------

#libraries:---------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(zoo)
library(nbastatR)
library(future)
#-------------------------------------------------------------------------------

#Read Data

lineup_game <- read_csv("Data/lineup_pbp_1415.csv",col_types = cols(timeQuarter = "c"))

lineup_game_stats <- lineup_game %>%
  mutate(canSub = case_when(numberEventMessageType == 5 & !numberEventActionType %in% c(1, 2) ~ 1,    # dead ball turnovers
                            numberEventMessageType == 6 & numberEventActionType != 16 ~ 1,            # fouls
                            numberEventMessageType == 11 & numberEventActionType != 4 ~ 1,
                            numberEventMessageType == 7 & numberEventActionType == 5 ~ 1,             # kickballs
                            numberEventMessageType == 4 & numberEventActionType == 0 & !str_detect(str_to_upper(descriptionPlayHome), "OFF:") ~ 1,
                            numberEventMessageType == 4 & numberEventActionType == 0 & !str_detect(str_to_upper(descriptionPlayVisitor), "OFF:") ~ 1,
                            TRUE ~ 0)) %>%
  mutate(secsPassedGame2 = ifelse(timeQuarter == "12:00" &
                                    (str_detect(str_to_lower(descriptionPlayHome), "technical") |
                                       str_detect(str_to_lower(descriptionPlayVisitor), "technical")),
                                  secsPassedGame + 0.5, secsPassedGame)) %>%    # Note 4
  group_by(idGame, numberPeriod, secsPassedGame) %>%
  mutate(numberNew = ifelse(numberEventMessageType == 3 & numberEventActionType == 12, 
                            paste(numberEvent[numberEventMessageType == 3 & numberEventActionType == 11], collapse = ", "), 
                            as.character(numberEvent)),
         numberNew = ifelse(numberEventMessageType == 3 & numberEventActionType %in% c(14, 15), 
                            paste(numberEvent[numberEventMessageType == 3 & numberEventActionType == 13], collapse = ", "),
                            numberNew)) %>%
  mutate(numberNew = str_split(numberNew, ", "),
         numberNew = map(numberNew, ~as.numeric(.)),
         numberNew = map2_dbl(numberNew, numberEvent, ~ max(.x[.x <= .y]))) %>%
  ungroup() %>%
  arrange(idGame, numberNew, numberEvent) %>%
  group_by(idGame) %>%
  mutate(newptsHome = cumsum(shotPtsHome),
         newptsAway = cumsum(shotPtsAway)) %>%
  group_by(idGame, numberPeriod, secsPassedGame2) %>%
  mutate(subOpp = cumsum(canSub)) %>%
  group_by(idGame = as.character(idGame), numberPeriod = as.character(numberPeriod), subOpp, secsPassedGame2 = as.character(secsPassedGame2)) %>%
  mutate(hasFouls = sum(numberEventMessageType == 3)) %>%
  mutate(newptsHome = ifelse(hasFouls > 0,
                             newptsHome[row_number() == max(row_number()[numberEventMessageType == 3])],
                             newptsHome),
         newptsAway = ifelse(hasFouls > 0,
                             newptsAway[row_number() == max(row_number()[numberEventMessageType == 3])],
                             newptsAway)) %>%
  ungroup() %>%
  select(-hasFouls) %>%
  select(-c(numberNew, secsPassedGame2)) %>%
  mutate_all(~ as.character(.)) %>%
  mutate(secsPassedGame = as.numeric(secsPassedGame),
         numberEvent = as.numeric(numberEvent))


#Turn table into long format:
lineup_stats <- lineup_game_stats %>%
  select(idGame, numberEvent, slugTeamHome, slugTeamAway, numberPeriod, timeQuarter, secsPassedGame, 
         newptsHome, newptsAway, lineupHome, lineupAway) %>%
  pivot_longer(cols = starts_with("lineup"),
               names_to = "lineupLocation",
               names_prefix = "lineup",
               values_to = "lineup") %>%
  mutate(ptsTeam = ifelse(lineupLocation == "Home", newptsHome, newptsAway),
         ptsOpp = ifelse(lineupLocation == "Away", newptsHome, newptsAway),
         slugTeam = ifelse(lineupLocation == "Home", slugTeamHome, slugTeamAway),
         slugOpp = ifelse(lineupLocation == "Away", slugTeamHome, slugTeamAway)) %>%
  distinct(idGame, slugTeam, slugOpp, numberPeriod, timeQuarter, secsPassedGame, ptsTeam, ptsOpp, lineup, 
           teamLocation = lineupLocation, numberEvent) %>%
  arrange(idGame, numberEvent)

#Id every lineup change and create column for the number of each stint:
lineup_stats <- lineup_stats %>%
  group_by(idGame, slugTeam) %>%
  mutate(lineupChange = lineup != lag(lineup),
         lineupChange = coalesce(lineupChange, FALSE)) %>%
  group_by(idGame, slugTeam) %>%
  mutate(lineupStint = cumsum(lineupChange)) %>%
  ungroup() %>%
  arrange(idGame, lineupStint, numberEvent) %>%
  group_by(idGame, slugTeam, lineup, lineupStint) %>%
  summarise(initialScoreTeam = ptsTeam[row_number() == min(row_number())],
            initialScoreOpp = ptsOpp[row_number() == min(row_number())],
            finalScoreTeam = ptsTeam[row_number() == max(row_number())],
            finalScoreOpp =  ptsOpp[row_number() == max(row_number())],
            initialTime = secsPassedGame[row_number() == min(row_number())],
            finalTime = secsPassedGame[row_number() == max(row_number())]) %>%
  ungroup() %>%
  arrange(idGame, lineupStint) %>%
  group_by(idGame, slugTeam) %>%                              
  mutate(finalTime = ifelse(row_number() == max(row_number()), finalTime, lead(initialTime))) %>%  
  ungroup()


#calculate the net points and total time for each lineup in seconds:
lineup_stats <- lineup_stats %>%
  mutate(across(c(contains("Score")), ~ as.numeric(.), .names = "{col}")) %>%
  mutate(totalScoreTeam = finalScoreTeam - initialScoreTeam,
         totalScoreOpp = finalScoreOpp - initialScoreOpp,
         netScoreTeam = totalScoreTeam - totalScoreOpp,
         totalTime = finalTime - initialTime) %>%
  arrange(idGame, lineupStint)


#------------------------------------------------------------------------------
#It works! Let´s write it as csv and push it to Github:
write.csv(lineup_stats,
          "C:/Users/pablo/Desktop/ITAM/Tesis/Thesis/Data/lineup_stats.csv",
          row.names = FALSE)
#Pushed to github:
#https://github.com/pablolopez2733/Thesis/blob/main/Data/lineup_stats.csv