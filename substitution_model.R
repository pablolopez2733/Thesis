#-------------------------------------------------------------------------------
#subtitution Stochastic Model
#-------------------------------------------------------------------------------

#--------------------LIBRARIES--------------------------------------------------
library(tidyverse)
library(dplyr)
library(readr)
#-------------------------------------------------------------------------------

#Read data:
lineup_pm <- read_csv("Data/lineup_stats.csv")

foo <- lineup_pm %>%
  group_by(lineup,slugTeam) %>%
  summarise(seasonPM = sum(netScoreTeam),
            seasonSecs = sum(totalTime)) %>%
  ungroup() %>%
  arrange(-seasonPM) %>%
  mutate(seasonMin = paste0(floor(seasonSecs / 60), ":", str_pad(round(seasonSecs %% 60, 0), side = "left", width = 2, pad = 0))) %>%
  select(seasonSecs,seasonPM,slugTeam,lineup)

prueba2018<- lineup_stats %>%
  group_by(lineup) %>%
  summarise(seasonPM = sum(netScoreTeam),
            seasonSecs = sum(totalTime)) %>%
  ungroup() %>%
  arrange(-seasonPM) %>%
  mutate(seasonMin = paste0(floor(seasonSecs / 60), ":", str_pad(round(seasonSecs %% 60, 0), side = "left", width = 2, pad = 0))) %>%
  select(-seasonSecs)