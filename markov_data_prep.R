library(tidyverse)
library(lubridate)
library(dplyr)

df <- read.csv("C:/Users/pablo/Desktop/ITAM/Tesis/datos_de_nba/20142015.csv")

# Add 0s and 1s
pbp <- df %>% 
  mutate(
    fg_attempted = ifelse(fg_attempted == "f",0,1),
    ft_attempted = ifelse(ft_attempted == "f",0,1),
    fg_made = ifelse(fg_made == "f",0,1),
    tp_made = ifelse(tp_made == "f",0,1),
    ft_made = ifelse(ft_made == "f",0,1),
    is_assist = ifelse(is_assist == "f",0,1)
    
  )

# order it by game
pbp_series <- pbp %>% 
  arrange(game_id,period,desc(seconds_elapsed)) %>% 
  select()

