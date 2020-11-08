#-------------------------------------------------------------------------------
#subtitution Stochastic Model
#-------------------------------------------------------------------------------

#--------------------LIBRARIES--------------------------------------------------
library(tidyverse)
library(dplyr)
library(readr)
#-------------------------------------------------------------------------------

#Read data:
subs <- read_csv("https://github.com/pablolopez2733/Thesis/blob/main/Data/subs_1415.csv?raw=true")
names(subs)[names(subs) == 'slugTeamPlayer'] <- 'Team'


#Returns a list
subs_by_team <- subs %>% 
  group_by(Team)
subs_by_team <- group_split(subs_by_team)

#Let´s just play with DAllas----------------------------------------------------
dallas <- as.data.frame(subs_by_team[[7]]) #get dallas subs


#New approach, we first bind all lineups and then generate ids
all_lineups <- data.frame("lineups" = c(unique(dallas$lineupAfter),unique(dallas$lineupBefore))) 
dict <- data.frame("Units" = unique(all_lineups$lineups))
dict$id <- 1:nrow(dict)

test_join <- dallas %>% 
  left_join(dict, by = c("lineupBefore" = "Units")) %>% 
  rename(id_Before = id)

test_join <- test_join %>% 
  left_join(dict , by = c("lineupAfter" = "Units")) %>% 
  rename(id_After = id)
#-----------------------------------------------------------------------------
dallas_pm$id <- 1:nrow(dallas_pm)
dallas_subs <- subs %>% filter(Team == "DAL")

test_join <- dallas_subs %>% 
  left_join(dallas_pm, by = c("lineupBefore" = "lineup")) %>% 
  rename(id_Before = id)

test_join <- test_join %>% 
  left_join(dallas_pm, by = c("lineupAfter" = "lineup")) %>% 
  rename(id_After = id)
