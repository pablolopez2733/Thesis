#-------------------------------------------------------------------------------
#subtitution Stochastic Model
#-------------------------------------------------------------------------------

#--------------------LIBRARIES--------------------------------------------------
library(tidyverse)
library(dplyr)
library(readr)
#-------------------------------------------------------------------------------


#---------------------------------------------------------------------
# Calculate GSW transition matrix

lineup_stats <- read_csv("https://github.com/pablolopez2733/Thesis/blob/main/Data/lineup_stats.csv?raw=true")
gsw <- lineup_stats %>%
  filter(slugTeam == "GSW")
#check if the unique values are same as paper:
unique(gsw$lineup) #431 vs 446 OK

#Try to make a dictionary to add an id to every lineup
gsw_dict <- data.frame(unique(gsw$lineup))
gsw_dict$l_id <- 1:nrow(gsw_dict)#add ids

#now let´s left join it with the gsw df:
gsw <- gsw %>% 
  left_join(gsw_dict, by = c("lineup" = "unique.gsw.lineup.")) 

#check for na's
sum(is.na(gsw$l_id)) #No NAs :D !!!

#Calculate tranition matrix for gs:
gsw_tm <- matrix(0L, nrow = nrow(gsw_dict), ncol = nrow(gsw_dict))
colnames(gsw_tm) <- gsw_dict$l_id
rownames(gsw_tm) <- gsw_dict$l_id

#for for filling the matrix:
for (i in 2:nrow(gsw)) {
  gsw_tm[(gsw$l_id[i-1]),gsw$l_id[i]] = gsw_tm[(gsw$l_id[i-1]),gsw$l_id[i]] + 1
}
#LOOKS LIKE IT FCKIN WORKS!!!!!!!!!!!