###############################################################################
####################### SCRIPT FOR LOADING DATA ###############################
###############################################################################

# Regular Season==============================================================
lineup_stats <- read_csv("data/lineup_stats.csv")
grouped_pm <- read_csv("data/grouped_lineup_stats.csv")

# Playoffs ===================================================================
lineup_stats_playoffs <- read_csv("data/lineup_stats_playoffs.csv")
grouped_pm_playoffs <- read_csv("data/grouped_lineup_stats_playoffs.csv")

teams <- sort(unique(grouped_pm$slugTeam)) 

# PPP Multinomial model
# pbp <- read_csv("data/20142015.csv")

# Advanced player stats 2012-2015---------------------------------------------
# nba1213 <-  read_csv("data/adv_stats/adv_stats_1213.txt")
# nba1213$Year <- 2012
# nba1314 <-  read_csv("data/adv_stats/adv_stats_1314.txt")
# nba1314$Year <- 2013
# nba1415 <-  read_csv("data/adv_stats/adv_stats_1415.txt")
# nba1415$Year <- 2014
# 
# adv <- rbind(nba1213,nba1314,nba1415)
