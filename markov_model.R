library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)
library(fastDummies)
library(ash)
library(markovchain)




##########################
# Define team
#########################
team <- "CLE"
team_subs <- lineup_stats %>% filter(slugTeam == team)
team_subs <- team_subs %>% left_join(grouped_pm, by = "lineup")
team_subs <- team_subs[order(team_subs$idGame,team_subs$lineupStint),]
subcount <- nrow(team_subs)
subs <- round(subcount/82) 

tops <- grouped_pm %>%
  filter(slugTeam == team) %>% 
  head(sort(grouped_pm$seasonSecs,decreasing=TRUE), n = subs)

top_subs <- team_subs %>% 
  filter(l_id %in% tops$l_id)
  
# Get events
substitutions <- top_subs$l_id

##########################################################
# Set Markov Model
#########################################################
seq_matrix<-createSequenceMatrix(substitutions,sanitize=T)
mc_mle<-markovchainFit(data=substitutions)
mc_boot<-markovchainFit(data=substitutions,method="bootstrap",nboot=5, name="Bootstrap Mc")

season_matrix_mle <- mc_mle$estimate@transitionMatrix
season_matrix_boot <- mc_boot$estimate@transitionMatrix

states <- levels(as.factor(unique(top_subs$l_id))) 

##############################################################################
# Simulation Functions
############################################################################

#simulate a game
sim_game <- function()
{
  X <- as.character(top_subs[which.max(top_subs$Season_mins),]$l_id) #por la pi cero dada sabemos que iniciamos en una semana sin ataques
  M <- season_matrix_mle
  for(i in 2:subs)X[i] <- sample(states, 1, prob = M[X[i-1],])
  return(X)
}

mc_game <- function(n)
{
  results <- as.data.frame(t(as.matrix(sim_game())))
  for (i in 2:n) {
    results <- rbind(results,as.data.frame(t(as.matrix(sim_game()))))
  }
  return(results)
}


##########################################################
# Playing Times
##########################################################
team <- "CLE"
ptimes <- lineup_stats %>%
  filter(slugTeam == team) %>% 
  group_by(lineup,idGame) %>%
  summarise(
    time = sum(totalTime),
    pm = sum(netScoreTeam)
    )

# average playing times
rates <- ptimes %>% 
  group_by(lineup) %>% 
  summarise(t = mean(time))

s_exps <- rexp(1, rates[which(rates$lineup)])

# Check fits
xax <- seq(min(ptimes$time), max(ptimes$time), length=1000)
hist(ptimes$time, freq=F, breaks=50)
lines(density(ptimes$time), col="red")
lines(dnorm(xax,
            mean = mean(ptimes$time),
            sd = sd(ptimes$time)),col="blue",lty = 1, lwd = 2)
lines(dexp(xax,rate = 1/mean(ptimes$time),F),col="green",lty = 1, lwd = 2)

