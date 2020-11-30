suppressMessages(library(tidyverse))
library(nFactors)
repeat_player_prediction <- read.csv('repeat-player-prediction.csv')
games <- read.csv('games.csv')
guesses <- read.csv('guesses.csv')

#What other strategies do players use and how can we characterize them? (Naomi, Felix)
#Factor analysis of strategies for optimal strategy
#build intuitions by printing out random non-optimal games

#https://www.statmethods.net/advstats/factor.html

#Average distance to last guess- each last guess is different, group by number of guesses first
#average distance of correct answer to the last guess?
games %>% ggplot(aes(x=numGuesses))+
  geom_histogram(binwidth = 1)
summary(games$numGuesses)
group_by(games)

# Information gain (not sure what this mean)

#Potential strategies: Using previous result as a prior
#binary search rounded to 10/5’s/evens/odds
#Sequential
#Random



#format
fit<- factanal(games$guess1:games$guess15, 3, rotation= "varimax")
print(fit, digits = 2, cutoff= .3, sort= TRUE)
load<- fit$loadings[,1:2]
plot(load, type= "n")