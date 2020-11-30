setwd("/Users/rkaufman/Documents/Github/number_guessing_game_PSYCH_201/data") #for Rob
suppressMessages(library(tidyverse))
library(nFactors)
repeat_player_prediction <- read.csv('repeat-player-prediction.csv')
games <- read.csv('games.csv')
guesses_processed <- read.csv('guesses_processed.csv')

#'What's up with out of bounds guesses? When are they most likely to occur? 
#'Are there any trends that precit when a guess will be out of bounds? (Rob)
#'1. Find out if there are any incomplete games in the dataset (when the final response for a 
#'game =! the target #)
#'2. Are there any factors that predict when an OOB guess will occur? 
#'A. # of guesses
#'B. Game/guess speed
#'C. From certain types of players
#'3. Why do we think these are occurring?
#'A. Forgetting?
#'B. Poor players?