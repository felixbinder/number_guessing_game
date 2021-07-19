## ----setup, include=FALSE---------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ---------------------------------------------------------------------------------------------------
# --- Setup --- #

# Load libraries
library(tidyverse)
library(RColorBrewer)
library(knitr)

# Load data
games <- read.csv("data/games.csv")

# Cast startTime and endTime to POSIX
games <- games %>%
  mutate(
    startTime = as.POSIXct(startTime),
    endTime = as.POSIXct(startTime)
  )

## ---------------------------------------------------------------------------------------------------

guesses <- games %>%
  # Pivot longer on guess columns
  pivot_longer(cols = starts_with("guess"), names_to = "attemptNum", values_to = "guess",
               names_pattern = "guess([[:digit:]]+)") %>%
  
  # Filter NAs
  filter(
    !is.na(guess)
  ) %>%
  # Remove irrelevant vars
  select(gameId, user, targetNum, attemptNum, guess) %>%
  
  # Convert attemptNum to numeric
  mutate(
    attemptNum = as.numeric(attemptNum),
    guess = as.numeric(guess)
  )



## ---------------------------------------------------------------------------------------------------

source("utils/get_EIG.R")

guesses <- guesses %>%
  group_by(gameId) %>%
  mutate(guessId = paste(gameId, attemptNum, sep = "_")) %>%
  arrange(gameId, attemptNum) %>%
  mutate(
    
    # Check if guess is too high, low, or correct
    response = case_when(
      guess > targetNum ~ "lower",
      guess < targetNum ~ "higher",
      guess == targetNum ~ "correct",
    ),
    
    # Initialise lowerBound
    lowerBound = 1,
    
    # add lower and upper bound info
    lowerBound = case_when(
      row_number() == 1 ~ 1,
      lag(response) == "lower" ~ lag(lowerBound, order_by=attemptNum),
      lag(response) == "higher" ~ lag(guess, order_by=attemptNum) + 1
    ),
    
    lowerBound = cummax(lowerBound),
    
    # Initialise upperBound
    upperBound = 100,
    
    # add lower and upper bound info
    upperBound = cummin(case_when(
      row_number() == 1 ~ 100,
      lag(response) == "higher" ~ lag(upperBound, order_by=attemptNum),
      lag(response) == "lower" ~ lag(guess, order_by=attemptNum) - 1
    )),
    
    numAvailable = (upperBound - lowerBound) + 1,
    prob = 1 / numAvailable,
    
    # Get entropy
    entropy = -1 * prob * log2(prob) * numAvailable,
    
    # info gain
    infoGain = entropy - lead(entropy, default = 0),
    
    # Find optimal guess
    optimalGuess = floor((upperBound + lowerBound) / 2),
    
    # check if guess is optimal
    isOptimal = as.numeric((abs(guess - ((upperBound + lowerBound) / 2))) < 1),
    
    # check if guess is out of bounds
    outOfBounds = as.numeric(guess < lowerBound | guess > upperBound),
    
    # find no excluded
    numExcluded = numAvailable - lead(numAvailable, default = 1),
    propExcluded = numExcluded / numAvailable,
    
    # ----- Expected IG ---- #
    EIG = get_EIG(lowerBound, upperBound, guess),
    EIG.optimal = get_EIG(lowerBound, upperBound, optimalGuess),
    EIG.relative = EIG / EIG.optimal,
        
    # ----- Repeat info measures for optimal guess ---- #
    
    # Optimal response
    # Check if guess is too high, low, or correct
    
    # Optimal lower, upper, and numAvailable
    numAvailable.optimal = case_when(
      optimalGuess > targetNum ~ (optimalGuess - lowerBound) + 1,
      optimalGuess < targetNum ~ (upperBound - optimalGuess),
      optimalGuess == targetNum ~ 1,
    ),
    
    # Optimal probability of correct guess, entropy, info gain
    prob.optimal = 1 / numAvailable.optimal,
    entropy.optimal = -1 * prob.optimal * log2(prob.optimal) * numAvailable.optimal,
    infoGain.optimal = entropy - entropy.optimal,
    
    # optimal excluded
    numExcluded.optimal = numAvailable - numAvailable.optimal,
    propExcluded.optimal = numExcluded.optimal / numAvailable,
    
    # comparison to optimal measures
    infoGain.norm = infoGain / infoGain.optimal,
    propExcluded.norm = propExcluded / propExcluded.optimal,
    
    # Add a thousandth so we can scale plots more readably
    mili = 1 / 1000,
  ) %>%
  ungroup()




## ---------------------------------------------------------------------------------------------------
outofbounds_exclusions <- guesses %>%
  group_by(gameId) %>%
  filter(outOfBounds == 1) %>% 
  select(gameId)

guesses<-guesses[!guesses$gameId %in% outofbounds_exclusions$gameId,]

# Remove games as well
games<-games[!games$gameId %in% outofbounds_exclusions$gameId,]



## ---------------------------------------------------------------------------------------------------

guesses <- guesses %>%
  filter(numAvailable > 1)



## ---------------------------------------------------------------------------------------------------

cor(guesses$infoGain, guesses$propExcluded)

guesses %>%
  select(numAvailable, guess, optimalGuess, entropy, infoGain, EIG) %>%
  filter(guess == optimalGuess)


## ---------------------------------------------------------------------------------------------------

# Group guesses by gameId and calculate aggregate stats
games.info <- guesses %>%
  group_by(gameId) %>%
  summarise(
    infoGain.mean = mean(infoGain),
    EIG = mean(EIG),
    EIG.relative = mean(EIG.relative),
    propExcluded.mean = mean(propExcluded),
    numOptimal = sum(isOptimal),
    propOptimal = numOptimal / n(),
    .groups = "drop"
  )

# Merge games.info back into games
games <- merge(games, games.info) 

# Add game_index information
games <- games %>%
  group_by(user) %>%
  mutate(
    game_index = dense_rank(startTime),
    no_user_games = n()
  ) %>%
  ungroup()

# Merge game & user info onto guesses
guesses <- merge(guesses,
  games %>%
    select(gameId, game_index, no_user_games))



## ---------------------------------------------------------------------------------------------------

# Create a summary df grouped by user
users <- games %>% 
  group_by(user) %>%
  summarise(
    # Number of games played
    no_games=n(),
    # Date of first game
    first_game=min(startTime),
    # Date of last game
    last_game=max(startTime),
    # Mean duration of games
    duration.mean=mean(duration),
    # Mean no_guesses
    guesses.mean=mean(numGuesses),
    guesses.total = sum(numGuesses),
    .groups = "drop"
  ) %>%
  mutate(
    # bin no_games into roughly even chunks
    no_games_bin=cut(
      no_games,
      breaks = c(0, 1, 2, 5, 10, 200),
      labels = c("1", "2-4", "5-9", "10-99", "100+"),
      ),
    # find the time between first and last games
    career_length = difftime(last_game, first_game, units="hours"),
    career_length_bin = factor(
      case_when(
        no_games < 2 ~ "Single game",
        career_length < (5 / 60) ~ "< 5 min",
        career_length < 1 ~ "5-60 min",
        career_length < 24 ~ "1-24 hr",
        career_length < 168 ~ "1-7 days",
        career_length < 720 ~ "7 - 30 days",
        career_length >= 720 ~ "30+ days"),
    levels = c("Single game", "< 5 min", "5-60 min", "1-24 hr", "1-7 days", "7 - 30 days", "30+ days")
    )
  )


users <- merge(
  users,
  guesses %>%
    group_by(user) %>%
    summarise(
      infoGain.mean = mean(infoGain),
      EIG = mean(EIG),
      EIG.relative = mean(EIG.relative),
      propExcluded.mean = mean(propExcluded),
      numOptimal = sum(isOptimal),
      propOptimal = numOptimal / n(),
      .groups = "drop"
    )
)



## ---------------------------------------------------------------------------------------------------

write.csv(guesses, "data/guesses_processed.csv")
write.csv(games, "data/games_processed.csv")
write.csv(users, "data/users_processed.csv")


