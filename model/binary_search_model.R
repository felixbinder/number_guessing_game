#Generate a dataframe of binary search for all 100 possible guesses
source("utils/get_EIG.R")
gameID = 0
gameIDs = c()
guesses = c()
attemptNums = c()
targetNums = c()

for (target in rep(1:100,times=round(nrow(games)/100))){
  lower = 1
  upper = 100
  X = 0
  gameID = gameID + 1
  print(paste(c(gameID,'/',round(nrow(games)/100)*100)))
  while (TRUE){
    guess = as.integer((upper-lower)/2+lower) #generate guess (will round up if odd)
    X = X+1
    #save the guess
    # df.bs[i,] = c(gameID,X,target,guess)
    # i = i+1 
    gameIDs = c(gameIDs,gameID)
    guesses = c(guesses,guess)
    attemptNums = c(attemptNums,X)
    targetNums = c(targetNums,target)
    if (guess == target){ #won
      break
    }
    if (guess > target){ #lower
      upper = guess-1
    }
    if (guess < target){ #higher
      lower = guess+1
    }
  }
}

df.bs = data.frame(gameId = gameIDs,
                   attemptNum = attemptNums,
                   targetNum = targetNums,
                   guess = guesses)

guesses.binary <- df.bs %>%
  group_by(gameId) %>%
  mutate(guessId = paste(gameId, X, sep = "_")) %>%
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

write_csv(guesses.binary,"data/guesses_binary_search.csv")