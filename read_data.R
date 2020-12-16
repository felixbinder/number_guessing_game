#read in preprocessed csvs. Working directory should be the top folder of the repo
games <- read.csv("data/games_processed.csv") 
#everything we need is in games.csv, the other files are just data computed from
guesses <- read.csv("data/guesses_processed.csv")
users <- read.csv("data/users_processed.csv")
guesses.binary = read.csv("data/guesses_binary_search.csv")
guesses.random = read.csv("data/guesses_random_search.csv")
