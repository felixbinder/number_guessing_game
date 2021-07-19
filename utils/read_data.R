# Read data or preprocess if necessary 

# ---- Check if process_data has been run ----

if (!file.exists("data/games_processed.csv")) {
  print("Running utils/process_data.R. This will take a couple of minutes but only needs to be done once.")
  source("utils/process_data.R")
}

# read in preprocessed csvs
games <- read.csv("data/games_processed.csv") 
guesses <- read.csv("data/guesses_processed.csv")
users <- read.csv("data/users_processed.csv")

# ---- Check if modeling files have been run ---- 

if (!file.exists("data/guesses_binary_search.csv")) {
  print("Running model/binary_search_model.R. This will take several minutes but only needs to be done once.")
  source("model/binary_search_model.R")
}

if (!file.exists("data/guesses_random_search.csv")) {
  print("Running model/random_search_model.R. This will take several minutes but only needs to be done once.")
  source("model/random_search_model.R")
}

guesses.binary = read.csv("data/guesses_binary_search.csv")
guesses.random = read.csv("data/guesses_random_search.csv")
