#Generate a dataframe of random legal guesses
df.rs = data.frame(matrix(ncol = 4, nrow = 0))
colnames(df.rs) <- c("gameID","X","targetNum","guess")

for (gameID in 1:10000){
  target = sample.int(100,1)
  guesses = c()
  lower = 1
  upper = 100
  X = 0
  while (TRUE){
    guess = sample.int(upper-lower+1,1)+lower-1 #generate guess
    X = X+1
    #save the guess
    df.rs[nrow(df.rs)+1,] = c(gameID,X,target,guess)
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


df.rs %>% ggplot(aes(x=guess))+
  geom_histogram(bins = 99)


guesses.random_search = df.rs
write_csv(df.rs,"data/guesses_random_search.csv")

