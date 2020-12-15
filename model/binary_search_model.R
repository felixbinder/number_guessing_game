#Generate a dataframe of binary search for all 100 possible guesses
df.bs = data.frame(matrix(ncol = 3, nrow = 0))
colnames(df.bs) <- c("X","targetNum","guess")

for (target in 1:100){
  guesses = c()
  lower = 1
  upper = 100
  X = 0
  while (TRUE){
    guess = as.integer((upper-lower)/2+lower) #generate guess (will round up if odd)
    X = X+1
    #save the guess
    df.bs[nrow(df.bs)+1,] = c(X,target,guess)
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


df.bs %>% ggplot(aes(x=guess))+
  geom_histogram(bins = 99)


guesses.binary_search = df.bs
write_csv(df.bs,"data/guesses_binary_search.csv")

