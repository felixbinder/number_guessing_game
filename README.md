# Number guessing game project in PSYCH201
This repo holds the analysis code and data. 

The dataset is from [here](https://www.kaggle.com/sdobson46/higher-or-lower-game), with `higher-lower-data2.csv` from [here](https://github.com/samdobson/guessing-game-ml-dataset). The data is under CC BY-SA 4.0.

## Project Plan: Dataset

We plan to use a dataset from a number guessing game running on Amazon Alexa. Users play a game with the voice assistant where the game randomly chooses a number between 1 to 100 and the user tries to guess the number. After a guess, the user is told whether their guess was too low or too high. The dataset includes 380,000 guesses across 50,000 games and 14,000 players. This simple game has an optimal solution strategy, which is binary search. The dataset includes unique player IDs for all games as well as the date of the game and the outcome of the game. The dataset also allows for analysis of a player’s first game and information about whether the player returned to play more games. This dataset only includes completed games with no more than 15 guesses. The dataset excludes all guesses that Alexa did not understand or were invalid because they were not in the 1 to 100 number range. The dataset includes repeated guesses and guesses that conflict with previous hints provided by Alexa. 

Our primary research questions are:
* How well do players approximate the optimal strategy (binary search)?
* What other strategies do players use and how can we characterize them?
* How do players’ strategies change and/or improve over time?
* Does players’ success affect their likelihood to continue playing?

We plan to investigate whether players approximate a binary search strategy by using the unique player IDs from the data set. If players do not use a binary search strategy, we can use factor analysis and identify what other strategies are utilized as well as what biases are popular. We will attempt to model different strategies and predictions of players’ next guesses given a sequence. As the dataset also includes users who played the game multiple times, we will analyze how a player’s strategy changes over the number of games they play. We can examine whether the players’ success or failure in previous games influence the strategies they will implement in the following games. Furthermore, we will analyze how a player’s performance on the game impacts their likelihood to play again, specifically if players are more likely to quit after guessing the number quickly or slowly. We are also interested in investigating if there is a relationship between the number of guesses and the player's likelihood of producing a repeat guess or a guess that conflicts with prior hints. Finally, we will identify any biases that people have and hypothesize the meaning of these biases. For example, are there certain numbers that players struggle to guess and are there particular numbers that players prefer to guess? With this large dataset, we hope to better understand the optimal binary strategy and overall the human behavior in the context of game strategy.
