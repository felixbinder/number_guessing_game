guesses %>%
  ggplot(aes(attemptNum, infoGain)) +
  stat_summary_bin(fun.data="mean_cl_boot",
                   geom="pointrange",
                   binwidth = 1,
                   color=color2) +
  geom_smooth(method = 'lm') + 
  theme_minimal() 
  # labs(
  #   title="Mean information gain per guess for n-th game of user",
  #   x = "No. previous games played by user",
  #   y = "Mean information gain"
  # )


guesses %>%
  ggplot(aes(attemptNum, entropy)) +
  stat_summary_bin(fun.data="mean_cl_boot",
                   geom="pointrange",
                   binwidth = 1,
                   color=color2) +
  geom_smooth(method = 'lm') + 
  theme_minimal() 


guesses %>%
  ggplot(aes(attemptNum, isOptimal)) +
  stat_summary_bin(fun.data="mean_cl_boot",
                   geom="pointrange",
                   binwidth = 1,
                   color=color2) +
  geom_smooth(method = 'lm') + 
  theme_minimal() +
  labs(
    title="Proportion of optimal guesses for the n-th guess",
    x = "N-th guess",
    y="Proportion of optimal guesses"
  )

guesses %>%
  ggplot(aes(attemptNum, isOptimal-1/(upperBound-lowerBound))) +
  stat_summary_bin(fun.data="mean_cl_boot",
                   geom="pointrange",
                   binwidth = 1,
                   color=color2) +
  geom_smooth(method = 'lm') + 
  theme_minimal() +
  labs(
    title="Proportion of optimal guesses for the n-th guess beyond chance",
    x = "N-th guess",
    y="Proportion of optimal guesses - chance of optimal guess"
  )

guesses %>%
  ggplot(aes(attemptNum, (upperBound-lowerBound))) +
  stat_summary_bin(fun.data="mean_cl_boot",
                   geom="pointrange",
                   binwidth = 1,
                   color=color2) +
  geom_smooth(method = 'lm') + 
  theme_minimal() 
