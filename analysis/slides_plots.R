
# ----- EIG by participant ---- #

games.binary <- guesses.binary %>%
  group_by(gameId) %>%
  summarise(
    EIG = mean(EIG),
    EIG.relative = mean(EIG.relative)
  )
games.binary$user <- sample(games$user, nrow(games.binary), replace = T)
users.binary <- games.binary %>%
  group_by(user) %>%
  summarise(
    EIG = mean(EIG),
    EIG.relative = mean(EIG.relative)
  )

games.random <- guesses.random %>%
  group_by(gameId) %>%
  summarise(
    EIG = mean(EIG),
    EIG.relative = mean(EIG.relative)
  )
games.random$user <- sample(games$user, nrow(games.random), replace = T)
users.random <- games.random %>%
  group_by(user) %>%
  summarise(
    EIG = mean(EIG),
    EIG.relative = mean(EIG.relative)
  )

p.eig.user <- users %>%
  ggplot(aes(x = EIG)) + 
  geom_density(data = users.random,
               color=color.random, fill=color.random, alpha=0.5, size=0.5) +
  geom_density(data = users.binary,
               color=color.binary, fill=color.binary, alpha=0.5, size=0.5) +
  geom_density(color=color.human, fill = color.human, alpha=0.8, size=0.5) +
  theme_minimal() +
  labs(
    x = "Expected Information Gain",
    y = "Density"
  ) + 
  theme(
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    panel.grid = element_blank(),
    axis.text = element_text(color = "white"),
    text = element_text(color = "white", size=20)
  )

ggsave("eig_user.png", p.eig.user, bg = "transparent", width = 10, height = 5)


# ----- REIG vs Game Index ----- #

# Overall
p.reig.idx.1 <- games %>%
  filter(no_user_games > 2) %>%
  ggplot(aes(game_index, EIG.relative)) +
  stat_summary_bin(fun.data="mean_cl_boot",
                   geom="pointrange",
                   binwidth = 5,
                   color=color.human) +
  geom_smooth(method = 'lm', formula = y~x, color=color.human) + 
  geom_hline(yintercept = guesses.random %>%
               pull(EIG.relative) %>% mean(na.rm=T),
             color = color.random,
             size = 1,
             linetype = "dashed"
  )+
  geom_hline(yintercept = guesses.binary %>%
               pull(EIG.relative) %>% mean(na.rm=T),
             color = color.binary,
             size = 1,
             linetype = "dashed"
  )+
  theme_minimal() + 
  labs(
    x = "No. previous games played by user",
    y = "Relative Expected Information Gain"
  ) + 
  theme(
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    panel.grid = element_blank(),
    axis.text = element_text(color = "white"),
    text = element_text(color = "white", size=20)
  )

ggsave("reig_idx_1.png", p.reig.idx.1, bg = "transparent", width = 10, height = 5)


# Overall + Top 50
p.reig.idx.2 <- games %>%
  filter(no_user_games > 2) %>%
  ggplot(aes(game_index, EIG.relative)) +
  stat_summary_bin(fun.data="mean_cl_boot",
                   geom="pointrange",
                   binwidth = 5,
                   color=color.human) +
  geom_smooth(method = 'lm', formula = y~x, color=color.human) + 
  geom_smooth(method = 'lm', formula = y~x,
              mapping=aes(color=factor(user)),
              se=F, data = games %>% filter(no_games_user > 50)) + 
  geom_hline(yintercept = guesses.random %>%
               pull(EIG.relative) %>% mean(na.rm=T),
             color = color.random,
             size = 1,
             linetype = "dashed"
  )+
  geom_hline(yintercept = guesses.binary %>%
               pull(EIG.relative) %>% mean(na.rm=T),
             color = color.binary,
             size = 1,
             linetype = "dashed"
  )+
  theme_minimal() + 
  labs(
    x = "No. previous games played by user",
    y = "Relative Expected Information Gain"
  ) + 
  theme(
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    panel.grid = element_blank(),
    axis.text = element_text(color = "white"),
    text = element_text(color = "white", size=20),
    legend.position = "none"
  )

ggsave("reig_idx_2.png", p.reig.idx.2, bg = "transparent", width = 10, height = 5)


# REIG Relative to Participant Mean

games <- merge(games, 
               users %>% select(user, EIG.relative) %>%
                 rename(EIG.relative.user = EIG.relative))

p.reig.idx.3 <- games %>%
  mutate(
    EIG.relative.self = EIG.relative / EIG.relative.user
  ) %>%
  filter(no_user_games > 2) %>%
  ggplot(aes(game_index, EIG.relative.self)) +
  stat_summary_bin(fun.data="mean_cl_boot",
                   geom="pointrange",
                   binwidth = 5,
                   color=color.human) +
  geom_hline(yintercept = 1,
             color = "white",
             size = 1,
             linetype = "dashed"
  )+
  geom_smooth(method = 'lm', formula = y~x, color=color.human) + 
  theme_minimal() + 
  labs(
    x = "No. previous games played by user",
    y = "REIG change (by user)"
  ) + theme(
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    panel.grid = element_blank(),
    axis.text = element_text(color = "white"),
    text = element_text(color = "white", size=20),
    legend.position = "none"
  )

ggsave("reig_idx_3.png", p.reig.idx.3, bg = "transparent", width = 10, height = 5)

