
library(tidyverse)
library(RColorBrewer)
library(knitr)
library(lme4)

# Load data from data processing 
guesses <- read.csv('../data/guesses_processed.csv')
games <- read.csv('../data/games_processed.csv')
users <- read.csv('../data/users_processed.csv')

# Load simulation data
guesses.binary = read.csv('../data/guesses_binary_search.csv')
guesses.random = read.csv('../data/guesses_random_search.csv')

# Create color palette
col.pal <- brewer.pal(6, "Set1")

color.human = col.pal[2]
color.binary = col.pal[3]
color.random = col.pal[1]

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

# Empty


p.eig.user.empty <- users %>%
  ggplot(aes(x = EIG)) + 
  # geom_density(data = users.random,
  #              color=color.random, fill=color.random, alpha=0.5, size=0.5) +
  # geom_density(data = users.binary,
  # color=color.binary, fill=color.binary, alpha=0.5, size=0.5) +
  # geom_density(color=color.human, fill = color.human, alpha=0.8, size=0.5) +
  geom_hline(yintercept=0, color="black", size=0.5) +
  theme_minimal() +
  labs(
    x = "Expected Information Gain",
    y = "Density"
  ) + 
  xlim(0, 1.5) +
  ylim(0, 7) +
  theme(
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    panel.grid = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.text = element_text(color = "black"),
    text = element_text(color = "black", size=20)
  )

ggsave("img/eig_user_empty.png", p.eig.user.empty, bg = "transparent", width = 10, height = 5)


# Random guessing

p.eig.user.random <- users %>%
  ggplot(aes(x = EIG)) + 
  geom_density(data = users.random,
               color=color.random, fill=color.random, alpha=0.5, size=0.5) +
  # geom_density(data = users.binary,
               # color=color.binary, fill=color.binary, alpha=0.5, size=0.5) +
  # geom_density(color=color.human, fill = color.human, alpha=0.8, size=0.5) +
  theme_minimal() +
  labs(
    x = "Expected Information Gain",
    y = "Density"
  ) + 
  xlim(0, 1.5) +
  ylim(0, 7) +
  theme(
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    panel.grid = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.text = element_text(color = "black"),
    text = element_text(color = "black", size=20)
  )

ggsave("img/eig_user_random.png", p.eig.user.random, bg = "transparent", width = 10, height = 5)

p.eig.user.binary <- users %>%
  ggplot(aes(x = EIG)) + 
  geom_density(data = users.random,
               color=color.random, fill=color.random, alpha=0.5, size=0.5) +
  geom_density(data = users.binary,
  color=color.binary, fill=color.binary, alpha=0.5, size=0.5) +
  # geom_density(color=color.human, fill = color.human, alpha=0.8, size=0.5) +
  theme_minimal() +
  labs(
    x = "Expected Information Gain",
    y = "Density"
  ) + 
  xlim(0, 1.5) +
  ylim(0, 7) +
  theme(
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    panel.grid = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.text = element_text(color = "black"),
    text = element_text(color = "black", size=20)
  )

ggsave("img/eig_user_binary.png", p.eig.user.binary, bg = "transparent", width = 10, height = 5)


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
  xlim(0, 1.5) +
  ylim(0, 7) +
  theme(
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    panel.grid = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.text = element_text(color = "black"),
    text = element_text(color = "black", size=20)
  )

ggsave("img/eig_user.png", p.eig.user, bg = "transparent", width = 10, height = 5)

#### COGSCI 2021 figure

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
    y = "Density",
    title = "Distribution of Expected Information Gain across guesses"
  ) + 
  xlim(0, 1.5) +
  ylim(0, 7) +
  theme(
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    panel.grid = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.text = element_text(color = "black"),
    text = element_text(color = "black", size=20)
  )

ggsave("img/eig_user_cogsci2021.png", p.eig.user, bg = "transparent", width = 10, height = 5)


# ----- REIG vs Game Index ----- #

# Empty
p.reig.idx.empty <- games %>%
  filter(no_user_games > 2) %>%
  ggplot(aes(game_index, EIG.relative)) +
  # stat_summary_bin(fun.data="mean_cl_boot",
  #                  geom="pointrange",
  #                  binwidth = 5,
  #                  color="transparent") +
  # geom_smooth(method = 'lm', formula = y~x, color="transparent",fill="transparent") +
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
  ylim(0.76, 1.0321930) +
  xlim(1, 137.5) +
  theme_minimal() + 
  labs(
    x = "No. previous games played by user",
    y = "Relative Expected Information Gain"
  ) + 
  theme(
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    panel.grid = element_blank(),
    axis.text = element_text(color = "black"),
    text = element_text(color = "black", size=20)
  )

ggsave("img/reig_idx_empty.png", p.reig.idx.empty, bg = "transparent", width = 10, height = 5)


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
    axis.text = element_text(color = "black"),
    text = element_text(color = "black", size=20)
  )

ggsave("img/reig_idx_1.png", p.reig.idx.1, bg = "transparent", width = 10, height = 5)


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
    axis.text = element_text(color = "black"),
    text = element_text(color = "black", size=20),
    legend.position = "none"
  )

ggsave("img/reig_idx_2.png", p.reig.idx.2, bg = "transparent", width = 10, height = 5)


# REIG Relative to Participant Mean

## Empty

p.reig.idx.3.empty <- games %>%
  mutate(
    EIG.relative.self = EIG.relative / EIG.relative.user
  ) %>%
  filter(no_user_games > 2) %>%
  ggplot(aes(game_index, EIG.relative.self)) +
  # stat_summary_bin(fun.data="mean_cl_boot",
  #                  geom="pointrange",
  #                  binwidth = 5,
  #                  color=color.human) +
  geom_hline(yintercept = 1,
             color = "black",
             size = 1,
             linetype = "dashed"
  )+
  # geom_smooth(method = 'lm', formula = y~x, color=color.human) + 
  theme_minimal() + 
  ylim(0.9, 1.05) + 
  xlim(1, 137.5) +
  labs(
    x = "No. previous games played by user",
    y = "REIG change (by user)"
  ) + theme(
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    panel.grid = element_blank(),
    axis.text = element_text(color = "black"),
    text = element_text(color = "black", size=20),
    legend.position = "none"
  )

ggsave("img/reig_idx_3_empty.png", p.reig.idx.3.empty, bg = "transparent", width = 10, height = 5)


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
             color = "black",
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
    axis.text = element_text(color = "black"),
    text = element_text(color = "black", size=20),
    legend.position = "none"
  )

ggsave("img/img/reig_idx_3.png", p.reig.idx.3, bg = "transparent", width = 10, height = 5)

