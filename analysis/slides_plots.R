
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

