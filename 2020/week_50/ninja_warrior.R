library(tidyverse)
library(extrafont)
library(ggthemes)
tuesdata <- tidytuesdayR::tt_load(2020, week = 51)

df <- tuesdata$ninja_warrior


top_obstacle <- df %>%
  count(obstacle_name, sort = T) %>%
  top_n(5)

png("ninja_warrior.png")

df %>%
  filter(obstacle_name %in% top_obstacle$obstacle_name) %>%
  group_by(season, obstacle_name) %>%
  count(sort = T) %>%
  ggplot(aes(x = season, y = n, fill = obstacle_name)) + 
  theme_economist() +
  geom_bar(position = 'stack', stat = 'identity' ) + 
  scale_x_discrete(name = "Season", limits = seq(1,10)) +
  scale_y_continuous(limits = c(0, 35), breaks = seq(0,35,5)) +
  scale_fill_manual(values = c("#00AC41", "#FFD700", "#284C88", "#D8282B", "#FFFFFF")) +
  coord_flip() + 
  labs(
    y = 'Count', 
    title = 'Top 5 Overall Ninja Warrior Obstacles Across Seasons', 
    legend = 'Obstacle Name') +
  theme(
    legend.position = c(0.7, 0.175),
    legend.title = element_blank(),
    legend.key.size = unit(0.2, "cm"),
    panel.grid.major.y = element_blank()
  ) + 
  guides(fill = guide_legend(ncol = 2, title = 'Obstacle Name'))

dev.off()