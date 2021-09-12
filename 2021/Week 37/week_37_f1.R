# Week 37 TidyTuesday
library(tidytuesdayR)
library(tidyverse)
library(ggtext)
library(showtext)
library(ragg)

tuesdata <- tidytuesdayR::tt_load(2021, week = 37)


results <- tuesdata$results
constructors <- tuesdata$constructors
drivers <- tuesdata$drivers

# creating master df from relevant data
df <- results %>% 
  select(resultId, driverId, constructorId, position, points, laps, time) %>% 
  left_join(drivers, by='driverId') %>% 
  select(-c(nationality,url)) %>% 
  left_join(constructors, by='constructorId') %>% 
  select(-c(nationality,url)) %>% 
  rename(constructorName = name)

# getting top 10 drivers by total points
top_p <- df %>% 
  select(surname,points) %>% 
  group_by(surname) %>% 
  summarise(totalPoints = sum(points)) %>% 
  arrange(desc(totalPoints)) %>% 
  top_n(10)

# getting top 10 constructors by raceCount
top_c <- df %>% 
  filter(surname %in% top_p$surname) %>% 
  select(constructorName) %>% 
  count(constructorName) %>% 
  top_n(10)

# creating visualization data from above filters
vis_df <- df %>% 
  filter(surname %in% top_p$surname) %>% 
  select(surname, constructorName) %>% 
  group_by(surname, constructorName) %>% 
  count() %>% 
  rename(raceCount = n) %>% 
  ungroup() %>% 
  mutate(constructorOther = replace(constructorName, !constructorName %in% top_c$constructorName, "Other"))
  
# creating cosntructorColors based on F1 team colors
constructorColors <- c(
  "Benetton" = "#008860",
  "Ferrari" = "#DC0000", 
  "Honda" = "#9d2933",
  "McLaren" = "#FF8700",
  "Mercedes" = "#00D2BE",
  "Other" = "#8d847f",
  "Red Bull" = "#1E41FF",
  "Renault" = "#FFF500",
  "Toro Rosso" = "#469BFF",
  "Toyota" = "#49b675",
  "Williams" = "#FFFFFF"
)

# getting F1 fonts
font_add_google(family="Baloo 2", "Baloo 2")
showtext_auto()

dark_theme2 <- theme(
  # theme based on @moriah_taylor58 work https://github.com/moriahtaylor1

  #titles
  plot.title=element_text(family="Baloo 2", hjust=0.5, size=35, color="white"),
  plot.title.position = "plot",
  plot.subtitle = element_markdown(family="Baloo 2", size=35, hjust=0.5, color="white"),
  plot.caption=element_text(family="Baloo 2", size=35, color="#333333", hjust=0.5),
  plot.caption.position = "plot",
  #background
  panel.border=element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "black"),
  plot.background = element_rect(fill = "black"),
  plot.margin=ggplot2::margin(0.5, 0.5, 0.5, 0.5, "in"),
  #axes
  axis.ticks.length=unit(0.15, "cm"),
  axis.ticks = element_line(),
  axis.line = element_blank(),
  axis.title = element_blank(),
  axis.text = element_text(family="Baloo 2", size=20, color="white"),
  #legend
  legend.position = "top",
  legend.background = element_rect(fill="black", color="black"),
  legend.box.background = element_rect(fill="black", color="black"),
  legend.text = element_text(family="Baloo 2", color="white", size=18),
  legend.title = element_blank(),
  legend.key = element_rect(fill="black"))

# creating initial visualization
racesConstructor <- ggplot(data=vis_df) + 
  geom_col(aes(x=surname, y=raceCount, fill=constructorOther)) + 
  coord_flip() +
  labs(title="Number of Formula 1 Races by Driver",
       subtitle="With # of races for constructor") +
  scale_fill_manual(values = constructorColors) + 
  dark_theme2
  
ggsave("RacesByDriverConstructor.png",
       plot=racesConstructor,
       device = agg_png(width = 7, height = 5, units = "in", res = 300))
