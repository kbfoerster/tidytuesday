# Get the data
library(tidyverse)
library(ggdark)

tuesdata <- tidytuesdayR::tt_load('2020-01-21') 
tuesdata <- tidytuesdayR::tt_load(2020, week = 4)

spotify_songs <- tuesdata$spotify_songs

str(spotify_songs)

theme_set(dark_theme_minimal(base_family = "Bebas Neue"))

spotify_songs %>%
  group_by(playlist_genre, playlist_subgenre) %>%
  summarise(avg_danceability = mean(danceability)) %>%
  ungroup() %>%
  ggplot(aes(x = reorder(playlist_subgenre, avg_danceability), y=avg_danceability, fill=playlist_genre)) + 
  geom_bar(stat="identity") + 
  coord_flip() + 
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.title = element_blank(),
    plot.title = element_text(size = 20),
  ) + 
  labs(title = "Subgenre Danceability"
       )

# TODO envisioning a thin (colored?) line with the dancing emoji on the ends for a fun twist
