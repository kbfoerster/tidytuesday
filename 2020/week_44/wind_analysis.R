library(tidyverse)

tuesdata <- tidytuesdayR::tt_load(2020, week = 44)

df <- tuesdata$`wind-turbine`

df %>%
  count(commissioning_date, sort = T) %>%
  ggplot(aes(commissioning_date, n)) + 
  geom_bar(stat = 'identity') + 
  coord_flip()

top_manufacturer <- df %>%
  count(manufacturer, sort = T) %>%
  top_n(5)

df %>%
  filter(manufacturer %in% top_manufacturer$manufacturer) %>%
  group_by(manufacturer, commissioning_date) %>%
  count(sort = T)

# TODO find a way to do every other y-tick or something when these two pipes are merged
  ggplot(aes(x = commissioning_date, y = n, fill = manufacturer)) + 
  geom_bar(stat = 'identity') + 
  scale_x_continuous(breaks = df[seq(1, length(df$commissioning_date), by =2)]) +
  coord_flip() + 
  facet_wrap(vars(manufacturer)) 
  
