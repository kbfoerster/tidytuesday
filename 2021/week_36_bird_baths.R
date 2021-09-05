library(tidyverse)
library(ggplot2)
library(ggthemes)
library(wesanderson)

tuesday <- tidytuesdayR::tt_load(2021, week=36)

bird_baths <- tuesday$bird_baths

#### eda on bird_baths ####

# survey_year,urban_rural, and bioregions all have same NA values 
# going to exclude as it's not relevant to visualization

#bird_baths %>% 
#  filter(is.na(urban_rural)) %>% 
#  distinct(bioregions)

# filtering out NA urban_rural
bird_df <- bird_baths %>% 
  filter(!is.na(urban_rural))


# finding and grabbing top 5 bioregions
top_bio <- bird_df %>% 
  select(bioregions, bird_count) %>% 
  group_by(bioregions) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  ungroup() %>% 
  top_n(n= 5) 

# creating summarized table
region_sum <- bird_df %>% 
  filter(bioregions %in% top_bio$bioregions) %>% 
  select(urban_rural,bioregions, bird_count) %>% 
  group_by(urban_rural,bioregions) %>% 
  summarise(count = sum(bird_count))


#### Visualization ####

# creating our theme

my_theme <- theme(
  #plot.title=element_markdown(family='regular', hjust=0.5, size=45, color='black'),
  plot.title.position = 'plot',
  plot.caption = element_text(hjust=0.5),
  plot.caption.position = 'plot',
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  # messing with axis
  axis.ticks = element_blank(),
  axis.line = element_blank(),
  legend.title = element_blank()
)

ggplot(region_sum, aes(bioregions, count)) +
  geom_col(aes(fill=urban_rural)) +
  labs(x='Bioregions',
       y='Bird Count',
       title='Bird Count by Bioregion',
       #subtitle='Total bird count from 2014 to 2015 for several bioregions, further divided by rural vs urban sightings',
       caption='\nKyle Foerster | @kbfoerster | #TidyTuesday',
       legend='') +
  theme_economist() +
  scale_fill_manual(values=wes_palette(n=2, name='Moonrise2'), name='')
  #my_theme
