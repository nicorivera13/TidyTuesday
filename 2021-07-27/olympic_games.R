##########################
# TidyTuesday 2021-07-27
# by Nicolas Rivera
# @Nicogitano13
##########################

# Packages
library('readr')
library('tidyverse')
library('maps')

# Working directory
setwd('/Users/Nicolas/Documents/MDS-UAI/Programación con R/TidyTuesday/2021-07-27')

# Getting the data
olympics <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-27/olympics.csv')

noc_region <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-27/noc_regions.csv')

olympics <- olympics %>% 
  left_join(noc_region,
             by = c('noc' = 'NOC')) %>% 
  distinct() #drop duplicates

# Number of medals by country
country_medals <- olympics %>% 
  filter(!is.na(medal)) %>% 
  group_by(region) %>% 
  summarise(total = n()) %>% 
  filter(!is.na(region))

# Setting world map
world <- map_data('world')
world <- world[world$region != 'Antarctica',]

# Checking countries
countries1 <- data.frame(x=unique(country_medals$region))
countries2 <- data.frame(y=unique(world$region))

check_countries <- anti_join(countries1,
                   countries2, 
                   by = c('x' = 'y'))

country_medals <- country_medals %>% 
  mutate(region = recode(region, 
                         'Virgin Islands, US' = 'USA'))

# Building map
world <- left_join(world,
                   country_medals, 
                   by = 'region')

world_map <- ggplot(data = world, 
                    aes(x = long,
                        y = lat,
                        group = group,
                        fill = total)) +
  geom_polygon() +
  scale_fill_distiller(name = '', palette ="RdYlBu", label = scales::comma) +
  theme_bw() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_rect(colour= "black", size= .25))+
  theme(legend.position="bottom",
        legend.text = element_text(size=8),
        legend.key.size = unit(1, 'cm'),
        plot.caption = element_text(face = 'italic', size = 8)) +
  labs(title = 'Cumulative olympic medals by country',
       subtitle = '(1986-2016)',
       caption = 'Notes: (1) Medal count includes gold, silver and bronze medals. (2) Both summer and winter seasons are considered. (3) "Gray" countries have never won a medal. Source: Kaggle.\nTwitter: @Nicogitano13')

ggsave('olympics_games.png')
