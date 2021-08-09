##########################
# TidyTuesday 2021-08-03
# by Nicolas Rivera
# @Nicogitano13
##########################

# Packages
library('readr')
library('tidyverse')
library('countrycode')
library('wordcloud2') 

library('webshot')
webshot::install_phantomjs()
library('htmlwidgets')

# Working directory
setwd('/Users/Nicolas/Documents/MDS-UAI/Programación con R/TidyTuesday/2021-08-03')

# Getting the data
athletes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-03/athletes.csv')

# Data mining
table(athletes$medal)

athletes$names <- countrycode(athletes$abb,
                              origin = 'iso3c', 
                              destination = 'country.name')

table(athletes$abb[is.na(athletes$names)])

list_countries <- athletes %>% 
  select(abb, country, names) %>% 
  filter(!is.na(abb) & !is.na(country) & is.na(names)) %>% 
  select(abb, country) %>% 
  unique %>% 
  arrange(abb) %>% 
  slice(1:3,8:9,17:20,23:26) %>% 
  transmute(abb = abb, country2 = country)

athletes <- left_join(athletes,
                      list_countries,
                      by = 'abb')  

athletes$final_country <- ifelse(is.na(athletes$names), 
                                 athletes$country2,
                                 athletes$names)

athletes$final_country[athletes$final_country == 'FR Germany'] <- 'Germany'
athletes$final_country[athletes$final_country == 'Hong Kong SAR China'] <- 'China'
athletes$final_country[athletes$final_country == 'USSR'] <- 'Russia'
athletes$final_country[athletes$final_country == 'Chinese Taipei'] <- 'China'

# Total number of medals
medals <- athletes %>% 
  filter(!is.na(final_country)) %>% 
  group_by(final_country) %>% 
  summarise(freq = n()) %>% 
  arrange(desc(freq)) %>% 
  transmute(word = final_country, freq = freq)

# Word cloud
my_graph <- wordcloud2(medals, 
                       size=1, 
                       color='random-light', 
                       backgroundColor='black')

saveWidget(my_graph,"paralympic_games.html", selfcontained = F)
