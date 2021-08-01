##########################
# TidyTuesday 2021-07-13
# by Nicolas Rivera
# @Nicogitano13
##########################

# Packages
library('readr')
library('tidyverse')

#install.packages('gganimate')
library(gganimate)
#install.packages('hrbrthemes')
library(hrbrthemes)
#install.packages('gifski')
library('gifski')
#install.packages('av')
library('av')
#install.packages('animation')
library('animation')

# Working directory
setwd('/Users/Nicolas/Documents/MDS-UAI/Programación con R/TidyTuesday/2021-07-13')

# Getting the data
scoobydoo <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-13/scoobydoo.csv')

str(scoobydoo)

# Extracting year
scoobydoo$year <- as.numeric(substr(as.character(scoobydoo$date_aired), start = 1, stop = 4))
class(scoobydoo$year)

# Computing number of monsters caught/captured by character and year
scoobydoo$fred <- ifelse(scoobydoo$caught_fred == 'TRUE' | scoobydoo$captured_fred == 'TRUE', 1, 0)
scoobydoo$daphnie <- ifelse(scoobydoo$caught_daphnie == 'TRUE' | scoobydoo$caught_daphnie == 'TRUE', 1, 0)
scoobydoo$velma <- ifelse(scoobydoo$caught_velma == 'TRUE' | scoobydoo$captured_velma == 'TRUE', 1, 0)
scoobydoo$shaggy <- ifelse(scoobydoo$caught_shaggy == 'TRUE' | scoobydoo$captured_shaggy == 'TRUE', 1, 0)
scoobydoo$scooby <- ifelse(scoobydoo$caught_scooby == 'TRUE' | scoobydoo$captured_scooby == 'TRUE', 1, 0)

year <- data_frame(year = min(scoobydoo$year):max(scoobydoo$year))
                            
monsters <- scoobydoo %>% 
  group_by(year) %>% 
  summarise(fred = sum(fred),
            daphnie = sum(daphnie),
            velma = sum(velma),
            shaggy = sum(shaggy),
            scooby = sum(scooby)) %>% 
  full_join(year, by = 'year') %>% 
  arrange(year)

monsters$fred[is.na(monsters$fred)] <- 0
monsters$daphnie[is.na(monsters$daphnie)] <- 0
monsters$velma[is.na(monsters$velma)] <- 0
monsters$shaggy[is.na(monsters$shaggy)] <- 0
monsters$scooby[is.na(monsters$scooby)] <- 0

monsters$fred_cum <- NULL
for (i in min(scoobydoo$year):max(scoobydoo$year)){
  if (i == min(scoobydoo$year)){
    monsters$fred_cum[monsters$year == i] <- monsters$fred
  }
  else{
    monsters$fred_cum[monsters$year == i] <- monsters$fred[monsters$year == i] + monsters$fred_cum[monsters$year == i-1]
  }
}

monsters$daphnie_cum <- NULL
for (i in min(scoobydoo$year):max(scoobydoo$year)){
  if (i == min(scoobydoo$year)){
    monsters$daphnie_cum[monsters$year == i] <- monsters$daphnie
  }
  else{
    monsters$daphnie_cum[monsters$year == i] <- monsters$daphnie[monsters$year == i] + monsters$daphnie_cum[monsters$year == i-1]
  }
}

monsters$velma_cum <- NULL
for (i in min(scoobydoo$year):max(scoobydoo$year)){
  if (i == min(scoobydoo$year)){
    monsters$velma_cum[monsters$year == i] <- monsters$velma
  }
  else{
    monsters$velma_cum[monsters$year == i] <- monsters$velma[monsters$year == i] + monsters$velma_cum[monsters$year == i-1]
  }
}

monsters$shaggy_cum <- NULL
for (i in min(scoobydoo$year):max(scoobydoo$year)){
  if (i == min(scoobydoo$year)){
    monsters$shaggy_cum[monsters$year == i] <- monsters$shaggy
  }
  else{
    monsters$shaggy_cum[monsters$year == i] <- monsters$shaggy[monsters$year == i] + monsters$shaggy_cum[monsters$year == i-1]
  }
}

monsters$scooby_cum <- NULL
for (i in min(scoobydoo$year):max(scoobydoo$year)){
  if (i == min(scoobydoo$year)){
    monsters$scooby_cum[monsters$year == i] <- monsters$scooby
  }
  else{
    monsters$scooby_cum[monsters$year == i] <- monsters$scooby[monsters$year == i] + monsters$scooby_cum[monsters$year == i-1]
  }
}

monsters_final <- monsters[,c('year', 'fred_cum', 'daphnie_cum', 
                              'velma_cum', 'shaggy_cum', 'scooby_cum')]

monsters_final <- gather(monsters_final, fred_cum, daphnie_cum, 
                         velma_cum, shaggy_cum, scooby_cum,
                         key = characters , value = total)  

monsters_final$characters[monsters_final$characters == 'daphnie_cum'] <- 'Daphnie'
monsters_final$characters[monsters_final$characters == 'fred_cum'] <- 'Fred'
monsters_final$characters[monsters_final$characters == 'scooby_cum'] <- 'Scooby'
monsters_final$characters[monsters_final$characters == 'shaggy_cum'] <- 'Shaggy'
monsters_final$characters[monsters_final$characters == 'velma_cum'] <- 'Velma'

monsters_final  %>%
  ggplot(aes(x=year, y=total, group=characters, color=characters)) +
  geom_line() +
  geom_point() +
  labs(x='Year',
       y = 'Number of monsters',
       color = '',
    title = 'Scooby-doo, where are you!',
       subtitle = 'Number of monsters caught/captured by character considering all \n the episodes and movies aired in US per year',
       caption = 'Source: Kaggle.\nTwitter: @Nicogitano13')+
  transition_reveal(year) +
  theme_bw() + 
  theme(plot.title = element_text(face="bold"))

anim_save('scooby_doo.gif')
