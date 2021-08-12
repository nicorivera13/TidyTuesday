##########################
# TidyTuesday 2021-08-10
# by Nicolas Rivera
# @Nicogitano13
##########################

# Packages
library('readr')
library('tidyverse')
library('viridis')
library('hrbrthemes')

# Working directory
setwd('/Users/Nicolas/Documents/MDS-UAI/Programación con R/TidyTuesday/2021-08-10')

investment <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-10/investment.csv')
chain_investment <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-10/chain_investment.csv')
ipd <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-10/ipd.csv')

# Exploring the data

table(chain_investment$meta_cat)
table(chain_investment$category[chain_investment$meta_cat=='Education'])

chain_investment_educ <- chain_investment[chain_investment$meta_cat=='Education', ]

table(chain_investment_educ$category)

# Graph

ggplot(chain_investment_educ, aes(x=year, y=gross_inv_chain, fill=category)) + 
  geom_area(alpha=0.6 , size=.5, colour='white') +
  scale_fill_viridis(discrete = T, name = 'Category of investment') +
  theme_ipsum() + 
  labs(title = 'US investment in education',
       subtitle = 'Gross investment (chained 2021 dollars) in millions of USD, 1947-2017',
       caption = 'Source:  Bureau of Economic Analysis') +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = seq(from = 1950 , to = 2020, by = 10), limits = c(1945, 2020)) +
  xlab('Year') + ylab('Millions of USD') +
  theme(axis.text.x = element_text(angle = 0),
        legend.key.size = unit(0.5, 'cm'),
        legend.text = element_text(size=7.5),
        legend.title = element_text(size=10))

ggsave('us_investment_educ.png')
