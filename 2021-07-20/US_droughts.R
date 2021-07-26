##########################
# TidyTuesday 2021-07-20
# by Nicolas Rivera
# @nicogitano13
##########################

# Packages
library('readr')
library('forcats')
library('tidyverse')
library('ggpubr')

# Working directory
setwd('/Users/Nicolas/Documents/MDS-UAI/Programación con R/TidyTuesday/2021-07-20')

# Reading the data
drought <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-20/drought.csv')

names(drought)
str(drought)

# Changing drought levels
drought <- drought %>% 
  mutate(drought_lvl = fct_recode(as.factor(drought_lvl), 
                            Abnormally = 'D0', 
                            Moderate = 'D1',
                            Severe = 'D2',
                            Extreme = 'D3',
                            Exceptional = 'D4'))

table(drought$drought_lvl)

# Computing total area by state
area_state <- drought %>% 
  mutate(area_state = ifelse(area_pct == 0, NA, area_total*100/area_pct)) %>% 
  group_by(state_abb) %>% 
  summarise(area_state = mean(area_state, na.rm = TRUE))
  
# Getting regions
states <-  read_csv('https://raw.githubusercontent.com/cphalpert/census-regions/master/us%20census%20bureau%20regions%20and%20divisions.csv')

drought <- drought %>% 
  left_join(states, by = c('state_abb' = 'State Code')) %>% 
  left_join(area_state, by = 'state_abb')

table(drought$Region)

# Total areas by regions
drought_region <- drought %>% 
  group_by(valid_start, valid_end, Region, drought_lvl) %>% 
  summarise(area_drought = sum(area_total),
          area_region = sum(area_state)) %>% 
  filter(!is.na(Region)) %>% 
  mutate(perc = round((area_drought/area_region)*100, 2))

# Figures
regions <- unique(drought_region$Region)

list_regions <- map(regions, ~ drought_region %>% 
               filter(Region == .x & drought_lvl != 'None')) %>% 
               set_names(regions)

graphs <- map2(list_regions, names(list_regions),
                 ~ ggplot(.x, aes(x = valid_start, y = perc, fill = drought_lvl)) + 
                   geom_col() +
                   xlab('Date') + ylab('%') + 
                   ylim(0,100) +
                   guides(fill=guide_legend(title='')) + 
                   theme_bw() +
                   theme(axis.text.x = element_text(angle=90, hjust=1)) +
                   scale_x_date(date_breaks = '1 years', date_labels = "%Y") +
                   labs(title = .y))

final <- ggarrange(graphs$Midwest, graphs$Northeast, graphs$South, graphs$West , 
                   ncol = 2, nrow = 2, common.legend = TRUE, legend = 'bottom')

final <- annotate_figure(final, 
                         top = text_grob('Percent of region in that drought category', 
                                       color = 'Black', 
                                       face = 'bold', 
                                       size = 14),
                         bottom = text_grob('Note: Category "no drought" is omitted. Source: U.S. Drought Monitor.', 
                                            color = 'Black', 
                                            face = 'italic', 
                                            size = 10))

final

ggsave('US_droughts.png')
