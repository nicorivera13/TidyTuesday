##########################
# TidyTuesday 2021-08-20
# by Nicolas Rivera
# @Nicogitano13
##########################

# Packages
library('readr')
library('tidyverse')
library('tidytext')
library('stopwords')
library('ggpubr')

# Working directory
setwd('/Users/Nicolas/Documents/MDS-UAI/Programación con R/TidyTuesday/2021-08-17')

# Gettting the data
computer <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-17/computer.csv')

# Stopwords
stopwords <- data.frame(stopwords('english'))
colnames(stopwords) <- c('sw')

others <- tibble(sw = c('zero', 'one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine', 'ten', 'twenty', 'thirty', 'fourty', 'fifty', 'seconds', 'minutes', 'computer', 'can'))

the_stopwords <- bind_rows(stopwords, 
                           others)

# Most frequent words: people vs computers
table(computer$char_type)

freq <- tibble(phrase = computer$interaction, 
               clasif = computer$char_type) %>%
  unnest_tokens(output = word, 
                input = phrase, 
                strip_numeric = TRUE) %>%
  anti_join(the_stopwords, by = c('word' = 'sw')) %>%
  group_by(clasif) %>%
  count(word, sort = TRUE)

# Building figure

frec_people <- freq[freq$clasif == 'Person',]
frec_computers <- freq[freq$clasif == 'Computer',]

graph_people <- frec_people %>% 
  slice_head(n = 15) %>%
  ggplot(aes(y = reorder(word, n), n)) +
  geom_col(fill = "#0057e7") +
  theme_minimal() +
  labs (y = NULL,
        x = "Frequency",
        title = "People")

graph_computers <- frec_computers %>% 
  slice_head(n = 15) %>%
  ggplot(aes(y = reorder(word, n), n)) +
  geom_col(fill = "#D62D20") +
  theme_minimal() +
  labs (y = NULL,
        x = "Frequency",
        title = "Computers")

final <- ggarrange(graph_people, 
                   graph_computers, 
                   ncol = 2, 
                   nrow = 1, 
                   common.legend = TRUE, 
                   legend = 'bottom')

final <- annotate_figure(final, 
                         top = text_grob('Most frequent words by type of character', 
                                         color = 'Black',
                                         face = 'bold', 
                                         size = 14), 
                         bottom = text_grob('Source: SpeechInteraction.org',
                                            color = 'Black',
                                            face = 'italic', 
                                            size = 8))

ggsave('most_freq_words.png')