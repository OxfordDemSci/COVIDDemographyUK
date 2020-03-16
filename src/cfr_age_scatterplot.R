# Scatterplot of CFR and %>65

library(readr)
library(ggplot2)
library(dplyr)
library(cowplot)

jhu_data <- read_csv('data/JHU_03-15-2020.csv') %>%
  filter(Confirmed > 0) %>%
  group_by(`Country/Region`) %>%
  summarize(confirmed = sum(Confirmed), deaths = sum(Deaths), recovered = sum(Recovered)) %>%
  mutate(crude_cfr = deaths/confirmed) %>%
  mutate(country = recode(`Country/Region`, `Korea, South` = 'South Korea', US = 'USA', `Taiwan*` = 'Taiwan')) %>%
  select(country, confirmed, crude_cfr)
  
pop_2020 <- read_csv('data/corona_pop_proj.csv') %>%
  mutate(country = recode(country, `Bolivia (Plurinational State of)` = 'Bolivia', `Iran (Islamic Republic of)` = 'Iran', `Russian Federation` = 'Russia', `United States of America` = 'USA', `Republic of Korea` = 'South Korea', `Venezuela (Bolivarian Republic of)` = 'Venezuela', `Viet Nam` = 'Vietnam', `China, Taiwan Province of China` = 'Taiwan')) %>%
  filter(age %in% c('70-79', '80-89', '90+')) %>%
  group_by(country) %>%
  summarize(pop = sum(pop), total_pop = first(total_pop)) %>%
  mutate(percent_70_plus = pop / total_pop) %>%
  left_join(jhu_data)

p <- filter(pop_2020, confirmed > 200) %>%
ggplot(aes(percent_70_plus, crude_cfr, size = confirmed, label = country)) +
  geom_text() +
  theme_cowplot()

save_plot('figs/cfr_age_scatterplot.pdf', p)
