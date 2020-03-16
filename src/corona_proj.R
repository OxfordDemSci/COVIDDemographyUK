# Coronavirus mortality projections by demographic breakdown
# All population numbers are in thousands, sticking with the bayesPop convention

library(dplyr)
library(tidyr)
library(purrr)
library(cowplot)
library(gganimate)
library(stringr)
library(forcats)
library(bayesPop)
library(readxl)

# These are the converged simulations provided by Hana Ševčíková
tfr.dir <- 'data/sim20190525'
e0.dir <- 'data/sim20190528'

pop.dir <- 'data/full_pop_sim'

# To generate the total population trajectories:
# pop.pred <- pop.predict(
#   end.year = 2100, start.year = 1950, present.year = 2020,
#   wpp.year = 2019, output.dir = pop.dir, nr.traj = 1000,
#   inputs = list(tfr.sim.dir = tfr.dir,
#                 e0F.sim.dir = e0.dir, 
#                 e0M.sim.dir = "joint_"),
#   ## optionally run in parallel:
#   parallel = TRUE,
#   verbose = T
# )

# Load the total population trajectories

pop.pred <- get.pop.prediction(pop.dir)

years <- pop.pred$proj.years.pop
countries <- pop.pred$countries$name %>% as.character()
sexes <- c('male', 'female')

# Get the median population estimates, by age, for a sex in a country in a year
# If male, negate the populations, for later plotting
get_median <- function(year, cntry, sex, pop.pred) {
  cntry_medians <- pop.byage.table(pop.pred, country = cntry, year = year, sex = sex) %>%
    as.data.frame() %>%
    select(contains('median')) %>%
    tibble::rownames_to_column('age')
  
  # Rename median.year to year
  names(cntry_medians)[2] <- str_sub(names(cntry_medians)[2], 8)
  
  cntry_medians <- mutate(cntry_medians, sex = sex, country = cntry) %>%
    gather('year', 'pop', 2)
  
  if(sex == 'male') {
    cntry_medians <- mutate(cntry_medians, pop = -pop)
  }
  
  return(cntry_medians)
}

# Take the cartesian product of the years, countries, and sexes
inputs <- expand.grid(years, countries, sexes) %>%
  mutate(Var2 = as.character(Var2), Var3 = as.character(Var3))

# Run get_median for all combinations and concatenate the matrices
medians <- pmap(
  list(
    year = inputs$Var1,
    cntry = inputs$Var2,
    sex = inputs$Var3),
  get_median, pop.pred) %>%
  bind_rows()

# ggplot2 will follow the factor order on the y-axis
medians$age <- factor(medians$age, levels = medians[1:27, 'age'])
medians$sex <- fct_recode(medians$sex, Male = 'male', Female = 'female') %>%
  fct_relevel('Male', 'Female')

# Recode to match the age categories from the Italian coronavirus data
medians$age <- recode(
  medians$age,
  `0-4` = '0-9',
  `5-9` = '0-9',
  `10-14` = '10-19',
  `15-19` = '10-19',
  `20-24` = '20-29',
  `25-29` = '20-29',
  `30-34` = '30-39',
  `35-39` = '30-39',
  `40-44` = '40-49',
  `45-49` = '40-49',
  `50-54` = '50-59',
  `55-59` = '50-59',
  `60-64` = '60-69',
  `65-69` = '60-69',
  `70-74` = '70-79',
  `75-79` = '70-79',
  `80-84` = '80-89',
  `85-89` = '80-89',
  `90-94` = '90+',
  `95-99` = '90+',
  `100-104` = '90+',
  `105-109` = '90+',
  `110-114` = '90+',
  `115-119` = '90+',
  `120-124` = '90+',
  `125-129` = '90+',
  `130+` = '90+'
  )

# Get the data for 2020 and get rid of sex-specificity
medians_2020 <- filter(medians, year == 2020) %>%
  group_by(age, country) %>%
  summarize(pop = sum(abs(pop)))

# Read in Liliana's age-specific CFRs (from Italian data)
italy_rates <- read_excel('data/italy-13march.xlsx') %>%
  select(age, lethality_rate) %>%
  mutate(age = as_factor(age))

medians_2020 <- left_join(medians_2020, italy_rates)

# Assume an infection rate of 0.4
medians_2020 <- mutate(medians_2020, dead = pop*0.4*lethality_rate)

# To get a number that can be compared between countries, divide the number
# of deaths in each country-age cohort by the total population for that country
# Looking at twitter reactions, this isn't intuitive to a lot of people so we
# should think about other options
total_pop <- group_by(medians_2020, country) %>%
  summarize(total_pop = sum(pop))

medians_2020 <- left_join(medians_2020, total_pop) %>%
  mutate(dead_prop = dead/total_pop)

p1 <- filter(medians_2020, country %in% c('Italy', 'United Kingdom', 'United States of America', 'Brazil', 'Nigeria')) %>%
  mutate(country = factor(country, levels = c('Italy', 'United Kingdom', 'United States of America', 'Brazil', 'Nigeria'))) %>%
  ggplot(aes(age, dead_prop, color = country, group = country, fill = country)) +
    geom_line() +
    scale_color_brewer('', palette = 'Dark2') +
    scale_fill_brewer('', palette = 'Dark2') +
    labs(x = 'Age', y = 'Expected deaths/Total pop') +
    theme_cowplot()

p2 <- filter(medians_2020, country %in% c('Italy', 'United Kingdom', 'United States of America', 'Brazil', 'Nigeria')) %>%
  mutate(pop_prop = pop / total_pop, country = factor(country, levels = c('Italy', 'United Kingdom', 'United States of America', 'Brazil', 'Nigeria'))) %>%
  ggplot(aes(age, pop_prop, color = country, group = country)) +
  geom_line() +
  scale_color_brewer('', palette = 'Dark2') +
  labs(x = 'Age', y = 'Proportion') +
  theme_cowplot()

p12 <- plot_grid(p1, p2, nrow = 2)

save_plot('figs/corona_lineplots.pdf', p12, nrow = 2, base_asp = 2.4)
