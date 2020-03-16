# Make an animated population pyramid of the projections, faceted by country

library(dplyr)
library(tidyr)
library(purrr)
library(cowplot)
library(gganimate)
library(stringr)
library(forcats)
library(bayesPop)

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

# For testing, subsample countries
#countries <- sample(countries, 12)

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

# Given a dataframe, render an animation
make_animation <- function(df) {
  anim <- droplevels(df) %>% # Remove unused countries
    ggplot(aes(age, pop, fill = sex, group = age)) +
    geom_bar(stat = 'identity', alpha = 0.8) +
    scale_y_continuous(labels = function(x) as.character(abs(x))) +
    coord_flip() +
    facet_wrap(~ country, ncol = 5, scales = 'free_x') +
    scale_fill_brewer('Sex', palette = 'Dark2') +
    transition_states(year, transition_length = 0.5) +
    shadow_trail(distance = 1, alpha = 0.5) + # Show the first value in the background
    labs(
      title = 'Median population projections to 2100',
      subtitle = 'Year: {closest_state}',
      caption = 'Visualization: David Brazel\nData: Hana Ševčíková and the bayesPop team',
      x = 'Age',
      y = 'Population') +
    theme(legend.title = element_blank()) +
    theme_cowplot()
  
  return(animate(anim, height = 922, width = 1229, nframes = 150))
}

# Sort the vector of countries alphabetically and split it into chunks of 10 countries
countries <- sort(countries)

# Make and save an animation for each chunk
split_countries <- split(countries, ceiling(seq_along(countries) / 10))

write_animation <- function(countries, df) {
  df <- filter(df, country %in% countries)
  
  first <- countries[1]
  last <- countries[length(countries)]
  
  anim <- make_animation(df)
  
  fname <- paste0('figs/pop_pyramid_', first, '_to_', last, '.gif')
  anim_save(fname, anim)
}

walk(split_countries, write_animation, df = medians)
