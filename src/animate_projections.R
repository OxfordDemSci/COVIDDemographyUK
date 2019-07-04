# Make an animated population pyramid of the projections, faceted by country

library(dplyr)
library(tidyr)
library(purrr)
library(cowplot)
library(gganimate)
library(stringr)
library(forcats)
library(bayesPop)

tfr.dir <- 'sim20190525'
e0.dir <- 'sim20190528'
pop.dir <- 'full_pop_sim'

pop.pred <- pop.predict(
  end.year = 2100, start.year = 1950, present.year = 2020,
  wpp.year = 2019, output.dir = pop.dir, nr.traj = 1000,
  inputs = list(tfr.sim.dir = tfr.dir,
                e0F.sim.dir = e0.dir, 
                e0M.sim.dir = "joint_"),
  ## optionally run in parallel:
  parallel = TRUE,
  verbose = T
)

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
medians$sex <- fct_recode(medians$sex, Female = 'female', Male = 'male')

anim <- ggplot(medians, aes(age, pop, fill = sex, group = age)) + 
  geom_bar(stat = 'identity', alpha = 0.8) +
  scale_y_continuous(labels = function(x) as.character(abs(x))) +
  coord_flip() +
  facet_wrap(~ country, ncol = 6, scales = 'free_x') +
  scale_fill_brewer(palette = 'Dark2') +
  transition_states(year, transition_length = 0.5) +
  shadow_trail(distance = 1, alpha = 0.5) + # Show the first value in the background
  labs(
    title = 'Median population projections to 2100',
    subtitle = 'Year: {closest_state}',
    caption = 'Visualization: David Brazel\nData: bayesPop Team',
    x = 'Age',
    y = 'Population') +
  theme(panel.spacing = unit(2, "lines"), legend.title = element_blank())

animated <- animate(anim, height = 800, width = 1320, nframes = 150)
anim_save('pop_pyramids.gif')
