library(dplyr)
library(tidyr)
library(cowplot)
library(bayesTFR)

# Read in the converged simulation
tfr.pred.converged <- get.tfr.prediction('data/sim20190525/')

# Make the DL curve plots
DLcurve.plot(tfr.pred.converged, country = 'Ghana')
DLcurve.plot(tfr.pred.converged, country = 'Papua New Guinea')

# Get the median trajectories for each country
papua_trajectory <-
  tfr.trajectories.table(tfr.pred.converged, 'Papua New Guinea') %>%
  as.data.frame() %>%
  tibble::rownames_to_column('year')

ghana_trajectory <-
  tfr.trajectories.table(tfr.pred.converged, 'Ghana') %>%
  as.data.frame() %>%
  tibble::rownames_to_column('year')

# Get a single data frame
dual_trajectories <- bind_rows(papua_trajectory, ghana_trajectory)

# Calculate an indicator of whether a value is observed or projected
dual_trajectories$old <- dual_trajectories$year <= 2018

# Get the full trajectories
ghana_trajs <-
  get.tfr.trajectories(tfr.pred.converged, 'Ghana') %>%
  as.data.frame() %>%
  tibble::rownames_to_column('year')
papua_trajs <-
  get.tfr.trajectories(tfr.pred.converged, 'Papua New Guinea') %>%
  as.data.frame() %>%
  tibble::rownames_to_column('year')

# Convert from wide to long format
ghana_trajs <- gather(ghana_trajs, traj_num, median, V1:V1000)
ghana_trajs$traj_num <- paste0(ghana_trajs$traj_num, 'G')
papua_trajs <- gather(papua_trajs, traj_num, median, V1:V1000)
papua_trajs$traj_num <- paste0(papua_trajs$traj_num, 'P')

dual_samples <- bind_rows(papua_trajs, ghana_trajs)

# Subsample the trajectories for plotting purposes
dual_samples_sel <- dual_samples[c(1:4250, 17001:21250),]

#Plot the trajectories for Ghana and Papua New Guinea
ggplot(dual_trajectories,
       aes(year, median, group = country, color = country)) +
  geom_line(aes(group = traj_num), data = dual_samples_sel, alpha = 0.15) + # Add the full trajectories
  geom_line(aes(alpha = old), size = 1.5) + # Add the median line
  geom_line(aes(y = `0.1`),
            alpha = 0.5,
            linetype = 2,
            size = 1.5) + # Add the 10th percentile line
  geom_line(aes(y = `0.9`),
            alpha = 0.5,
            linetype = 2,
            size = 1.5) + # Add the 90th percentile line
  scale_color_brewer(type = 'qual', palette = 'Dark2') + # Set the color scheme
  scale_alpha_manual(values = c('FALSE' = 0.5, 'TRUE' = 1)) + # Define the alpha values for the median line
  geom_point(color = 'black') + # Add points for each observation on the median line
  ylab('TFR') + # Change the y-label
  guides(alpha = F) # Remove the legend for the alpha differences on the median line
