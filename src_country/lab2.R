library(bayesLife)

# Read the converged MCMC run from disk
e0.converged.pred <- get.e0.prediction('data/sim20190528/')

# Define the countries we're looking at
cntry <- 'Uzbekistan'
nbr1 <- 'Turkmenistan'
nbr2 <- 'Tajikistan'
nbr3 <- 'Kazakhstan'

# Retrieve their trajectories as matrices with time periods along the rows
# and simulations along the columns
tr_cntry <- get.e0.trajectories(e0.converged.pred, cntry)
tr_nbr1 <- get.e0.trajectories(e0.converged.pred, nbr1)
tr_nbr2 <- get.e0.trajectories(e0.converged.pred, nbr2)
tr_nbr3 <- get.e0.trajectories(e0.converged.pred, nbr3)

# Calculate the element-wise minima of those matrices
nbr_min <- pmin(tr_nbr1, tr_nbr2, tr_nbr3)

# Get a Boolean matrix indicating, for each entry, whether Uzbekistan has a
# lower life expectancy than its three neighbors
tr_nbr_comp <- tr_cntry < nbr_min

# Apply takes its first argument (a dataframe) and passes the dataframe's rows
# or columns (as determined by its second argument) to its third argument (a function)
# In this case, the function all will return TRUE if and only if all of the values
# passed to it are TRUE. We pass each simulation (each column) of the Boolean matrix
# to it, to determine wheter Uzbekistan's life expectancy was higher than its'
# neighbors at all time points in that simulation. We return a vector of these
# Boolean values
log_vec <- apply(tr_nbr_comp, 2, all)

# The sum of a Boolean vector in R is the number of TRUE values
sum(log_vec) / length(log_vec)


# Here are the three previous lines in one
sum(apply(tr_cntry < nbr_min, 2, all)) / dim(nbr_min)[2]
