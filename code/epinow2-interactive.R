# Author: Sam Abbott
# This script is designed to be run within code/interactiveSimulation.R and 
# handles EpiNow2 estimation of the simulated data specified in that script
# Packages 
library(EpiNow2)
library(data.table)
library(purrr)

# Set up cores
options(mc.cores = 4)

# Convert data into expected format for EpiNow2
simulated_cases <- as.data.table(simulation)
simulated_cases <- simulated_cases[,.(date, confirm = observations)]

# Add poisson noise 
# Huisman et al simulation used above is deterministic outside of delays from
# infection to observation (critically it has no error model for infections or 
# observations). This causes EpiNow2 issues as it is designed to fit to real world 
# data where observation error is likely to be present.
simulated_cases$confirm <- map_dbl(simulated_cases$confirm, ~ rpois(1, .))

# Estimate delays from gamma distributions used for simulation
# EpiNow2 is designed to work with only lognormal delays currently 
# so here we approximate lognormal distributions by sampling from the gamma distributions
# defined above and then fitting a log normal 
# This likely introduces a bias that could be removed by implementing gamma delays or changing 
# the Huisman et al. code to work with log normals
approximate_lognormal <- function(param_def) {
  estimate_delay(rgamma(1000, shape =  param_def$shape, scale = param_def$scale),
                 bootstraps = 10, bootstrap_samples = 100, max_value = 15)
}
incubation_period <- approximate_lognormal(IncubationParams)
reporting_delay <-  approximate_lognormal(OnsetToCountParams)

# Generation time
# defined as mean 4.8, sd 2.3 in estimateRe (defined in shiny-dailyRe)
# as the Huisman et al approach does not support uncertainty a small placeholder has been used
generation_time <- list(
  mean = 4.8,
  mean_sd = 0.01,
  sd = 2.3,
  sd_sd = 0.01,
  max = 15
)

# set defaults for EpiNow2
# here we use no day of the week effect in line with the Huisman et al
# (as this is also built into their data simulation) approach and a Poisson 
# observation (introduced above to add stochasticity to their simulation). 
estimate_with_epinow2 <- function(...) {
  estimate_infections(reported_cases = simulated_cases, 
                      generation_time = generation_time,
                      delays = delay_opts(incubation_period, reporting_delay),
                      stan = stan_opts(control = list(adapt_delta = 0.95)),
                      obs = obs_opts(family = "poisson", week_effect = FALSE),
                      horizon = 0, verbose = TRUE, ...)
}
# deconvolution approach that has a much faster run time but relatively poor real
# time performance (though comparable to other bayesian deconvolution approaches)
epinow2_Re_deconvolution <- estimate_with_epinow2(rt = NULL)
# generative approach that has a longer run time but better real time estimates
epinow2_Re_generative <- estimate_with_epinow2(rt = rt_opts(prior = list(mean = 2.5, sd = 0.5)))