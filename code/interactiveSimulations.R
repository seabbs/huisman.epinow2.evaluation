###########################################################
## interactiveSimulations.R
## author: J.S. Huisman
## Adapted by: Sam Abbott
###########################################################

library(tidyverse)
library(lubridate)
library(viridis)
library(EpiNow2)
library(data.table)

plot_path = 'figures'

source('code/generateSimulations.R')
###########################################################
###### Simulate observations and Estimate Re ######

# 6 times, 3 plateaus; 10 times, 5 plateaus
shift_times = c(0, 15, 50, 60, 80, 100, 110, 120, 130, 160)
R_levels = c(3.5, 0.5, 1.2, 0.95, 1.1)

IncubationParams <- getGammaParams(meanParam = 5.3, sdParam = 3.2)
OnsetToCountParams = getGammaParams(4.5, 4.9)

simulation <- simulateTS(shift_times, R_levels,
                         IncubationParams, OnsetToCountParams, init_infection = 10,
                         noise = list(), smooth_R = FALSE,
                         timevarying = FALSE)

estimatedInfections <- estimateInfectionTS(simulation, IncubationParams, OnsetToCountParams,
                                           smooth_param = TRUE, fixed_shift = FALSE,
                                           timevarying = FALSE, n_boot = 100)

estimatedRe <- estimateReTS(estimatedInfections, delay = 0)

# Add EpiNow2 estimates ---------------------------------------------------

# set up cores
options(mc.cores = 4)

# convert data into expected format
obs <- as.data.table(simulation)
obs <- obs[.(date, confirm = observations)]

# estimate delays from gamma samples
# EpiNow2 is designed to work with only lognormal delays currently 
# so here we approximate lognormal distributions by sampling from the gamma distributions
# defined above and then fitting a log normal 
# This likely introduces a bias that could be removed by implementing gamma delays or changing 
# the Huisman et al. code to work with log normals
approximate_lognormal <- function(param_def) {
  estimate_delay(rgamma(1000, shape =  param_def$shape, scale = param_def$scale),
                 bootstraps = 1, bootstrap_samples = 1000, max_value = 30)
}
incubation_period <- approximate_lognormal(IncubationParams)
reporting_delay <-  approximate_lognormal(OnsetToCountParams)

# get generation time
# defined as Gamma(shape 2.73, scale =1.39) in code/generateSimulations.R
generation_time <- list(
  mean = 
  mean_sd = 0.01,
  sd = 
)
# define delays in the format expected by EpNow2
estimate_with_EpiNow2 <- function(...) {
  estimate_infections(obs, generation_time = 
                      delays = delay_opts(incubation_period, reporting_delay),
                      horizon = 0, verbose = TRUE)
}
epinow2_Re_generative <- estimate_with_EpiNow2()
epinow2_Re_deconvolution <- estimate_with_EpiNow2(rt = NULL)
  


###### Rearrange data for plotting ######

longSim <- simulation %>%
  pivot_longer(cols = c(Re, infections, observations),
               names_to = 'type') %>%
  mutate(type = factor(R.utils::capitalize(type), levels = c('Re', 'Infections', 'Observations')),
         source_type = 'simulation',
         plot_row = ifelse(type == 'Re', 'Re', 'Cases'),
         plot_row = factor(plot_row, levels = c('Re', 'Cases')) )

longInfections <- estimatedInfections %>%
  dplyr::select(c(date, replicate, value)) %>% 
  group_by(date) %>%
  summarize(
    median_val = median(value),
    high_quant = quantile(value, probs=0.975, na.rm=T),
    low_quant = quantile(value, probs=0.025, na.rm=T),
    .groups = "keep"
  ) %>%
  ungroup() %>%
  mutate(type = factor('Infections', levels = c('Re', 'Infections', 'Observations'), ordered = TRUE),
         source_type = 'estimation',
         plot_row = factor('Cases', levels = c('Re', 'Cases'), ordered = TRUE))

longRe <- cleanReTSestimate(estimatedRe) %>%
  dplyr::select(-c(country, region, source, data_type,estimate_type)) %>% 
  mutate(type = factor('Re', levels = c('Re', 'Infections', 'Observations'), ordered = TRUE),
         source_type = 'estimation',
         plot_row = factor('Re', levels = c('Re', 'Cases')) )

###### Plot the assumed and estimated values ######

ggplot() +
  geom_line(data = longSim, aes(x = date, y = value, colour = type), size = 2) +
  geom_ribbon(data = longRe %>% filter(median_R_mean < 4), aes(x = date, ymin = median_R_lowHPD,
                                 ymax = median_R_highHPD), alpha = 0.7) +
  geom_line(data = longRe %>% filter(median_R_mean < 4), aes(x = date, y = median_R_mean), size=1.1) +
  geom_ribbon(data = longInfections, aes(x = date, ymin = low_quant,
                                         ymax = high_quant), alpha = 0.7) +
  geom_line(data = longInfections, aes(x = date, y = median_val), size=1.1) +
  labs( x = 'Date', colour = 'Simulated') +
  scale_colour_viridis(direction = -1, begin = 0.3, discrete = T) + 
  facet_grid(rows = vars(plot_row), scale = 'free', switch = 'y') +
  theme_minimal() +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    strip.text.y = element_text(size = 25),
    strip.placement = 'outside',
    axis.text.y= element_text(size=20),
    axis.text.x= element_text(size=20),
    axis.title.y =  element_blank(), 
    axis.title.x =  element_text(size=25),
    legend.title = element_text(size=25),
    legend.text = element_text(size=20),
    legend.position = c(0.8, 0.8)
  )

plotPath <- paste0(plot_path, "/Fig1.pdf")
ggsave(plotPath, width = 8, height = 8)
