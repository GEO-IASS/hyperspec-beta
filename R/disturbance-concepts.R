library(tidyverse)
library(splines)
library(forcats)
# Simulating a time series of disturbance for one pixel -------------------
min_year <- 1
max_year <- 300
year <- min_year:max_year

# low-level random perterbations
X_bs <- bs(year, degree = 50, intercept = TRUE) %>%
  as.matrix

beta_bs <- rnorm(ncol(X_bs), sd = 2)

smooth_errors <- tibble(err = plogis(X_bs %*% beta_bs %>% c), 
                        year = year)



# disturbance happens in 1990
disturbance_year <- 50
length_disturbance <- 20
initial_state <- 2
disturbed_state <- -4
mu <- c(rep(initial_state, length(min_year:(disturbance_year))), 
        seq(initial_state, disturbed_state, length.out = length_disturbance)) # values following disturbance


years_recovered <- 100
mu_recov <- c(seq(disturbed_state, initial_state, 
                  length.out = length(year) - length(mu) - years_recovered), 
              rep(initial_state, years_recovered))
mu_stable <- rep(disturbed_state, length.out = length(year) - length(mu))
plot(c(mu, mu_recov))
points(c(mu, mu_stable), col = 'red')

sd_y <- .05
sd_error <- .01

d_pre <- tibble(year = 1:length(mu), 
                mu = mu) %>%
  left_join(smooth_errors) %>%
  mutate(y = rnorm(n(), mu + err, sd = sd_y), 
         y_error = exp(rnorm(n(), 0, sd_error)),
         p = plogis(y), 
         lo = plogis(y - y_error), 
         hi = plogis(y + y_error)) %>%
  mutate(period = ifelse(mu == initial_state, 
         'Initial state', 
         'Disturbance underway'), 
         is_future = FALSE)


# time of the future for predictions
t_future <- 0.85 * max_year %>% floor
error_increase <- .02

d_recov <- tibble(year = (length(mu) + 1):max_year,
                  mu = mu_recov) %>%
  left_join(smooth_errors) %>%
  mutate(y = rnorm(n(), mu + err, sd = sd_y),
         is_future = year > t_future,
         y_error = exp(rnorm(n(), 0, sd_error)) + 
           is_future * error_increase * (year - t_future),
         p = plogis(y), 
         lo = plogis(y - y_error), 
         hi = plogis(y + y_error)) %>%
  mutate(period = ifelse(mu == initial_state, 
                         'Recovered to initial state', 
                         'Recovery underway'))


d_stable <- tibble(year = (length(mu) + 1):max_year,
                   mu = mu_stable) %>%
  left_join(smooth_errors) %>%
  mutate(y = rnorm(n(), mu + err, sd = sd_y),
         is_future = year > t_future,
         y_error = exp(rnorm(n(), 0, sd_error)) + 
           is_future * error_increase *  (year - t_future),
         p = plogis(y), 
         lo = plogis(y - y_error), 
         hi = plogis(y + y_error)) %>%
  mutate(period = 'Not recovering')

full_join(d_pre, d_recov) %>%
  full_join(d_stable) %>% 
  mutate(period2 = ifelse(is_future, 'Future predictions', period)) %>%
  ggplot(aes(year, p, ymin = lo, ymax = hi, 
             fill = fct_reorder2(period2, p, year, .desc = FALSE),
             color = fct_reorder2(period2, p, year, .desc = FALSE),
             group = interaction(period, period2))) + 
  geom_line() + 
  geom_ribbon(alpha = .8, na.rm = TRUE, color = NA) + 
  ylim(0, 1) + 
  theme_minimal() + 
  xlab('Time') + 
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank()) + 
  ylab('State variable') + 
  scale_fill_calc('') + 
  scale_color_calc('')
ggsave('state-concept.pdf', width = 10, height = 4)

