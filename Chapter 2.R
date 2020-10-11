###-----Rethinking: Chapter 2-----###

#packages
library(pacman)
p_load(brms, gridExtra, tidyverse)

##Model building: globe tossing example##

#Saving globe tossing in a tibble
(d <- tibble(toss = c("w", "l", "w", "w", "w", "l", "w", "l", "w")))
#adding n trials and n succes
(
  d <-
    d %>% 
    mutate(n_trials  = 1:9,
           n_success = cumsum(toss == "w"))
)

##Components of model##
#Likelihood
dbinom(x = 6, size = 9, prob = .7)
#changing prob
tibble(prob = seq(from = 0, to = 1, by = .01)) %>% 
  ggplot(aes(x = prob,
             y = dbinom(x = 6, size = 9, prob = prob))) +
  geom_line() +
  labs(x = "probability",
       y = "binomial likelihood") +
  theme(panel.grid = element_blank())

#Prior
tibble(a = 0,
       b = c(1, 1.5, 2, 3, 9)) %>% 
  mutate(prob = 1 / (b - a))
#plotting the prior probability
tibble(a = 0,
       b = c(1, 1.5, 2, 3, 9)) %>% 
  expand(nesting(a, b), parameter_space = seq(from = 0, to = 9, length.out = 500)) %>% 
  mutate(prob = dunif(parameter_space, a, b),
         b = str_c("b = ", b)) %>% 
  
  ggplot(aes(x = parameter_space, ymin = 0, ymax = prob)) +
  geom_ribbon() +
  scale_x_continuous(breaks = c(0, 1:3, 9)) +
  scale_y_continuous(breaks = c(0, 1/9, 1/3, 1/2, 2/3, 1),
                     labels = c("0", "1/9", "1/3", "1/2", "2/3", "1")) +
  theme(panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_blank()) +
  facet_wrap(~b, ncol = 5)

#Estimating the posterior
sequence_length <- 1e3

d <-
  tibble(probability = seq(from = 0, to = 1, length.out = sequence_length)) %>% 
  expand(probability, row = c("flat", "stepped", "Laplace")) %>% 
  arrange(row, probability) %>% 
  mutate(prior = ifelse(row == "flat", 1,
                        ifelse(row == "stepped", rep(0:1, each = sequence_length / 2),
                               exp(-abs(probability - .5) / .25) / ( 2 * .25))),
         likelihood = dbinom(x = 6, size = 9, prob = probability)) %>% 
  group_by(row) %>% 
  mutate(posterior = prior * likelihood / sum(prior * likelihood)) %>% 
  gather(key, value, -probability, -row) %>% 
  ungroup() %>% 
  mutate(key = factor(key, levels = c("prior", "likelihood", "posterior")),
         row = factor(row, levels = c("flat", "stepped", "Laplace"))) 
p1 <-
  d %>%
  filter(key == "prior") %>% 
  ggplot(aes(x = probability, y = value)) +
  geom_line() +
  scale_x_continuous(NULL, breaks = c(0, .5, 1)) +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(subtitle = "prior") +
  theme(panel.grid       = element_blank(),
        strip.background = element_blank(),
        strip.text       = element_blank()) +
  facet_wrap(row ~ ., scales = "free_y", ncol = 1)

p2 <-
  d %>%
  filter(key == "likelihood") %>% 
  ggplot(aes(x = probability, y = value)) +
  geom_line() +
  scale_x_continuous(NULL, breaks = c(0, .5, 1)) +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(subtitle = "likelihood") +
  theme(panel.grid       = element_blank(),
        strip.background = element_blank(),
        strip.text       = element_blank()) +
  facet_wrap(row ~ ., scales = "free_y", ncol = 1)

p3 <-
  d %>%
  filter(key == "posterior") %>% 
  ggplot(aes(x = probability, y = value)) +
  geom_line() +
  scale_x_continuous(NULL, breaks = c(0, .5, 1)) +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(subtitle = "posterior") +
  theme(panel.grid       = element_blank(),
        strip.background = element_blank(),
        strip.text       = element_blank()) +
  facet_wrap(row ~ ., scales = "free_y", ncol = 1)

grid.arrange(p1, p2, p3, ncol = 3)

#Grid approximation
(d <-
    tibble(p_grid            = seq(from = 0, to = 1, length.out = 20),  # define grid
           prior             = 1) %>%                                   # define prior
    mutate(likelihood      = dbinom(6, size = 9, prob = p_grid)) %>%  # compute likelihood at each value in grid
    mutate(unstd_posterior = likelihood * prior) %>%                  # compute product of likelihood and prior
    mutate(posterior       = unstd_posterior / sum(unstd_posterior))  # standardize the posterior, so it sums to 1
)
d %>% 
  ggplot(aes(x = p_grid, y = posterior)) +
  geom_point() +
  geom_line() +
  labs(subtitle = "20 points",
       x = "probability of water",
       y = "posterior probability") +
  theme(panel.grid = element_blank())
#5 point grid
tibble(p_grid            = seq(from = 0, to = 1, length.out = 5),
       prior             = 1) %>%
  mutate(likelihood      = dbinom(6, size = 9, prob = p_grid)) %>%
  mutate(unstd_posterior = likelihood * prior) %>%
  mutate(posterior       = unstd_posterior / sum(unstd_posterior)) %>% 
  
  ggplot(aes(x = p_grid, y = posterior)) +
  geom_point() +
  geom_line() +
  labs(subtitle = "5 points",
       x = "probability of water",
       y = "posterior probability") +
  theme(panel.grid = element_blank())

#Quadridic approximation (Gaussian approximation)
library(rethinking)

globe_qa <-
  rethinking::map(
    alist(
      w ~ dbinom(9, p),  # binomial likelihood
      p ~ dunif(0, 1)    # uniform prior
    ), 
    data = list(w = 6))

# display summary of quadratic approximation
precis(globe_qa)
#Read: assuming that the posterior is Gaussian, it is maximized at .67 and it's standard deviation is .16

#showing how uncertainty decreases with sample size in quadradic
globe_qa_18 <-
  rethinking::map(
    alist(
      w ~ dbinom(9 * 2, p),
      p ~ dunif(0, 1)
    ), data = list(w = 6  *2))
precis(globe_qa_18)

globe_qa_36 <-
  rethinking::map(
    alist(
      w ~ dbinom(9 * 4, p),
      p ~ dunif(0, 1)
    ), data = list(w = 6 * 4))
precis(globe_qa_36)

#figure 2.8: comparing quadridic app. to analytical (grid?) app.
n_grid <- 100

tibble(p_grid                  = seq(from = 0, to = 1, length.out = n_grid) %>% rep(., times = 3),
       prior                   = 1,
       w                       = rep(c(6, 12, 24), each = n_grid),
       n                       = rep(c(9, 18, 36), each = n_grid),
       m                       = .67,
       s                       = rep(c(.16, .11, .08), each = n_grid)) %>%
  mutate(likelihood            = dbinom(w, size = n, prob = p_grid)) %>%
  mutate(unstd_grid_posterior  = likelihood * prior,
         unstd_quad_posterior  = dnorm(p_grid, m, s)) %>%
  group_by(w) %>% 
  mutate(grid_posterior        = unstd_grid_posterior / sum(unstd_grid_posterior),
         quad_posterior        = unstd_quad_posterior / sum(unstd_quad_posterior),
         n = str_c("n = ", n)) %>% 
  mutate(n = factor(n, levels = c("n = 9", "n = 18", "n = 36"))) %>% 
  
  ggplot(aes(x = p_grid)) +
  geom_line(aes(y = grid_posterior)) +
  geom_line(aes(y = quad_posterior),
            color = "grey50") +
  labs(x = "proportion water",
       y = "density") +
  theme(panel.grid = element_blank()) +
  facet_wrap(~n, scales = "free")

#MCMC model fit from brms
globe_qa_brms <-
  brm(data = list(w = 24), 
      family = binomial(link = "identity"),
      w | trials(36) ~ 1,
      prior(beta(1, 1), class = Intercept),
      iter = 4000, warmup = 1000,
      control = list(adapt_delta = .9),
      seed = 4)

print(globe_qa_brms)

#density plot of the model
posterior_samples(globe_qa_brms) %>% 
  mutate(n = "n = 36") %>%
  
  ggplot(aes(x = b_Intercept)) +
  geom_density(fill = "black") +
  labs(x = "proportion water") +
  xlim(0, 1) +
  theme(panel.grid = element_blank()) +
  facet_wrap(~n)
