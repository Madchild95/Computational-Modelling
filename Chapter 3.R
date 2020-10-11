###-----Rethinking: Chapter 3-----###

#loading packages
library(pacman)
p_load(tidyverse,tidybayes,gridExtra)

##Vampire example##
#using bayes theorem
tibble(pr_positive_vampire   = .95,
       pr_positive_mortal    = .01,
       pr_vampire            = .001) %>% 
  mutate(pr_positive         = pr_positive_vampire * pr_vampire + pr_positive_mortal * (1 - pr_vampire)) %>% 
  mutate(pr_vampire_positive = pr_positive_vampire * pr_vampire / pr_positive) %>% 
  glimpse()

#using frequency format
tibble(pr_vampire            = 100 / 100000,
       pr_positive_vampire   = 95 / 100,
       pr_positive_mortal    = 99 / 99900) %>% 
  mutate(pr_positive         = 95 + 999) %>% 
  mutate(pr_vampire_positive = pr_positive_vampire * 100 / pr_positive) %>% 
  glimpse()

##Sampling##
#generating samples of posteriors using grid app.
  # how many grid points would you like?
n <- 1001
n_success <- 6
n_trials  <- 9

(
  d <-
    tibble(p_grid     = seq(from = 0, to = 1, length.out = n),
           # note we're still using a flat uniform prior
           prior      = 1) %>% 
    mutate(likelihood = dbinom(n_success, size = n_trials, prob = p_grid)) %>% 
    mutate(posterior  = (likelihood * prior) / sum(likelihood * prior))
)
    #the results will have the same proportions as the exact posterior density

#saving the rows as a sampling dataset
# how many samples would you like?
n_samples <- 1e4

# make it reproducible
set.seed(3)

samples <-
  d %>% 
  dplyr::sample_n(size = n_samples, weight = posterior, replace = T)

glimpse(samples)

#variable numbering the samples and plotting the zigzagging distribution
samples %>% 
  mutate(sample_number = 1:n()) %>% 
  
  ggplot(aes(x = sample_number, y = p_grid)) +
  geom_line(size = 1/10) +
  labs(x = "sample number",
       y = "proportion of water (p)")

#plotting the density
samples %>% 
  ggplot(aes(x = p_grid)) +
  geom_density(fill = "black") +
  coord_cartesian(xlim = 0:1) +
  xlab("proportion of water (p)")

##Describing and understanding the posterior with the samples generated
#Intervals of defined boundaries
  #what is the probability that the proportion of water is less than some value (here .5)
d %>% 
  filter(p_grid < .5) %>% 
  summarise(sum = sum(posterior))
  #this means that there is a 17% probability that the proportion of water is below 0.5 (meaning half of the globe)
#plot
d %>% 
  ggplot(aes(x = p_grid)) +
  geom_line(aes(y = posterior)) +
  geom_ribbon(data = d %>% filter(p_grid < .5),
              aes(ymin = 0, ymax = posterior)) +
  labs(x = "proportion of water (p)",
       y = "density")

#for the interval between .5 and .75 
samples %>% 
  filter(p_grid > .5 & p_grid < .75) %>% 
  summarise(sum = n() / n_samples)
  #this means that there is a 60% probability that the proportion of water lies between 0.5 and 0.75
#plot
d %>% 
  ggplot(aes(x = p_grid)) +
  geom_line(aes(y = posterior)) +
  # note this next line is the only difference in code from the last plot
  geom_ribbon(data = d %>% filter(p_grid < .75 & p_grid > .5),
              aes(ymin = 0, ymax = posterior)) +
  labs(x = "proportion of water (p)",
       y = "density")

#Intervals of defined mass
  #what is the boundary of the lower 80% of the posterior probability
(q_80 <- quantile(samples$p_grid, prob = .8))
  #this means that the lower 80% posterior probability exists below a parameter value of .76
  #alternative way of getting the 80th percentile
samples %>% 
  select(p_grid) %>% 
  pull() %>% 
  quantile(prob = .8)
  #or
samples %>% 
  summarise(`80th percentile` = quantile(p_grid, p = .8))

#plot
d %>% 
  ggplot(aes(x = p_grid)) +
  geom_line(aes(y = posterior)) +
  geom_ribbon(data = d %>% filter(p_grid < q_80),
              aes(ymin = 0, ymax = posterior)) +
  annotate(geom = "text",
           x = .25, y = .0025,
           label = "lower 80%") +
  labs(x = "proportion of water (p)",
       y = "density")

  #for the middle 80th percentile use
samples %>% 
  summarise(`10th percentile` = quantile(p_grid, p = .1),
            `90th percentile` = quantile(p_grid, p = .9))
  #as a numeric vector use
(q_10_and_90 <- quantile(samples$p_grid, prob = c(.1, .9)))
  
#plot
d %>% 
  ggplot(aes(x = p_grid)) +
  geom_line(aes(y = posterior)) +
  geom_ribbon(data = d %>% filter(p_grid > q_10_and_90[1] & p_grid < q_10_and_90[2]),
              aes(ymin = 0, ymax = posterior)) +
  annotate(geom = "text",
           x = .25, y = .0025,
           label = "middle 80%") +
  labs(x = "proportion of water (p)",
       y = "density")


#Creating a skewed distribution
# here we update the `dbinom()` parameters
n_success <- 3
n_trials  <- 3

# update `d`
d <-
  d %>% 
  mutate(likelihood = dbinom(n_success, size = n_trials, prob = p_grid)) %>% 
  mutate(posterior  = (likelihood * prior) / sum(posterior))

# make the next part reproducible
set.seed(3) #I don't get this!

# here's our new samples tibble
(
  samples <-
    d %>% 
    sample_n(size = n_samples, weight = posterior, replace = T)
)

#looking at the central 50% percentile interval 
quantile(samples$p_grid, prob = c(.25, .75))

  #using the rethinking package
rethinking::PI(samples$p_grid, prob = .5)

  #using tydybayes we can get confidence intervals quick and easy
median_qi(samples$p_grid, .width = .5)
  #...and even multiple types at ones
median_qi(samples$p_grid, .width = c(.5, .8, .99))

#plot
p1 <- d %>% 
  ggplot(aes(x = p_grid)) +
  # check out our sweet `qi()` indexing
  geom_ribbon(data = d %>% filter(p_grid > qi(samples$p_grid, .width = .5)[1] & 
                                    p_grid < qi(samples$p_grid, .width = .5)[2]),
              aes(ymin = 0, ymax = posterior),
              fill = "grey75") +
  geom_line(aes(y = posterior)) +
  labs(subtitle = "50% Percentile Interval",
       x = "proportion of water (p)",
       y = "density")

#Higest posterior density interval (HPDI)
#when the distribution is skewed though, we better use HPDI
rethinking::HPDI(samples$p_grid, prob = .5)

  #which tydybayes also supports
mode_hdi(samples$p_grid, .width = .5) #prefix *_ = type of distribution tendency, _* = type of interval

#plot
p2 <- d %>% 
  ggplot(aes(x = p_grid)) +
  geom_ribbon(data = d %>% filter(p_grid > hdi(samples$p_grid, .width = .5)[1] & 
                                    p_grid < hdi(samples$p_grid, .width = .5)[2]),
              aes(ymin = 0, ymax = posterior),
              fill = "grey75") +
  geom_line(aes(y = posterior)) +
  labs(subtitle = "50% HPDI",
       x = "proportion of water (p)",
       y = "density")

grid.arrange(p1,p2)

#Point estimators
  #MAP estimator#
d %>% 
  arrange(desc(posterior)) %>% 
  slice(1:4)           #selecting the row(s) of interest
  
  #alternatively with select to get rows from the top
d %>% 
  select(posterior) %>% 
  top_n(n = 1)

  #Median and mean estimator#
samples %>% 
  summarise(mean   = mean(p_grid),
            median = median(p_grid))

#plotting the point estimators together
(
  point_estimates <-
    bind_rows(
      samples %>% mean_qi(p_grid),
      samples %>% median_qi(p_grid),
      samples %>% mode_qi(p_grid)
    ) %>% 
    select(p_grid, .point) %>% 
    # these last two columns will help us annotate  
    mutate(x = p_grid + c(-.03, .03, -.03),
           y = c(.1, .25, .4))
)
#the plot
d %>% 
  ggplot(aes(x = p_grid)) +
  geom_ribbon(aes(ymin = 0, ymax = posterior),
              fill = "grey75") +
  geom_vline(xintercept = point_estimates$p_grid) +
  geom_text(data = point_estimates,
            aes(x = x, y = y, label = .point),
            angle = 90) +
  labs(x = "proportion of water (p)",
       y = "density") +
  theme(panel.grid = element_blank())

  #Loss function#
#creating a column of weighted loss from the posterior
d %>% 
  mutate(loss = posterior * abs(0.5 - p_grid)) %>% 
  summarise(`expected loss` = sum(loss))

#makingthe loss function
make_loss <- function(our_d){
  d %>% 
    mutate(loss = posterior * abs(our_d - p_grid)) %>% 
    summarise(weighted_average_loss = sum(loss))
}

(
  l <-
    d %>% 
    select(p_grid) %>% 
    rename(decision = p_grid) %>% 
    mutate(weighted_average_loss = purrr::map(decision, make_loss)) %>% 
    unnest() 
)
# this will help us find the x and y coordinates for the minimum value
min_loss <-
  l %>% 
  filter(weighted_average_loss == min(weighted_average_loss)) %>% 
  as.numeric()

# the plot
lp1 <- l %>%   
  ggplot(aes(x = decision)) +
  geom_ribbon(aes(ymin = 0, ymax = weighted_average_loss),
              fill = "grey75") +
  geom_vline(xintercept = min_loss[1], color = "red", linetype = 3) +
  geom_hline(yintercept = min_loss[2], color = "red", linetype = 3) +
  ylab("expected proportional loss") +
  theme(panel.grid = element_blank())
  #this plot suggests the median as the point estimator of least loss

  #but if we use the quadradic loss function instead, the suggestion looks different
# ammend our loss function
make_loss <- function(our_d){
  d %>% 
    mutate(loss = posterior * (our_d - p_grid)^2) %>% #here it is made quadradic
    summarise(weighted_average_loss = sum(loss))
}

# remake our `l` data
l <-
  d %>% 
  select(p_grid) %>% 
  rename(decision = p_grid) %>% 
  mutate(weighted_average_loss = purrr::map(decision, make_loss)) %>% 
  unnest()

# update to the new minimum loss coordinates
min_loss <-
  l %>% 
  filter(weighted_average_loss == min(weighted_average_loss)) %>% 
  as.numeric()

# update the plot
lp2 <- l %>%   
  ggplot(aes(x = decision)) +
  geom_ribbon(aes(ymin = 0, ymax = weighted_average_loss),
              fill = "grey75") +
  geom_vline(xintercept = min_loss[1], color = "red", linetype = 3) +
  geom_hline(yintercept = min_loss[2], color = "red", linetype = 3) +
  ylab("expected proportional loss") +
  theme(panel.grid = element_blank())
  #now the suggested point estimator is that of the posterior mean intead
grid.arrange(lp1,lp2)
  
