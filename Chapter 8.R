###-----Rethinking: Chapter 8-----###

#packages
library(pacman)
p_load(tidyverse,
       brms, #also known as Bromance, Bayesian models with Tidyverse language
       rethinking, #McElreath's package
       ggrepel, #for texts in ggplot
       rcartocolor, #a color package used to plots in this chapter
       broom, #provides an array of convenience functions to convert statistical analysis summaries into tidy data objects
       gridExtra, #to get the plots together
       tidybayes,
       ggthemes, #to get the pander theme ;-)
       hrbrthemes, #to get the ipsum theme used in this chapter
       GGally, #to get ggpairs
       bayesplot #to get chain plot
)

##Simulating King Markov's journey/Metropolis sampling
num_weeks <- 1e5
positions <- rep(0,num_weeks)
current <- 10
#for loop
for (i in 1:num_weeks){
  positions[i] <- current
  
  proposal <- current + sample(c(-1,1), size = 1)
  
  if (proposal < 1) proposal <- 10
  if (proposal > 10) proposal <- 1
  
  prob_move <- proposal/current
  current <- ifelse(runif(1) < prob_move, proposal, current)
}

#Making the first plot work from Rbasic
week <- 1:100
island <- positions[1:100]
s <- tibble(island,week)
plot(island~week, s)
title(main = "Behold: The Metropolis algorithm in action!",
      sub = "The dots show the king's path over the first 100 weeks.")

#Getting help from brms to make the barplot work..
tibble(week   = 1:1e5,
       island = positions) %>%
  mutate(island = factor(island)) %>%
  
  ggplot(aes(x = island)) +
  geom_bar() +
  labs(title    = "Old Metropolis shines in the long run.",
       subtitle = "Sure enough, the time the king spent on each island was\nproportional to its population size.") +
  theme_pander()

##Back to the rugged example##

data(rugged)
d <- rugged
rm(rugged) #removes an item from the environment

#getting only nations with outcome variables of interest
d <- 
  d %>%
  mutate(log_gdp = log(rgdppc_2000))
#removing na's
dd <-
  d %>%
  drop_na(rgdppc_2000)

#fitting the interaction model  
b8.1 <-
  brm(data = dd, family = gaussian,
      log_gdp ~ 1 + rugged + cont_africa + rugged:cont_africa,
      prior = c(prior(normal(0, 100), class = Intercept),
                prior(normal(0, 10), class = b),
                prior(cauchy(0, 2), class = sigma)),
      seed = 8)
print(b8.1)
#try to plot the interaction
plot(conditional_effects(b8.1, 
                         effects = "rugged:cont_africa"),
     points = T)

#visualization of posterior distribution
post <- posterior_samples(b8.1, add_chain = T) #to check chains we need to include the add_chain 
str(post)

#getting the matrix
pairs(b8.1,
      off_diag_args = list(size = 1/5, alpha = 1/5))

#another nice way
post %>%
  select(b_Intercept:sigma) %>%
  ggpairs()

#and costumized
my_diag <- function(data, mapping, ...){
  ggplot(data = data, mapping = mapping) + 
    geom_density(fill = "grey50")
}

my_lower <- function(data, mapping, ...){
  ggplot(data = data, mapping = mapping) + 
    geom_point(shape = 1, size = 1/2, alpha = 1/6)
}

post %>%
  select(b_Intercept:sigma) %>%
  
  ggpairs(diag  = list(continuous = my_diag),
          lower = list(continuous = my_lower)) +
  labs(subtitle = "My custom pairs plot") +
  theme_pander()

#asessing IC of the model
waic(b8.1)
(l_b8.1 <- loo(b8.1))

#checking the chains
plot(b8.1)

#costumizing trace plot
mcmc_trace(post[, c(1:5, 8)],  # we need to include column 8 because it contains the chain info 
           facet_args = list(ncol = 3), 
           size = .15) +
  labs(title = "My custom trace plots") +
  scale_color_ipsum() +
  theme_ipsum() +
  theme(legend.position = c(.95, .2))

#riccardo's chain check code
color_scheme_set("viridis")
mcmc_trace(b8.1, pars = "b_Intercept") + theme_classic()
#chain check 2
mcmc_rank_hist(b8.1, pars = "b_Intercept") + theme_classic()
mcmc_rank_overlay(b8.1, pars = "b_Intercept") + theme_classic()
