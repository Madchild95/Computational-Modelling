###-----Rethinking: Chapter 5-----###

#packages
library(pacman)
p_load(tidyverse,brms,rethinking,ggrepel)

##--WaffleDivorce example--##

#the first plot made with brms
d %>%
  ggplot(aes(x = WaffleHouses/Population, y = Divorce)) +
  stat_smooth(method = "lm", fullrange = T, size = 1/2,
              color = "firebrick4", fill = "firebrick", alpha = 1/5) +
  geom_point(size = 1.5, color = "firebrick4", alpha = 1/2) +
  geom_text_repel(data = d %>% filter(Loc %in% c("ME", "OK", "AR", "AL", "GA", "SC", "NJ")),  
                  aes(label = Loc), 
                  size = 3, seed = 1042) +  # this makes it reproducible
  scale_x_continuous("Waffle Houses per million", limits = c(0, 55)) +
  coord_cartesian(xlim = 0:50, ylim = 5:15) +
  ylab("Divorce rate") +
  theme_bw() +
  theme(panel.grid = element_blank())  

#a cool ass plot of the states!!!
install.packages("devtools")
devtools::install_github("wmurphyrd/fiftystater")
library(fiftystater)
d %>% 
  # first we'll standardize the three variables to put them all on the same scale
  mutate(Divorce_z           = (Divorce - mean(Divorce))                     / sd(Divorce),
         MedianAgeMarriage_z = (MedianAgeMarriage - mean(MedianAgeMarriage)) / sd(MedianAgeMarriage),
         Marriage_z          = (Marriage - mean(Marriage))                   / sd(Marriage),
         # need to make the state names lowercase to match with the map data
         Location            = str_to_lower(Location)) %>% 
  # here we select the relevant variables and put them in the long format to facet with `facet_wrap()`
  select(Divorce_z:Marriage_z, Location) %>% 
  gather(key, value, -Location) %>% 
  
  ggplot(aes(map_id = Location)) +
  geom_map(aes(fill = value), map = fifty_states, 
           color = "firebrick", size = 1/15) +
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  scale_x_continuous(NULL, breaks = NULL) +
  scale_y_continuous(NULL, breaks = NULL) +
  scale_fill_gradient(low = "#f8eaea", high = "firebrick4") +
  coord_map() +
  theme_bw() +
  theme(panel.grid       = element_blank(),
        legend.position  = "none",
        strip.background = element_rect(fill = "transparent", color = "transparent")) +
  facet_wrap(~key)

##Spurious associations

#...rethinking...
data("WaffleDivorce")
d <- WaffleDivorce

#standadize predictor: mean age marriage
d$MedianAgeMarriage.s <- (d$MedianAgeMarriage-mean(d$MedianAgeMarriage))/sd(d$MedianAgeMarriage)

#fit linear regression for median age at marriage
m5.1 <- map(
  alist(
    Divorce ~ dnorm(mu,sigma),
    mu <- a +bA * MedianAgeMarriage.s,
    a ~ dnorm(10,10),
    bA ~ dnorm(0,1),
    sigma ~ dunif(0,10)
  ), data = d
)
precis(m5.1)
#percentile interval of mean
MAM.seq <- seq(from = -3, to = 3.5, length.out = 30)
mu <- link(m5.1, data = data.frame(MedianAgeMarriage.s=MAM.seq))
mu.PI <- apply(mu, 2, PI)

#plot it all
plot(Divorce~MedianAgeMarriage.s, data = d, col = rangi2)
abline(m5.1)
shade(mu.PI,MAM.seq)

#making the model for marriage rate
#standadize predictor: mean age marriage
d$Marriage.s <- (d$Marriage-mean(d$Marriage))/sd(d$Marriage)

#fit linear regression
m5.2 <- map(
  alist(
    Divorce ~ dnorm(mu,sigma),
    mu <- a +bA * Marriage.s,
    a ~ dnorm(10,10),
    bA ~ dnorm(0,1),
    sigma ~ dunif(0,10)
  ), data = d
)
precis(m5.2)
#percentile interval of mean
MAM.seq <- seq(from = -3, to = 3.5, length.out = 30)
mu <- link(m5.2, data = data.frame(Marriage.s=MAM.seq))
mu.PI <- apply(mu, 2, PI)

#plot it all
plot(Divorce~Marriage.s, data = d, col = rangi2)
abline(m5.2)
shade(mu.PI,MAM.seq)

#...brms...
##Model 1
#standadizing predictor median age of marriage
d <-
  d %>%
  mutate(MedianAgeMarriage_s = (MedianAgeMarriage - mean(MedianAgeMarriage)) /
           sd(MedianAgeMarriage))
#fitting univariable model
b5.1 <- 
  brm(data = d, family = gaussian,
      Divorce ~ 1 + MedianAgeMarriage_s,
      prior = c(prior(normal(10, 10), class = Intercept),
                prior(normal(0, 1), class = b),
                prior(uniform(0, 10), class = sigma)),
      iter = 2000, warmup = 500, chains = 4, cores = 4,
      seed = 5)
print(b5.1)
# define the range of `MedianAgeMarriage_s` values we'd like to feed into `fitted()`
nd <- tibble(MedianAgeMarriage_s = seq(from = -3, to = 3.5, length.out = 30))

# now use `fitted()` to get the model-implied trajectories
f <- 
  fitted(b5.1, newdata = nd) %>%
  as_tibble() %>%
  # tack the `nd` data onto the `fitted()` results
  bind_cols(nd)

# plot
ggplot(data = f, 
       aes(x = MedianAgeMarriage_s, y = Estimate)) +
  geom_smooth(aes(ymin = Q2.5, ymax = Q97.5),
              stat = "identity",
              fill = "firebrick", color = "firebrick4", alpha = 1/5, size = 1/4) +
  geom_point(data = d, 
             aes(y = Divorce), 
             size = 2, color = "firebrick4") +
  ylab("Divorce") +
  coord_cartesian(xlim = range(d$MedianAgeMarriage_s), 
                  ylim = range(d$Divorce)) +
  theme_bw() +
  theme(panel.grid = element_blank())

##Model 2
#standadizing marriage rate
d <-
  d %>%
  mutate(Marriage_s = (Marriage - mean(Marriage)) / sd(Marriage))

#fitting 2nd model
b5.2 <- 
  brm(data = d, family = gaussian,
      Divorce ~ 1 + Marriage_s,
      prior = c(prior(normal(10, 10), class = Intercept),
                prior(normal(0, 1), class = b),
                prior(uniform(0, 10), class = sigma)),
      iter = 2000, warmup = 500, chains = 4, cores = 4,
      seed = 5)

#wangle and plot
nd <- tibble(Marriage_s = seq(from = -2.5, to = 3.5, length.out = 30))

f <- 
  fitted(b5.2, newdata = nd) %>%
  as_tibble() %>%
  bind_cols(nd)

ggplot(data = f, 
       aes(x = Marriage_s, y = Estimate)) +
  geom_smooth(aes(ymin = Q2.5, ymax = Q97.5),
              stat = "identity",
              fill = "firebrick", color = "firebrick4", alpha = 1/5, size = 1/4) +
  geom_point(data = d, 
             aes(y = Divorce), 
             size = 2, color = "firebrick4") +
  coord_cartesian(xlim = range(d$Marriage_s), 
                  ylim = range(d$Divorce)) +
  ylab("Divorce") +
  theme_bw() +
  theme(panel.grid = element_blank())              

#the question we want to answer: "What is the predictive value of a variable, once I already know all of the other predictor variables?"
##Fitting the multivariate model with both predictors
#...rethinking...
#standadize divorce
d <-
  d %>%
  mutate(Divorce_s = (Divorce - mean(Divorce)) / sd(Divorce))

m5.3 <- map(
  alist(
    Divorce_s ~ dnorm(mu,sigma),
    mu <- a +bA * MedianAgeMarriage.s + bR * Marriage.s,
    a ~ dnorm(0,0.2),
    bA ~ dnorm(0,0.5),
    bR ~ dnorm(0,0.5),
    sigma ~ dexp(1)
  ), data = d
)

precis(m5.3)
#this means that there is little value added to the prediction of the divorce rate of a state in knowing the marriage rate, once we already know the median marriage age

#...brms...
b5.3 <- 
  brm(data = d, family = gaussian,
      Divorce ~ 1 + Marriage_s + MedianAgeMarriage_s,
      prior = c(prior(normal(10, 10), class = Intercept),
                prior(normal(0, 1), class = b),
                prior(uniform(0, 10), class = sigma)),
      iter = 2000, warmup = 500, chains = 4, cores = 4,
      seed = 5)
#with the standadized divorce
b5.3b <- 
  brm(data = d, family = gaussian,
      Divorce_s ~ 1 + Marriage_s + MedianAgeMarriage_s,
      prior = c(prior(normal(0, 0.2), class = Intercept),
                prior(normal(0, 0.5), class = b),
                prior(exponential(1), class = sigma)),
      iter = 2000, warmup = 500, chains = 4, cores = 4,
      seed = 5)
#convenient way of making a coefficient plot
stanplot(b5.3)

#a more fancy way is using tidybayes
install.packages("bayesplot", dependencies = T)
library(bayesplot)

post <- posterior_samples(b5.3)

color_scheme_set("red")
mcmc_intervals(post[, 1:4], 
               prob = .5,
               point_est = "median") +
  labs(title = "My fancy bayesplot-based coefficient plot") +
  theme(axis.text.y  = element_text(hjust = 0),
        axis.line.x  = element_line(size = 1/4),
        axis.line.y  = element_blank(),
        axis.ticks.y = element_blank())

#a third and fancy way using ggplot
library(tidybayes)
post %>% 
  select(-lp__) %>% 
  gather() %>% 
  
  ggplot(aes(x = value, y = reorder(key, value))) +  # note how we used `reorder()` to arrange the coefficients
  geom_vline(xintercept = 0, color = "firebrick4", alpha = 1/10) +
  stat_pointintervalh(point_interval = mode_hdi, .width = .95, 
                      size = 3/4, color = "firebrick4") +
  labs(title = "My tidybayes-based coefficient plot",
       x = NULL, y = NULL) +
  theme_bw() +
  theme(panel.grid   = element_blank(),
        panel.grid.major.y = element_line(color = alpha("firebrick4", 1/4), linetype = 3),
        axis.text.y  = element_text(hjust = 0),
        axis.ticks.y = element_blank())
#Or in McElreath's words: “Once we know median age at marriage for a State, there is little or no additive predictive power in also knowing the rate of marriage in that State” 

##Plotting posteriors of multivariate models
#...brms...
#Predictor residual plot
#we have to first make a model where marriage rate will predict marriage age
b5.4 <- 
  brm(data = d, family = gaussian,
      Marriage_s ~ 1 + MedianAgeMarriage_s,
      prior = c(prior(normal(0, 10), class = Intercept),
                prior(normal(0, 1), class = b),
                prior(uniform(0, 10), class = sigma)),
      iter = 2000, warmup = 500, chains = 4, cores = 4,
      seed = 5)
#we then produce the expected values of each state
f <- 
  fitted(b5.4) %>%
  as_tibble() %>%
  bind_cols(d)

head(f)
#then we can plot the residuals
f %>% 
  
  ggplot(aes(x = MedianAgeMarriage_s, y = Marriage_s)) +
  geom_point(size = 2, shape = 1, color = "firebrick4") +
  geom_segment(aes(xend = MedianAgeMarriage_s, yend = Estimate), 
               size = 1/4) +
  geom_line(aes(y = Estimate), 
            color = "firebrick4") +
  coord_cartesian(ylim = range(d$Marriage_s)) +
  theme_bw() +
  theme(panel.grid = element_blank())     
#the gray lines depict the residuals where those above the line have a higher rate of marriage than expected and those below have a lower rate than expected

#and now we can make the predictor residual plot
r <- 
  residuals(b5.4) %>%
  # to use this in ggplot2, we need to make it a tibble or data frame
  as_tibble() %>% 
  bind_cols(d)

# for the annotation at the top
text <-
  tibble(Estimate = c(- 0.5, 0.5),
         Divorce = 14.1,
         label = c("slower", "faster"))

# plot
r %>% 
  ggplot(aes(x = Estimate, y = Divorce)) +
  stat_smooth(method = "lm", fullrange = T,
              color = "firebrick4", fill = "firebrick4", 
              alpha = 1/5, size = 1/2) +
  geom_vline(xintercept = 0, linetype = 2, color = "grey50") +
  geom_point(size = 2, color = "firebrick4", alpha = 2/3) +
  geom_text(data = text,
            aes(label = label)) +
  scale_x_continuous("Marriage rate residuals", limits = c(-2, 2)) +
  coord_cartesian(xlim = range(r$Estimate),
                  ylim = c(6, 14.1)) +
  theme_bw() +
  theme(panel.grid = element_blank())
#This shows that states with fast marriage rate have about the same divorce rate as states with slow marriage rate

#we will do the same for age of marriage
#we now fit the model where marriage age is predicted by marriage rate
b5.4b <- 
  brm(data = d, family = gaussian,
      MedianAgeMarriage_s ~ 1 + Marriage_s,
      prior = c(prior(normal(0, 10), class = Intercept),
                prior(normal(0, 1), class = b),
                prior(uniform(0, 10), class = sigma)),
      iter = 2000, warmup = 500, chains = 4, cores = 4,
      seed = 5)
#we then produce the expected values of each state
f <- 
  fitted(b5.4b) %>%
  as_tibble() %>%
  bind_cols(d)

head(f)
#then we can plot the residuals
f %>% 
  
  ggplot(aes(x = Marriage_s, y = MedianAgeMarriage_s)) +
  geom_point(size = 2, shape = 1, color = "firebrick4") +
  geom_segment(aes(xend = Marriage_s, yend = Estimate), 
               size = 1/4) +
  geom_line(aes(y = Estimate), 
            color = "firebrick4") +
  coord_cartesian(ylim = range(d$MedianAgeMarriage_s)) +
  theme_bw() +
  theme(panel.grid = element_blank())     
#now some more data processing, residual generation and finally the plot
text <-
  tibble(Estimate = c(- 0.7, 0.5),
         Divorce  = 14.1,
         label    = c("younger", "older"))

residuals(b5.4b) %>%
  as_tibble() %>%
  bind_cols(d) %>% 
  
  ggplot(aes(x = Estimate, y = Divorce)) +
  stat_smooth(method = "lm", fullrange = T,
              color = "firebrick4", fill = "firebrick4", 
              alpha = 1/5, size = 1/2) +
  geom_vline(xintercept = 0, linetype = 2, color = "grey50") +
  geom_point(size = 2, color = "firebrick4", alpha = 2/3) +
  geom_text(data = text,
            aes(label = label)) +
  scale_x_continuous("Age of marriage residuals", limits = c(-2, 3)) +
  coord_cartesian(xlim = range(r$Estimate),
                  ylim = c(6, 14.1)) +
  theme_bw() +
  theme(panel.grid = element_blank())  

##Counterfactual plots
#...brms...
# we need new `nd` data
nd <- 
  tibble(Marriage_s          = seq(from = -3, to = 3, length.out = 30),
         MedianAgeMarriage_s = mean(d$MedianAgeMarriage_s))

fitted(b5.3, newdata = nd) %>% 
  as_tibble() %>% 
  # since `fitted()` and `predict()` name their intervals the same way, 
  # we'll need to `rename()` them to keep them straight
  rename(f_ll = Q2.5,
         f_ul = Q97.5) %>% 
  # note how we're just nesting the `predict()` code right inside `bind_cols()`
  bind_cols(
    predict(b5.3, newdata = nd) %>% 
      as_tibble() %>% 
      # since we only need the intervals, we'll use `transmute()` rather than `mutate()`
      transmute(p_ll = Q2.5,
                p_ul = Q97.5),
    # now tack on the `nd` data
    nd) %>% 
  
  # we're finally ready to plot
  ggplot(aes(x = Marriage_s, y = Estimate)) +
  geom_ribbon(aes(ymin = p_ll, ymax = p_ul),
              fill = "firebrick", alpha = 1/5) +
  geom_smooth(aes(ymin = f_ll, ymax = f_ul),
              stat = "identity",
              fill = "firebrick", color = "firebrick4", alpha = 1/5, size = 1/4) +
  coord_cartesian(xlim = range(d$Marriage_s),
                  ylim = c(6, 14)) +
  labs(subtitle = "Counterfactual plot for which\nMedianAgeMarriage_s = 0",
       y = "Divorce") +
  theme_bw() +
  theme(panel.grid = element_blank())

#For the opporsite graph
# new data
nd <- 
  tibble(MedianAgeMarriage_s = seq(from = -3, to = 3.5, length.out = 30),
         Marriage_s          = mean(d$Marriage_s))

# `fitted()` + `predict()`
fitted(b5.3, newdata = nd) %>% 
  as_tibble() %>% 
  rename(f_ll = Q2.5,
         f_ul = Q97.5) %>% 
  bind_cols(
    predict(b5.3, newdata = nd) %>% 
      as_tibble() %>% 
      transmute(p_ll = Q2.5,
                p_ul = Q97.5),
    nd
  ) %>% 
  
  # plot
  ggplot(aes(x = MedianAgeMarriage_s, y = Estimate)) +
  geom_ribbon(aes(ymin = p_ll, ymax = p_ul),
              fill = "firebrick", alpha = 1/5) +
  geom_smooth(aes(ymin = f_ll, ymax = f_ul),
              stat = "identity",
              fill = "firebrick", color = "firebrick4", alpha = 1/5, size = 1/4) +
  coord_cartesian(xlim = range(d$MedianAgeMarriage_s),
                  ylim = c(6, 14)) +
  labs(subtitle = "Counterfactual plot for which\nMarriage_s = 0",
       y = "Divorce") +
  theme_bw() +
  theme(panel.grid = element_blank())   

##Posterior predictive plots
#...brms...

fitted(b5.3) %>%
  as_tibble() %>%
  bind_cols(d) %>%
  
  ggplot(aes(x = Divorce, y = Estimate)) +
  geom_abline(linetype = 2, color = "grey50", size = .5) +
  geom_point(size = 1.5, color = "firebrick4", alpha = 3/4) +
  geom_linerange(aes(ymin = Q2.5, ymax = Q97.5),
                 size = 1/4, color = "firebrick4") +
  geom_linerange(aes(ymin = Estimate - Est.Error, 
                     ymax = Estimate + Est.Error),
                 size = 1/2, color = "firebrick4") +
  # Note our use of the dot placeholder, here: https://magrittr.tidyverse.org/reference/pipe.html
  geom_text(data = . %>% filter(Loc %in% c("ID", "UT")),
            aes(label = Loc), 
            hjust = 0, nudge_x = - 0.65) +
  labs(x = "Observed divorce", 
       y = "Predicted divorce") +
  theme_bw() +
  theme(panel.grid = element_blank())

##--MilkBrain Example--##
library(pacman)
p_load(tidyverse,brms,rethinking,tidybayes)

#loading the data
data(milk)
d <- milk
head(d)
summary(d)

#omitting na values
d2 <- d[complete.cases(d),]

#showing the immediate relationship between variables
pairs(~kcal.per.g + log(mass) + neocortex.perc, data = d2)

#standadizing variables
d2 <- d2 %>% 
  mutate(kcal.per.g_z           = (kcal.per.g - mean(kcal.per.g))                     / sd(kcal.per.g),
         neocortex.perc_z = (neocortex.perc - mean(neocortex.perc)) / sd(neocortex.perc)
  )

#fitting the first models
#...rethinking...#
m5.5 <- map(
  alist(
    kcal.per.g ~ dnorm(mu,sigma),
    mu <- a + b*neocortex.perc,
    a ~ dnorm(0,100),
    b ~ dnorm(0,1),
    sigma ~ dunif(0,1)
  ), data = d2
)
precis(m5.5, digits = 3)
coef(m5.5)["b"]*(76-55)

#predicted mean and CI
np.seq <- 0:100

pred.data <- data.frame(neocortex.perc=np.seq)

mu <- link(m5.5,data = pred.data, n=1e4)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

plot(kcal.per.g ~ neocortex.perc,data = d2, col=rangi2)
lines(np.seq,mu.mean)
lines(np.seq,mu.PI[1,], lty=2)
lines(np.seq,mu.PI[2,], lty=2)

#extract samples
post <- extract.samples(m5.5, n = 100)
#plot the lines
plot(kcal.per.g ~ neocortex.perc, d2)
for(i in 1:100){
  abline(a=post$a[i], b=post$b[i], col=col.alpha("black",0.3))
}

#second model
d2$log.mass <- log(d2$mass)
d2 <- d2 %>% 
  mutate(
    log.mass_z = log.mass - mean(log.mass)/sd(log.mass) 
  )

m5.6 <- map(
  alist(
    kcal.per.g ~ dnorm(mu,sigma),
    mu <- a + b*log.mass,
    a ~ dnorm(0,100),
    b ~ dnorm(0,1),
    sigma ~ dunif(0,1)
  ), data = d2
)
precis(m5.6)
#predicted mean and CI
np.seq <- -100:100

pred.data <- data.frame(log.mass=np.seq)

mu <- link(m5.6,data = pred.data, n=1e4)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

plot(kcal.per.g ~ log.mass,data = d2, col=rangi2)
lines(np.seq,mu.mean)
lines(np.seq,mu.PI[1,], lty=2)
lines(np.seq,mu.PI[2,], lty=2)
#multivariate model
m5.7 <- map(
  alist(
    kcal.per.g_z ~ dnorm(mu,sigma),
    mu <- a + bm*log.mass_z + bn*neocortex.perc_z,
    a ~ dnorm(0,0.2),
    bm ~ dnorm(0,0.5),
    bn ~ dnorm(0,0.5),
    sigma ~ dexp(1)
  ), data = d2
)

precis(m5.7)
m5.7a <- map(
  alist(
    kcal.per.g ~ dnorm(mu,sigma),
    mu <- a + bm*log.mass + bn*neocortex.perc,
    a ~ dnorm(0,100),
    bm ~ dnorm(0,1),
    bn ~ dnorm(0,1),
    sigma ~ dexp(1)
  ), data = d2
)
#counterfactual plots
#for log mass
mean.log.mass <- mean(d2$log.mass)
mean.log.mass_z <- mean(d2$log.mass_z)
np.seq <- 0:100

pred.data <- data.frame(
  neocortex.perc=np.seq,
  log.mass=mean.log.mass)

mu <- link(m5.7a,data = pred.data, n=1e4)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

plot(kcal.per.g ~ neocortex.perc,data = d2, type="n")
lines(np.seq,mu.mean)
lines(np.seq,mu.PI[1,], lty=2)
lines(np.seq,mu.PI[2,], lty=2)

#for neocortex
mean.neocortex <- mean(d2$neocortex.perc)

np.seq <- -100:100

pred.data <- data.frame(
  log.mass=np.seq,
  neocortex.perc=mean.neocortex)

mu <- link(m5.7a,data = pred.data, n=1e4)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

plot(kcal.per.g ~ log.mass,data = d2, type="n")
lines(np.seq,mu.mean)
lines(np.seq,mu.PI[1,], lty=2)
lines(np.seq,mu.PI[2,], lty=2)


#...brms...#
#formula
b5.5_f <- bf(
  kcal.per.g_z ~ 1 + neocortex.perc_z)

get_prior(b5.5_f, d2)
#setting priors
priorList <- c(
  prior(normal(0, 0.2), class = Intercept),
  prior(normal(0, 0.05), class = b),
  prior(exponential(1), class = sigma))

b5.5_PriorCheck <- brm(
  formula = b5.5_f,
  data = d2,
  family = gaussian,
  prior = priorList,
  sample_prior = "only" #does not take data into account
)

#Prior predictive checks
pp_check(b5.5_PriorCheck, nsamples = 100)

#the model with data
b5.5 <- brm(
  formula = b5.5_f,
  data = d2,
  family = gaussian,
  prior = priorList,
  sample_prior = T,
  iter = 2000, warmup = 500, chains = 4, cores = 4,
  seed = 5)

print(b5.5, digits = 3)
fixef(b5.5)[2] * (76 - 55)

nd <- tibble(neocortex.perc = 54:80)

fitted(b5.5, 
       newdata = nd,
       probs = c(.025, .975, .25, .75)) %>%
  as_tibble() %>%
  bind_cols(nd) %>% 
  
  ggplot(aes(x = neocortex.perc, y = Estimate)) +
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5),
              fill = "firebrick", alpha = 1/5) +
  geom_smooth(aes(ymin = Q25, ymax = Q75),
              stat = "identity",
              fill = "firebrick4", color = "firebrick4", alpha = 1/5, size = 1/2) +
  geom_point(data = d2, 
             aes(y = kcal.per.g),
             size = 2, color = "firebrick4") +
  coord_cartesian(xlim = range(dcc$neocortex.perc), 
                  ylim = range(dcc$kcal.per.g)) +
  ylab("kcal.per.g") +
  theme_bw() +
  theme(panel.grid = element_blank())

