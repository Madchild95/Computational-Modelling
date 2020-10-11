###-----Rethinking: Chapter 4-----###

#packages
library(pacman)
p_load(tidyverse,brms)

##Regressions
#Normal distributions: the soccer field example
# we set the seed to make the results of `runif()` reproducible.
set.seed(4)
pos <- 
  replicate(100, runif(16, -1, 1)) %>%        # here's the simulation
  as_tibble() %>%                             # for data manipulation, we'll make this a tibble
  rbind(0, .) %>%                             # here we add a row of zeros above the simulation results
  mutate(step = 0:16) %>%                     # this adds a step index
  gather(key, value, -step) %>%               # here we convert the data to the long format
  mutate(person = rep(1:100, each = 17)) %>%  # this adds a person id index
  # the next two lines allow us to make cumulative sums within each person
  group_by(person) %>%
  mutate(position = cumsum(value)) %>%
  ungroup()  # ungrouping allows for further data manipulation
glimpse(pos)

# the plot from the book
ggplot(data = pos, 
       aes(x = step, y = position, group = person)) +
  geom_vline(xintercept = c(4, 8, 16), linetype = 2) +
  geom_line(aes(color = person < 2, alpha  = person < 2)) +
  scale_color_manual(values = c("skyblue4", "black")) +
  scale_alpha_manual(values = c(1/5, 1)) +
  scale_x_continuous("step number", breaks = c(0, 4, 8, 12, 16)) +
  theme(legend.position = "none")
# Figure 4.2.a.
pos %>%
  filter(step == 4) %>%
  ggplot(aes(x = position)) +
  geom_line(stat = "density", color = "dodgerblue1") +
  coord_cartesian(xlim = -6:6) +
  labs(title = "4 steps")
# Figure 4.2.b.
pos %>%
  filter(step == 8) %>%
  ggplot(aes(x = position)) +
  geom_density(color = "dodgerblue2") +
  coord_cartesian(xlim = -6:6) +
  labs(title = "8 steps")
# Figure 4.2.c.
pos %>%
  filter(step == 16) %>%
  ggplot(aes(x = position)) +
  stat_function(fun = dnorm,
                args = list(mean = 0, sd = 2.180408),
                linetype = 2) +  # 2.180408 came from the previous code block
  geom_density(color = "transparent", fill = "dodgerblue3", alpha = 1/2) +
  coord_cartesian(xlim = -6:6) +
  labs(title = "16 steps",
       y     = "density")

#Using normal distributions
# how many `p_grid` points would you like?
n_points <- 100

(d <-
  tibble(w          = 6, 
         n          = 9,
         p_grid     = seq(from = 0, to = 1, length.out = n_points)) %>% 
  mutate(prior      = dunif(p_grid, 0, 1),
         likelihood = dbinom(w, n, p_grid)) %>% 
  mutate(posterior  = likelihood * prior / sum(likelihood * prior)))

#the visuals
d %>% 
  select(-w, -n) %>% 
  gather(key, value, -p_grid) %>% 
  # this line allows us to dictate the order the panels will appear in
  mutate(key = factor(key, levels = c("prior", "likelihood", "posterior"))) %>% 
  
  ggplot(aes(x = p_grid, ymin = 0, ymax = value, fill = key)) +
  geom_ribbon() +
  scale_fill_manual(values = c("blue", "red", "purple")) +
  scale_y_continuous(NULL, breaks = NULL) +
  theme(legend.position = "none") +
  facet_wrap(~key, scales = "free")


##---the height example model---##

library(rethinking)
data(Howell1)
d <- Howell1

#detaching rethinking in order to use brms
rm(Howell1)
detach(package:rethinking, unload = T)
library(brms)
library(tidyverse)

#investigationg the data
d %>%
  str() 

#getting the height values
d %>%
  select(height) %>%
  head()

#getting adults only
d2 <- 
  d %>%
  filter(age >= 18)
#plotting the density of height
plot(density(d2$height))

#plotting the priors (with rethinking)
curve(dnorm(x,178,20), from = 100, to = 250) #mu
curve(dunif(x,0,50), from = -10, to = 60) #sigma

# ...with brms...
#the plot for my
ggplot(data = tibble(x = seq(from = 100, to = 250, by = .1)), 
       aes(x = x, y = dnorm(x, mean = 178, sd = 20))) +
  geom_line() +
  ylab("density")
#the plot for sigma
tibble(x = seq(from = -10, to = 60, by = .1)) %>%
  
  ggplot(aes(x = x, y = dunif(x, min = 0, max = 50))) +
  geom_line() +
  scale_y_continuous(NULL, breaks = NULL) +
  theme(panel.grid = element_blank())

#sample from the prior
sample_mu <- rnorm(1e4, 183.7, 15)
sample_sigma <- runif(1e4, 0, 30)
prior_h <- rnorm(1e4, sample_mu, sample_sigma)
dens(prior_h)

#getting the prior probability distribution of hight
n <- 1e4

set.seed(4)
tibble(sample_mu    = rnorm(n, mean = 178,       sd = 20),
       sample_sigma = runif(n, min = 0,         max = 50)) %>% 
  mutate(x          = rnorm(n, mean = sample_mu, sd = sample_sigma)) %>% 
  
  ggplot(aes(x = x)) +
  geom_density() +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(subtitle = expression(paste("Prior predictive distribution for ", italic(h[i]))),
       x        = NULL) +
  theme(panel.grid = element_blank())

#The Grid app. for our height distribution
#...rethinking...
mu.list <- seq(from = 140, to = 160, length.out = 200)
sigma.list <- seq(from = 4,   to = 9,   length.out = 200)
post <- expand.grid(mu = mu.list, sigma = sigma.list)
post$LL <- sapply(1:nrow(post),function(i) sum(dnorm( #loglikelihood
  d2$height,
  mean = post$mu[i],
  sd = post$sigma[i],
  log = T)))
post$prod <- post$LL + 
  dnorm(post$mu, 178, 20, T) +
  dunif(post$sigma, 0, 50, T)
post$prob <- exp(post$prod - max(post$prod))

#...brms...
n <- 200

d_grid <-
  tibble(mu    = seq(from = 140, to = 160, length.out = n),
         sigma = seq(from = 4,   to = 9,   length.out = n)) %>% 
  # we'll accomplish with `tidyr::expand()` what McElreath did with base R `expand.grid()`
  expand(mu, sigma)

head(d_grid)
#defining the grid function
grid_function <- function(mu, sigma){
  dnorm(d2$height, mean = mu, sd = sigma, log = T) %>% 
    sum()
}
#getting all the values needed
d_grid <-
  d_grid %>% 
  mutate(log_likelihood = map2(mu, sigma, grid_function)) %>% 
  unnest() %>% 
  mutate(prior_mu       = dnorm(mu,    mean = 178, sd  = 20, log = T),
         prior_sigma    = dunif(sigma, min  = 0,   max = 50, log = T)) %>% 
  mutate(product        = log_likelihood + prior_mu + prior_sigma) %>% 
  mutate(probability    = exp(product - max(product)))

head(d_grid)

#Plottings
#contour plot of the posteriors from mu and sigma
#...rethinking...
contour_xyz(post$mu, post$sigma, post$prob)
#...brms...
d_grid %>% 
  ggplot(aes(x = mu, y = sigma, z = probability)) + 
  geom_contour() +
  labs(x = expression(mu),
       y = expression(sigma)) +
  coord_cartesian(xlim = range(d_grid$mu),
                  ylim = range(d_grid$sigma)) +
  theme(panel.grid = element_blank())

#as a heat map
#...rethinking...
image_xyz(post$mu, post$sigma, post$prob)
#...brms...
d_grid %>% 
  ggplot(aes(x = mu, y = sigma)) + 
  geom_raster(aes(fill = probability),
              interpolate = T) +
  scale_fill_viridis_c(option = "A") +
  labs(x = expression(mu),
       y = expression(sigma)) +
  theme(panel.grid = element_blank())

#drawing samples from the mu and sigma 
#...rethinking...
sample.rows <- sample(1:nrow(post), size = 1e4, replace = T, prob = post$prob)
sample.mu <- post$mu[sample.rows]
sample.sigma <- post$sigma[sample.rows]
plot(sample.mu,sample.sigma, cex = 0.5, pch = 16, col = col.alpha(rangi2, 0.1))

#...brms...
set.seed(4)
d_grid_samples <- 
  d_grid %>% 
  sample_n(size = 1e4, replace = T, weight = probability)
#and plotting them as a plausibility distribution
d_grid_samples %>% 
  ggplot(aes(x = mu, y = sigma)) + 
  geom_point(size = .9, alpha = 1/15) +
  scale_fill_viridis_c() +
  labs(x = expression(mu[samples]),
       y = expression(sigma[samples])) +
  theme(panel.grid = element_blank())

#as a density plot
#...rethinking...
dens(sample.mu)
dens(sample.sigma)
#...brms...
d_grid_samples %>% 
  select(mu, sigma) %>% 
  gather() %>% 
  
  ggplot(aes(x = value)) + 
  geom_density(fill = "grey33", size = 0) +
  scale_y_continuous(NULL, breaks = NULL) +
  xlab(NULL) +
  theme(panel.grid = element_blank()) +
  facet_wrap(~key, scales = "free")

#computing posterior modes and 95% HDIs
#...rethinking...
HPDI(sample.mu)
HPDI(sample.sigma)

#...brms...
library(tidybayes)

d_grid_samples %>% 
  select(mu, sigma) %>% 
  gather() %>% 
  group_by(key) %>% 
  mode_hdi(value)

#making a point about the distribution of sigma: it is not always normally distributed, because it has to be positive, 
#which means that it has a much broader range of possibility in one direction than the other, making it skewed to 
#the right (long right-hand tail) 
set.seed(4)
d3 <- sample(d2$height, size = 20)

n <- 200

# note we've redefined the ranges of `mu` and `sigma`
d_grid <-
  tibble(mu    = seq(from = 150, to = 170, length.out = n),
         sigma = seq(from = 4,   to = 20,  length.out = n)) %>% 
  expand(mu, sigma)
#changing functions to heights of d3
grid_function <- function(mu, sigma){
  dnorm(d3, mean = mu, sd = sigma, log = T) %>% 
    sum()
}
#making the posterior
(d_grid <-
  d_grid %>% 
  mutate(log_likelihood = map2_dbl(mu, sigma, grid_function)) %>% 
  mutate(prior_mu       = dnorm(mu,    mean = 178, sd  = 20, log = T),
         prior_sigma    = dunif(sigma, min  = 0,   max = 50, log = T)) %>% 
  mutate(product        = log_likelihood + prior_mu + prior_sigma) %>% 
  mutate(probability    = exp(product - max(product))))
#drawing new samples
set.seed(4)
d_grid_samples <- 
  d_grid %>% 
  sample_n(size = 1e4, replace = T, weight = probability)
#making another scatter plot
d_grid_samples %>% 
  ggplot(aes(x = mu, y = sigma)) + 
  geom_point(size = .9, alpha = 1/15) +
  scale_fill_viridis_c() +
  labs(x = expression(mu[samples]),
       y = expression(sigma[samples])) +
  theme(panel.grid = element_blank())
#and density plot
d_grid_samples %>% 
  select(mu, sigma) %>% 
  gather() %>% 
  
  ggplot(aes(x = value)) + 
  geom_density(fill = "grey33", size = 0) +
  scale_y_continuous(NULL, breaks = NULL) +
  xlab(NULL) +
  theme(panel.grid = element_blank()) +
  facet_wrap(~key, scales = "free")

##Fitting the quadridic model##
#...rethinking...
flist <- alist(
  height ~ dnorm(mu,sigma),
  mu ~ dnorm(178,20),
  sigma ~ dunif(0,50)
)
#fitting the model
m4.1 <- map(flist, data = d2)
#summary
precis(m4.1)
#with a strong prior for mu
m4.2 <- map(
  alist(
    height ~ dnorm(mu,sigma),
    mu ~ dnorm(178,0.1),
    sigma ~ dunif(0,50)
  ),
  data = d2
)
precis(m4.2)
#mu is much less affected by the data and sigma changes according to mu (see why in notes on 'How strong a prior?')
sd(d2$height)
mean(d2$height)
#we can try using a logscale on sigma to solve this 
m4.1_logsigma <- map(
  alist(
    height ~ dnorm(mu,exp(log_sigma)),
    mu ~ dnorm(178,0.1),
    log_sigma ~ dnorm(2,10)
  ),
  data = d2
)
post <- extract.samples(m4.1_logsigma)
sigma <- exp(post$log_sigma)
precis(m4.1_logsigma)
plot(sigma)
#...brms...
b4.1 <- 
  brm(data = d2, family = gaussian,
      height ~ 1,
      prior = c(prior(normal(178, 20), class = Intercept),
                prior(uniform(0, 50), class = sigma)),
      iter = 31000, warmup = 30000, chains = 4, cores = 4,
      seed = 4)
print(b4.1, prob = .89)

#fitting with a half Cauchy prior for sigma 
b4.1_half_cauchy <- 
  brm(data = d2, family = gaussian,
      height ~ 1,
      prior = c(prior(normal(178, 20), class = Intercept),
                prior(cauchy(0, 1), class = sigma)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      seed = 4)
plot(b4.1_half_cauchy)
print(b4.1_half_cauchy)
posterior_summary(b4.1_half_cauchy)
#Sampling from the posterior
#...rethinking...
post <- extract.samples(m4.1, n=1e4)
head(post)
precis(post)
plot(post) 

#...brms...
post <- posterior_samples(b4.1_half_cauchy)
head(post)
#maybe this function samples from the posterior??
ggplot(data = post, 
       aes(x = sigma)) +
  geom_density(size = 1/10, fill = "black") +
  scale_y_continuous(NULL, breaks = NULL) +
  xlab(expression(sigma)) +
  theme(panel.grid = element_blank())
#there is no problem with skew here, because HCM handles it

##Adding a predictor##

#the covariance of height and weight
#...rethinking...
plot(d2$height~d2$weight)
#yes, there is a linear relationship
# "fitting the model" with the new predictor
m4.3 <- map(
  alist(
    height ~ dnorm(mu,sigma),
    mu <- a + b*weight, 
    a ~dnorm(156,100),
    b ~ dnorm(0,10),
    sigma ~ dunif(0,50)
  ),
  data = d2
)
precis(m4.3)
#when weight goes up 1 kg, height goes up 90 cm with a sd of 5 cm to both sides. a doesn't make sense right now

#then we look at the correlation matrix
precis(m4.3, corr = T)
#we see that weight and height are almost perfectly negatively correlated

#then we mean center
d2$weight.c <- d2$weight - mean(d2$weight)
round(mean(d2$weight.c),3)
#then refit the model
m4.4 <- map(
  alist(
    height ~ dnorm(mu,sigma),
    mu <- a + b*weight.c, 
    a ~dnorm(156,100),
    b ~ dnorm(0,10),
    sigma ~ dunif(0,50)
  ),
  data = d2
)
precis(m4.4,corr=T)

#...brms...
b4.3 <- 
  brm(data = d2, family = gaussian,
      height ~ 1 + weight,
      prior = c(prior(normal(156, 100), class = Intercept),
                prior(normal(0, 10), class = b),
                prior(uniform(0, 50), class = sigma)),
      iter = 41000, warmup = 40000, chains = 4, cores = 4,
      seed = 4)
plot(b4.3)
posterior_summary(b4.3)[1:3, ]
#creating a correlation matrix
posterior_samples(b4.3) %>%
  select(-lp__) %>%
  cor() %>%
  round(digits = 2)

#mean centering with brms
d2 <- 
  d2 %>%
  mutate(weight_c = weight - mean(weight))
#refitting
b4.4 <- 
  brm(data = d2, family = gaussian,
      height ~ 1 + weight_c,
      prior = c(prior(normal(178, 100), class = Intercept),
                prior(normal(0, 10), class = b),
                prior(uniform(0, 50), class = sigma)),
      iter = 46000, warmup = 45000, chains = 4, cores = 4,
      seed = 4)

posterior_summary(b4.4)[1:3, ]
#and the cor matrix
posterior_samples(b4.4) %>%
  select(-lp__) %>%
  cor() %>%
  round(digits = 2)
#visual corr matrix
pairs(b4.4)

##Plotting the posterior##
#...rethinking...
#simple plot of linear relation superimposed on the data
plot(height ~ weight, data = d2)
abline(a=coef(m4.3)["a"], b=coef(m4.3)["b"])

#displaying uncertainty
#extracting first 20 data points and refitting
N <- 352
dN <- d2[1:N,]
mN <- map(
  alist(
    height ~ dnorm(mu,sigma),
    mu <- a + b*weight, 
    a ~ dnorm(178,100),
    b ~ dnorm(0,10),
    sigma ~ dunif(0,50)
  ),
  data = dN
)
#extract 20 samples from the posteriror
post <- extract.samples(mN, n=20)
#display raw data and sample size
plot(dN$weight,dN$height,
     col=rangi2, xlab="weight", ylab="height"
     )
mtext(concat("N = ", N))
#plot lines with transparency
for(i in 1:20){
  abline(a=post$a[i], b=post$b[i], col=col.alpha("black",0.2))
}

mu_at_50 <- post$a + post$b * 50

dens(mu_at_50, col=rangi2, lwd=2, xlab="mu|weight=50")

HPDI(mu_at_50, prob=0.89)
#the central 89% of the ways for the model to produce the data place the average height between 159cm and 160cm (conditional on the model and the data) assuming the weight is 50 kg

#define sequence of weights to compute predictions for
#these values will be be on the horizontal axis
weight.seq <- seq(from = 25, to = 70, by = 1)

#use link to compute mu for ech sample from posterior and for each weight in weight.seq
mu <- link(m4.3, data = data.frame(weight = weight.seq))
str(mu)

#plot of mu value at each height
#use type="n" to hide raw data
plot(height~weight, d2, type="n")
#loop over samples and plot each mu value
for(i in 1:100){
  points(weight.seq, mu[i,], pch = 16, col = col.alpha(rangi2,0.1))
}

#summarize the distribution for mu
mu.mean <- apply(mu, 2, mean) #compute the mean of each column (dimension "2") of the matrix mu
mu.HPDI <- apply(mu, 2, HPDI, prob = 0.89)

#plotting the uncertainty with mean and HPDI
plot(height~weight, d2, col = col.alpha(rangi2, 0.5))
lines(weight.seq, mu.mean)
shade(mu.HPDI, weight.seq)

#...brms...

#plotting posterior inference against data 
d2 %>%
  ggplot(aes(x = weight, y = height)) +
  geom_abline(intercept = fixef(b4.3)[1], 
              slope     = fixef(b4.3)[2]) +
  geom_point(shape = 1, size = 2, color = "royalblue") +
  theme_bw() +
  theme(panel.grid = element_blank())

#Plotting uncertainty
post <- posterior_samples(b4.3)

post %>%
  slice(1:5)  # this serves a similar function as `head()`

#getting the population level changes
n <- 10

b.10 <- 
  brm(data = d2 %>%
        slice(1:n),  # note our tricky use of `n` and `slice()`
      family = gaussian,
      height ~ 1 + weight,
      prior = c(prior(normal(178, 100), class = Intercept),
                prior(normal(0, 10), class = b),
                prior(cauchy(0, 1), class = sigma)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      seed = 4)

n <- 50

b.50 <- 
  brm(data = d2 %>%
        slice(1:n), 
      family = gaussian,
      height ~ 1 + weight,
      prior = c(prior(normal(178, 100), class = Intercept),
                prior(normal(0, 10), class = b),
                prior(cauchy(0, 1), class = sigma)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      seed = 4)

n <- 150

b.150 <- 
  brm(data = d2 %>%
        slice(1:n), 
      family = gaussian,
      height ~ 1 + weight,
      prior = c(prior(normal(178, 100), class = Intercept),
                prior(normal(0, 10), class = b),
                prior(cauchy(0, 1), class = sigma)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      seed = 4)

n <- 352

b.352 <- 
  brm(data = d2 %>%
        slice(1:n), 
      family = gaussian,
      height ~ 1 + weight,
      prior = c(prior(normal(178, 100), class = Intercept),
                prior(normal(0, 10), class = b),
                prior(cauchy(0, 1), class = sigma)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      seed = 4)

#plots and summary
plot(b.10)
print(b.10)

plot(b.50)
print(b.50)

plot(b.150)
print(b.150)

plot(b.352)
print(b.352)

#putting chains into a dataframe
post10  <- posterior_samples(b.10)
post50  <- posterior_samples(b.50)
post150 <- posterior_samples(b.150)
post352 <- posterior_samples(b.352)

#plotting the different models
p10 <- 
  ggplot(data =  d2[1:10 , ], 
         aes(x = weight, y = height)) +
  geom_abline(intercept = post10[1:20, 1], 
              slope     = post10[1:20, 2],
              size = 1/3, alpha = .3) +
  geom_point(shape = 1, size = 2, color = "royalblue") +
  coord_cartesian(xlim = range(d2$weight),
                  ylim = range(d2$height)) +
  labs(subtitle = "N = 10") +
  theme_bw() +
  theme(panel.grid = element_blank())

p50 <-
  ggplot(data =  d2[1:50 , ], 
         aes(x = weight, y = height)) +
  geom_abline(intercept = post50[1:20, 1], 
              slope     = post50[1:20, 2],
              size = 1/3, alpha = .3) +
  geom_point(shape = 1, size = 2, color = "royalblue") +
  coord_cartesian(xlim = range(d2$weight),
                  ylim = range(d2$height)) +
  labs(subtitle = "N = 50") +
  theme_bw() +
  theme(panel.grid = element_blank())

p150 <-
  ggplot(data =  d2[1:150 , ], 
         aes(x = weight, y = height)) +
  geom_abline(intercept = post150[1:20, 1], 
              slope     = post150[1:20, 2],
              size = 1/3, alpha = .3) +
  geom_point(shape = 1, size = 2, color = "royalblue") +
  coord_cartesian(xlim = range(d2$weight),
                  ylim = range(d2$height)) +
  labs(subtitle = "N = 150") +
  theme_bw() +
  theme(panel.grid = element_blank())

p352 <- 
  ggplot(data =  d2[1:352 , ], 
         aes(x = weight, y = height)) +
  geom_abline(intercept = post352[1:20, 1], 
              slope     = post352[1:20, 2],
              size = 1/3, alpha = .3) +
  geom_point(shape = 1, size = 2, color = "royalblue") +
  coord_cartesian(xlim = range(d2$weight),
                  ylim = range(d2$height)) +
  labs(subtitle = "N = 352") +
  theme_bw() +
  theme(panel.grid = element_blank())

#placing the plots next to each other
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

multiplot(p10, p150, p50, p352, cols = 2)

#Plotting regression intervals and contours
mu_at_50 <- 
  post %>% 
  transmute(mu_at_50 = b_Intercept + b_weight * 50)

head(mu_at_50)

#the density plot
mu_at_50 %>%
  ggplot(aes(x = mu_at_50)) +
  geom_density(size = 0, fill = "royalblue") +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(x = expression(mu["height | weight = 50"])) +
  theme_classic()

#getting the uncertainty
mean_hdi(mu_at_50[,1], .width = c(.89, .95))

#expressing uncertainty on the density plot
mu_at_50 %>%
  ggplot(aes(x = mu_at_50)) +
  geom_density(size = 0, fill = "royalblue") +
  stat_pointintervalh(aes(y = 0), 
                      point_interval = mode_hdi, .width = .95) +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(x = expression(mu["height | weight = 50"])) +
  theme_classic()

#using fitted instead of link
mu <- fitted(b4.3, summary = F)

str(mu)
#drawing out weights between 25 and 70 
weight_seq <- tibble(weight = seq(from = 25, to = 70, by = 1))

mu <-
  fitted(b4.3,
         summary = F,
         newdata = weight_seq) %>%
  as_tibble() %>%
  # here we name the columns after the `weight` values from which they were computed
  set_names(25:70) %>% 
  mutate(iter = 1:n())

str(mu)

#converting from wide to long format
mu <- 
  mu %>%
  gather(weight, height, -iter) %>% 
  # We might reformat `weight` to numerals
  mutate(weight = as.numeric(weight))

head(mu)

#getting the point plot
d2 %>%
  ggplot(aes(x = weight, y = height)) +
  geom_point(data = mu %>% filter(iter < 101),
             alpha = .1)

#alternatively
d2 %>%
  ggplot(aes(x = weight, y = height)) +
  geom_point(data = mu %>% filter(iter < 101), 
             color = "navyblue", alpha = .05) +
  theme(text = element_text(family = "Times"),
        panel.grid = element_blank())

#regression line and intervals table¨
mu_summary <-
  fitted(b4.3, 
         newdata = weight_seq) %>%
  as_tibble() %>%
  # let's tack on the `weight` values from `weight_seq`
  bind_cols(weight_seq)

head(mu_summary)

#regression line plot with uncertainty
d2 %>%
  ggplot(aes(x = weight, y = height)) +
  geom_smooth(data = mu_summary,
              aes(y = Estimate, ymin = Q2.5, ymax = Q97.5),
              stat = "identity",
              fill = "grey70", color = "black", alpha = 1, size = 1/2) +
  geom_point(color = "navyblue", shape = 1, size = 1.5, alpha = 2/3) +
  coord_cartesian(xlim = range(d2$weight)) +
  theme(text = element_text(family = "Times"),
        panel.grid = element_blank())


##Prediction Intervals##

#...rethinking...
#creating posterior predictions of heights 
sim.height <- sim(m4.3, data = list(weight=weight.seq), n =1e4)
str(sim.height)

#and the interval 
height.PI <- apply(sim.height, 2, PI, prob = 0.89)

#and plotting it all
#raw data
plot(height~weight, d2, col = col.alpha(rangi2,0.5))

#draw MAP line
lines(weight.seq, mu.mean)

#draw HPDI region for line
shade(mu.HPDI, weight.seq)

#draw PI region for simulated heights
shade(height.PI, weight.seq)

#...brms...
#create predictions of height
pred_height <-
  predict(b4.3,
          newdata = weight_seq) %>%
  as_tibble() %>%
  bind_cols(weight_seq)

pred_height %>%
  slice(1:6)

#the plot
d2 %>%
  ggplot(aes(x = weight)) +
  geom_ribbon(data = pred_height, 
              aes(ymin = Q2.5, ymax = Q97.5),
              fill = "grey83") +
  geom_smooth(data = mu_summary,
              aes(y = Estimate, ymin = Q2.5, ymax = Q97.5),
              stat = "identity",
              fill = "grey70", color = "black", alpha = 1, size = 1/2) +
  geom_point(aes(y = height),
             color = "navyblue", shape = 1, size = 1.5, alpha = 2/3) +
  coord_cartesian(xlim = range(d2$weight),
                  ylim = range(d2$height)) +
  theme(text = element_text(family = "Times"),
        panel.grid = element_blank())

##Polynomial regression##

#...rethinking...
#getting the full dataset
data("Howell1")
d <- Howell1
str(d)
#without filtering adults the relationship between height and weight is now curved
plot(height~weight, data = d)

#standardizing weight
d$weight.s <- (d$weight - mean(d$weight))/sd(d$weight)
mean(d$weight.s)
sd(d$weight.s)
#visual of the changed scale of x
plot(height~weight.s, data = d)
#creating the squared weight for b2
d$weight.s2 <- d$weight.s^2
#fitting the polynomial model
m4.5 <- map(
  alist(
    height ~ dnorm(mu,sigma),
    mu <- a + b1*weight.s + b2*weight.s2, 
    a ~dnorm(178,100),
    b1 ~ dnorm(0,10),
    b2~ dnorm(0,10),
    sigma ~ dunif(0,50)
  ),
  data = d)
precis(m4.5)

#plotting for interpretation
weight.seq <- seq(from = -2.2, to = 2, length.out = 30)
pred_dat <- list(weight.s = weight.seq, weight.s2 = weight.seq^2)
mu <- link(m4.5, data = pred_dat)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob = 0.89)
sim.height <- sim(m4.5, data = pred_dat)
height.PI <- apply(sim.height, 2, PI, prob = 0.89)
#plotting
plot(height~weight.s, d, col=col.alpha(rangi2,0.5),xaxt="n")
at <- c(-2,-1,0,1,2) #non-standadized xaxis
lables <- at*sd(d$weight) + mean(d$weight) #non-standadized xaxis
axis(side = 1, at = at, labels = round(lables,1)) #non-standadized xaxis
lines(weight.seq, mu.mean)
shade(mu.PI, weight.seq)
shade(height.PI, weight.seq)

#...brms...
data("Howell1")
d <- Howell1
#standadizing weight
d <-
  d %>%
  mutate(weight_s = (weight - mean(weight)) / sd(weight))

#refitting model
b4.5 <- 
  brm(data = d, family = gaussian,
      height ~ 1 + weight_s + I(weight_s^2),
      prior = c(prior(normal(178, 100), class = Intercept),
                prior(normal(0, 10), class = b),
                prior(cauchy(0, 1), class = sigma)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      seed = 4)

plot(b4.5)
print(b4.5)

#creating new fitted and predicted values
weight_seq <- tibble(weight_s = seq(from = -2.5, to = 2.5, length.out = 30))

f_quad <-
  fitted(b4.5, 
         newdata = weight_seq) %>%
  as_tibble() %>%
  bind_cols(weight_seq)

p_quad <-
  predict(b4.5, 
          newdata = weight_seq) %>%
  as_tibble() %>%
  bind_cols(weight_seq)  
#plotting the polynomial model
ggplot(data = d, 
       aes(x = weight_s)) +
  geom_ribbon(data = p_quad, 
              aes(ymin = Q2.5, ymax = Q97.5),
              fill = "grey83") +
  geom_smooth(data = f_quad,
              aes(y = Estimate, ymin = Q2.5, ymax = Q97.5),
              stat = "identity",
              fill = "grey70", color = "black", alpha = 1, size = 1/2) +
  geom_point(aes(y = height),
             color = "navyblue", shape = 1, size = 1.5, alpha = 1/3) +
  coord_cartesian(xlim = range(d$weight_s)) +
  theme(text = element_text(family = "Times"),
        panel.grid = element_blank())+
  #converting the xaxis back to non-standadized measures
  scale_x_continuous("standardized weight converted back",
                   breaks = at,
                   labels = round(at * sd(d$weight) + mean(d$weight), 1))




