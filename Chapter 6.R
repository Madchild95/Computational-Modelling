###-----Rethinking: Chapter 6-----###

#packages
library(pacman)
p_load(tidyverse, 
       brms, #also known as Bromance, Bayesian models with Tidyverse language
       rethinking, #McElreath's package
       ggrepel, #for texts in ggplot
       rcartocolor, #a color package used to plots in this chapter
       broom, #provides an array of convenience functions to convert statistical analysis summaries into tidy data objects
       gridExtra, #to get the plots together
       tidybayes
       )


##--Brain volume/body mass example--##

#...brms...
#creating a data frame easily
(
  d <- 
    tibble(species = c("afarensis", "africanus", "habilis", "boisei", "rudolfensis", "ergaster", "sapiens"), 
           brain   = c(438, 452, 612, 521, 752, 871, 1350), 
           mass    = c(37.0, 35.5, 34.5, 41.5, 55.5, 61.0, 53.5))
)

#simple plot of what we are trying to meassure 
plot(brain ~ mass, d)

#testing colors
carto_pal(7, "BurgYl") #getting the HEX numbers used
display_carto_pal(7, "BurgYl") #displaying the colors

#and plotting it with ggplot
d %>%
  ggplot(aes(x =  mass, y = brain, label = species)) +
  geom_point(color = carto_pal(7, "BurgYl")[5]) + #using number 5 for the points
  geom_text_repel(size = 3, color = carto_pal(7, "BurgYl")[7], family = "Courier", seed = 438) + #Adds the text to the plot points
  coord_cartesian(xlim = 30:65) + #defining barriers on the x-axis
  labs(x = "body mass (kg)",
       y = "brain volume (cc)",
       subtitle = "Average brain volume by body\nmass for six hominin species") +
  theme_classic() + #takes gitter away from the plot etc.
  theme(text = element_text(family = "Courier"),
        panel.background = element_rect(fill = alpha(carto_pal(7, "BurgYl")[3], 1/4))) #using a diluted version of the third color for the panel background

##Fitting six different models to describe brain volume ~ body mass
#making a model-fitting function because we want 6 different models
fit_lm <- function(model, formula){
  model <- lm(data = d, formula = formula)
}
#making a NESTED TIBBLE that saves the fits with some different information from the broom package
fits <-
  tibble(model   = str_c("b6.", 1:6),
         formula = c("brain ~ mass", 
                     "brain ~ mass + I(mass^2)", 
                     "brain ~ mass + I(mass^2) + I(mass^3)", 
                     "brain ~ mass + I(mass^2) + I(mass^3) + I(mass^4)", 
                     "brain ~ mass + I(mass^2) + I(mass^3) + I(mass^4) + I(mass^5)", 
                     "brain ~ mass + I(mass^2) + I(mass^3) + I(mass^4) + I(mass^5) + I(mass^6)")) %>% 
  mutate(fit     = purrr::map2(model, formula, fit_lm)) %>% 
  mutate(tidy    = purrr::map(fit, tidy),
         glance  = purrr::map(fit, glance))

# what did we just do?
print(fits)

#extracting R^2 from the nested tibble
fits <-
  fits %>% 
  mutate(r2      = glance %>% map_dbl("r.squared")) %>% #getting the number in decimals
  mutate(r2_text = round(r2, digits = 2) %>% as.character() %>% str_replace(., "0.", ".")) #writing it out in a more readable manner

#plotting the R^2 fits displaying it against the models 
fits %>% 
  ggplot(aes(x = r2, y = formula, label = r2_text)) + #putting R^2 on x-axis, formulas on y-axis, and the respective R^2 values as "data points"
  geom_text(color = carto_pal(7, "BurgYl")[7], size = 3.5) + #defining the text in the plot
  scale_x_continuous(expression(italic(R)^2), limits = 0:1, breaks = 0:1) + #setting the scale for x-axis on a 0-1 scale and adding a label
  ylab(NULL) + #not showing number in between 0 and 1
  theme_classic() +
  theme(axis.text.y  = element_text(hjust = 0), #probably location of text label on x-axis
        axis.ticks.y = element_blank(), #removing numbers from y-axis
        text         = element_text(family = "Courier"), #font of the text
        panel.background = element_rect(fill = alpha(carto_pal(7, "BurgYl")[3], 1/4))) #coloring the background

#making the plot from page 171
#specifying a base plot
p <-
  d %>% 
  ggplot(aes(x = mass, y = brain)) +
  geom_point(color = carto_pal(7, "BurgYl")[7]) +
  scale_x_continuous("body mass (kg)", limits = c(33, 62), expand = c(0, 0)) +
  coord_cartesian(ylim = c(300, 1500)) +
  ylab("brain volume (cc)") +
  theme_classic() +
  theme(text = element_text(family = "Courier"),
        panel.background = element_rect(fill = alpha(carto_pal(7, "BurgYl")[3], 1/4)))

#Applying it to all of the plots
# linear
p1 <- 
  p +
  stat_smooth(method = "lm", fullrange = TRUE, level = .89,  # note our rare use of 89% intervals
              color = carto_pal(7, "BurgYl")[6], fill = carto_pal(7, "BurgYl")[6], 
              size = 1/2, alpha = 1/3,
              formula = y ~ x) +
  ggtitle(NULL, subtitle = expression(paste(italic(R)^2, " = .49")))

# quadratic
p2 <-
  p + 
  stat_smooth(method = "lm", fullrange = TRUE, level = .89,
              color = carto_pal(7, "BurgYl")[6], fill = carto_pal(7, "BurgYl")[6], 
              size = 1/2, alpha = 1/3,              
              formula = y ~ poly(x, 2)) +
  ggtitle(NULL, subtitle = expression(paste(italic(R)^2, " = .54")))

# cubic
p3 <-
  p + 
  stat_smooth(method = "lm", fullrange = TRUE, level = .89,
              color = carto_pal(7, "BurgYl")[6], fill = carto_pal(7, "BurgYl")[6], 
              size = 1/2, alpha = 1/3,              
              formula = y ~ poly(x, 3)) +
  ggtitle(NULL, subtitle = expression(paste(italic(R)^2, " = .68")))

# fourth-order polynomial
p4 <-
  p + 
  stat_smooth(method = "lm", fullrange = TRUE, level = .89,
              color = carto_pal(7, "BurgYl")[6], fill = carto_pal(7, "BurgYl")[6], 
              size = 1/2, alpha = 1/3,              
              formula = y ~ poly(x, 4)) +
  ggtitle(NULL, subtitle = expression(paste(italic(R)^2, " = .81")))

# fifth-order polynomial
p5 <-
  p + 
  stat_smooth(method = "lm", fullrange = TRUE, level = .89,
              color = carto_pal(7, "BurgYl")[6], fill = carto_pal(7, "BurgYl")[6], 
              size = 1/2, alpha = 1/3,              
              formula = y ~ poly(x, 5)) +
  # we're adjusting the y-axis range for this plot (and the next)
  coord_cartesian(ylim = c(150, 1900)) +
  ggtitle(NULL, subtitle = expression(paste(italic(R)^2, " = .99")))

# sixth-order polynomial
p6 <-
  p + 
  # mark off 0 on the y-axis
  geom_hline(yintercept = 0, color = carto_pal(7, "BurgYl")[2], linetype = 2) + 
  stat_smooth(method = "lm", fullrange = TRUE, level = .89,
              color = carto_pal(7, "BurgYl")[6], fill = carto_pal(7, "BurgYl")[6], 
              size = 1/2, alpha = 1/3,              
              formula = y ~ poly(x, 6)) +
  coord_cartesian(ylim = c(-300, 1500)) +
  ggtitle(NULL, subtitle = expression(paste(italic(R)^2, " = 1")))

#And them pasting them together
grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2)

#Sensitivity of overfitting
# because these lines will be very curvy, we'll need new data over many points of `mass`
nd <- tibble(mass = seq(from = 30, to = 65, length.out = 200))

# redifine the function
make_lines <- function(row){
  my_fit <-
    d %>%
    filter(row_number() != row) %>% #dropping the rows in tidyverse d %>% filter(row_number() != 2)
    lm(formula = brain ~ mass + I(mass^2) + I(mass^3) + I(mass^4) + I(mass^5) + I(mass^6))
  
  predict(my_fit, nd) %>% 
    as_tibble() %>% 
    rename(brain = value) %>% 
    bind_cols(nd)
}

# make our new tibble
lines <-
  tibble(row = 1:7) %>% 
  mutate(p = purrr::map(row, make_lines)) %>% 
  unnest(p)

# plot!
p +
  geom_line(data = lines, 
            aes(group = row),
            color = carto_pal(7, "BurgYl")[6], alpha = 1/2, size = 1/2) +
  coord_cartesian(ylim = -300:2000)

##Underfitting
#making the simplest model
b6.7 <- lm(data = d, 
           brain ~ 1)

summary(b6.7)
#displaying that there is no variance explained
glance(b6.7)
#plotting the model
p +
  stat_smooth(method = "lm", fullrange = TRUE, level = .89,
              color = carto_pal(7, "BurgYl")[6], fill = carto_pal(7, "BurgYl")[6], 
              size = 1/2, alpha = 1/3,              
              formula = y ~ 1) +
  ggtitle(NULL, subtitle = expression(paste(italic(R)^2, " = 0")))


#sensitivity plot
# because these lines are straight, we only need new data over two points of `mass`
nd <- tibble(mass = c(30, 70))

make_lines <- function(row){
  my_fit <-
    d %>%
    filter(row_number() != row) %>% 
    lm(formula = brain ~ mass)
  
  predict(my_fit, nd) %>% 
    as_tibble() %>% 
    rename(brain = value) %>% 
    bind_cols(nd)
}
#redefining a list of lines
(
  lines <-
    tibble(row = 1:7) %>% 
    mutate(p = purrr::map(row, make_lines)) %>% 
    unnest(p)
)

#the plot
p + 
  scale_x_continuous(expand = c(0, 0)) +
  geom_line(data = lines, 
            aes(x = mass, y = brain, group = row),
            color = carto_pal(7, "BurgYl")[6], alpha = 1/2, size = 1/2)

##--Computing deviance--##
#...brms...
#Standadizing body mass
d <-
  d %>%
  mutate(mass_s = (mass - mean(mass)) / sd(mass))

# Here we specify our starting values
inits <- list(Intercept = mean(d$brain),
              mass_s    = 0,
              sigma     = sd(d$brain))

inits_list <- list(inits, inits, inits, inits)

# The model
b6.8 <- 
  brm(data = d, family = gaussian,
      brain ~ 1 + mass_s,
      prior = c(prior(normal(0, 1000), class = Intercept),
                prior(normal(0, 1000), class = b),
                prior(cauchy(0, 10), class = sigma)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      inits = inits_list,  # here we insert our start values
      seed = 6)

print(b6.8)

#computing the loglikelihood for all the parameter
ll <-
  b6.8 %>%
  log_lik() %>%
  as_tibble(.name_repair = ~ d$species)

ll %>%
  glimpse()
#computing deviance from ll
ll <-
  ll %>%
  mutate(sums     = rowSums(.),
         deviance = -2 * sums)
#plot of the deviance distribution
ll %>%
  ggplot(aes(x = deviance, y = 0)) +
  geom_halfeyeh(fill = carto_pal(7, "BurgYl")[5], color = carto_pal(7, "BurgYl")[7],
                point_interval = median_qi, .width = .95) +
  scale_x_continuous(breaks = quantile(ll$deviance, c(.025, .5, .975)),
                     labels = quantile(ll$deviance, c(.025, .5, .975)) %>% round(1)) +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(title = "The deviance distribution") +
  theme_classic() +
  theme(text = element_text(family = "Courier"),
        panel.background = element_rect(fill = alpha(carto_pal(7, "BurgYl")[3], 1/4)))

#getting the out-of-sample deviance by simulating data
n       <- 20
kseq    <- 1:5 #folds?
# I've reduced this number by one order of magnitude to reduce computation time
n_sim   <- 1e3
n_cores <- 4

# here's our dev object based on `N <- 20`
dev_20 <-
  sapply(kseq, function(k) {
    print(k);
    r <- mcreplicate(n_sim, sim.train.test(N = n, k = k),
                     mc.cores = n_cores);
    c(mean(r[1, ]), mean(r[2, ]), sd(r[1, ]), sd(r[2, ]))
  })

# here's our dev object based on N <- 100
n       <- 100
dev_100 <- 
  sapply(kseq, function(k) {
    print(k);
    r <- mcreplicate(n_sim, sim.train.test(N = n, k = k), 
                     mc.cores = n_cores);
    c(mean(r[1, ]), mean(r[2, ]), sd(r[1, ]), sd(r[2, ]))
  })

#convert them to tibbles, bind them together, and wrangle extensively before we’re ready to plot.
dev_tibble <-
  dev_20 %>% 
  as_tibble() %>% 
  bind_rows(
    dev_100 %>%
      as_tibble()
  ) %>% 
  mutate(n         = rep(c("n = 20", "n = 100"), each = 4),
         statistic = rep(c("mean", "sd"), each = 2) %>% rep(., times = 2),
         sample    = rep(c("in", "out"), times = 2) %>% rep(., times = 2)) %>% 
  gather(n_par, value, -n, -statistic, -sample) %>% 
  spread(key = statistic, value = value) %>% 
  mutate(n     = factor(n, levels = c("n = 20", "n = 100")),
         n_par = str_remove(n_par, "V") %>% as.double()) %>% 
  mutate(n_par = ifelse(sample == "in", n_par - .075, n_par + .075))

head(dev_tibble)

##creating the plot
# this intermediary tibble will make `geom_text()` easier
dev_text <-
  dev_tibble %>% 
  filter(n_par > 1.5, 
         n_par < 2.5) %>% 
  mutate(n_par = ifelse(sample == "in", n_par - .2, n_par + .28))

# the plot
dev_tibble %>% 
  ggplot(aes(x     = n_par, y = mean,
             ymin  = mean - sd, ymax = mean + sd,
             group = sample,
             color = sample, 
             fill  = sample)) +
  geom_pointrange(shape = 21) +
  geom_text(data = dev_text,
            aes(label = sample)) +
  scale_color_manual(values = c(carto_pal(7, "BurgYl")[7], carto_pal(7, "BurgYl")[5])) +
  scale_fill_manual(values  = c(carto_pal(7, "BurgYl")[5], carto_pal(7, "BurgYl")[7])) +
  labs(x = "number of parameters",
       y = "deviance") +
  theme_classic() +
  theme(text             = element_text(family = "Courier"),
        legend.position  = "none",
        strip.background = element_rect(fill = alpha(carto_pal(7, "BurgYl")[1], 1/4), color = "white"),
        panel.background = element_rect(fill = alpha(carto_pal(7, "BurgYl")[3], 1/4))) +
  facet_wrap(~n, scale = "free_y")

##Regularizing priors
#displaying different priors in terms on conservatism
tibble(x = seq(from = - 3.5, 
               to   = 3.5, 
               by   = .01)) %>%
  
  ggplot(aes(x = x, ymin = 0)) +
  geom_ribbon(aes(ymax = dnorm(x, mean = 0, sd = 0.2)), 
              fill = carto_pal(7, "BurgYl")[7], alpha = 1/2) +
  geom_ribbon(aes(ymax = dnorm(x, mean = 0, sd = 0.5)), 
              fill = carto_pal(7, "BurgYl")[6], alpha = 1/2) +
  geom_ribbon(aes(ymax = dnorm(x, mean = 0, sd = 1)), 
              fill = carto_pal(7, "BurgYl")[5], alpha = 1/2) +
  scale_y_continuous(NULL, breaks = NULL) +
  xlab("parameter value") +
  coord_cartesian(xlim = c(-3, 3)) +
  theme_classic() +
  theme(text = element_text(family = "Courier"),
        panel.background = element_rect(fill = alpha(carto_pal(7, "BurgYl")[3], 1/4)))


#making the simulation with different priors
# I've reduced this number by one order of magnitude to reduce computation time
n_sim <- 1e3
#Making a function to simulate
make_sim <- function(n, b_sigma){
  sapply(kseq, function(k) {
    print(k);
    # this is an augmented line of code
    r <- mcreplicate(n_sim, sim.train.test(N = n, k = k, b_sigma = b_sigma),
                     mc.cores = n_cores);
    c(mean(r[1, ]), mean(r[2, ]), sd(r[1, ]), sd(r[2, ])) }) %>% 
    
    # this is a new line of code
    as_tibble()
}

s <-
  tibble(n       = rep(c(20, 100), each = 3),
         b_sigma = rep(c(1, .5, .2), times = 2)) %>% 
  mutate(sim     = map2(n, b_sigma, make_sim)) %>% 
  unnest()

# wrangle the simulation data
s %>% 
  mutate(statistic = rep(c("mean", "sd"), each = 2) %>% rep(., times = 3 * 2),
         sample    = rep(c("in", "out"), times = 2) %>% rep(., times = 3 * 2)) %>% 
  gather(n_par, value, -n, -b_sigma, -statistic, -sample) %>% 
  spread(key = statistic, value = value) %>% 
  mutate(n     = str_c("n = ", n) %>% factor(., levels = c("n = 20", "n = 100")),
         n_par = str_remove(n_par, "V") %>% as.double())  %>% 
  
  # now plot
  ggplot(aes(x = n_par, y = mean,
             group = interaction(sample, b_sigma))) +
  geom_line(aes(color = sample, size = b_sigma %>% as.character())) +
  # this function contains the data from the previous simulation
  geom_point(data = dev_tibble, 
             aes(group = sample, fill = sample),
             color = "black", shape = 21, size = 2.5, stroke = .1) +
  scale_fill_manual(values = c(carto_pal(7, "BurgYl")[7], carto_pal(7, "BurgYl")[5])) +
  scale_color_manual(values = c(carto_pal(7, "BurgYl")[7], carto_pal(7, "BurgYl")[5])) +
  scale_size_manual(values = c(1, .5, .2)) +
  labs(x = "number of parameters",
       y = "deviance") +
  theme_classic() +
  theme(text             = element_text(family = "Courier"),
        legend.position  = "none",
        strip.background = element_rect(fill = alpha(carto_pal(7, "BurgYl")[1], 1/4), color = "white"),
        panel.background = element_rect(fill = alpha(carto_pal(7, "BurgYl")[3], 1/4))) +
  facet_wrap(~n, scale = "free_y")

