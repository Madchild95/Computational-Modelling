###-----Rethinking: Chapter 7-----###

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
       ggthemes #to get the pander theme ;-)
)

#continuous interactions - the tulip example
data("tulips")
d <- tulips
str(d)

#centering the variables
d <-
  d %>%
  mutate(shade_c = shade - mean(shade),
         water_c = water - mean(water))

#fitting the models
b7.8 <-
  brm(data = d, family = gaussian,
      blooms ~ 1 + water_c + shade_c,
      prior = c(prior(normal(130, 100), class = Intercept),
                prior(normal(0, 100), class = b),
                prior(cauchy(0, 10), class = sigma)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      control = list(adapt_delta = 0.9),
      seed = 7)

b7.9 <- 
  update(b7.8, 
         formula = blooms ~ 1 + water_c + shade_c + water_c:shade_c)
#getting a nice summary like the rethinking "coef" function
tibble(model  = str_c("b7.", 8:9)) %>% 
  mutate(fit  = purrr::map(model, get)) %>% 
  mutate(tidy = purrr::map(fit, broom::tidy)) %>% 
  unnest(tidy) %>% 
  filter(term != "lp__") %>% 
  select(term, estimate, model) %>% 
  spread(key = model, value = estimate) %>% 
  mutate_if(is.double, round, digits = 2)

#plotting the interaction effect in a triptych
# loop over values of `water_c` and plot predictions
shade_seq <- -1:1

for(w in -1:1){
  # define the subset of the original data
  dt <- d[d$water_c == w, ]
  # defining our new data
  nd <- tibble(water_c = w, shade_c = shade_seq)
  # use our sampling skills, like before
  f <- 
    fitted(b7.9, newdata = nd) %>%
    as_tibble() %>%
    bind_cols(nd)
  
  # specify our custom plot
  fig <- 
    ggplot() +
    geom_smooth(data = f,
                aes(x = shade_c, y = Estimate, ymin = Q2.5, ymax = Q97.5),
                stat = "identity", 
                fill = "#CC79A7", color = "#CC79A7", alpha = 1/5, size = 1/2) +
    geom_point(data = dt, 
               aes(x = shade_c, y = blooms),
               shape = 1, color = "#CC79A7") +
    coord_cartesian(xlim = range(d$shade_c), 
                    ylim = range(d$blooms)) +
    scale_x_continuous("Shade (centered)", breaks = c(-1, 0, 1)) +
    labs("Blooms", 
         title = paste("Water (centered) =", w)) +
    theme_pander() + 
    theme(text = element_text(family = "Times"))
  
  # plot that joint
  plot(fig)
}

##with shade centered

for(s in -1:1){
  # define the subset of the original data
  dt <- d[d$shade_c == s, ]
  # defining our new data
  nd <- tibble(shade_c = s, water_c = shade_seq)
  # use our sampling skills, like before
  f <- 
    fitted(b7.9, newdata = nd) %>%
    as_tibble() %>%
    bind_cols(nd)
  
  # specify our custom plot
  fig <- 
    ggplot() +
    geom_smooth(data = f,
                aes(x = water_c, y = Estimate, ymin = Q2.5, ymax = Q97.5),
                stat = "identity", 
                fill = "#CC79A7", color = "#CC79A7", alpha = 1/5, size = 1/2) +
    geom_point(data = dt, 
               aes(x = water_c, y = blooms),
               shape = 1, color = "#CC79A7") +
    coord_cartesian(xlim = range(d$water_c), 
                    ylim = range(d$blooms)) +
    scale_x_continuous("Water (centered)", breaks = c(-1, 0, 1)) +
    labs("Blooms", 
         title = paste("Shade (centered) =", s)) +
    theme_pander() + 
    theme(text = element_text(family = "Times"))
  
  # plot that joint
  plot(fig)
}

#An easy depiction of the interactions using marginal_effects()
plot(conditional_effects(b7.9, 
                      effects = "shade_c:water_c"),
     points = T)

plot(conditional_effects(b7.9, 
                         effects = "water_c:shade_c"),
     points = T)


#Try including bed in the model 
d <- d %>% 
  mutate(
    bed = as.factor(bed)
  )
#update model to include bed
b7.10 <- 
  update(b7.8, 
         formula = blooms ~ 1 + water_c + shade_c + water_c:shade_c + bed,
         newdata = d)

#add IC's
b7.9 <- add_criterion(b7.9, "loo")
b7.10 <- add_criterion(b7.10, "loo")
#compare
loo_compare(b7.9,b7.10)
loo_model_weights(b7.9,b7.10)

# cbind() trick to convert from elpd metric to more traditional WAIC metric
cbind(waic_diff = w[, 1] * -2,
      se        = w[, 2] *  2)
