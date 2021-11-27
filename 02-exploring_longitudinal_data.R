##############################################################################
#                 Exploring Longitudinal Data on Change                      #
##############################################################################

# Person-level dataset -------------------------------------------------------

library(tidyverse)

tolerance <- read_csv("https://stats.idre.ucla.edu/wp-content/uploads/2016/02/tolerance1.txt", col_names = T)

head(tolerance, n = 16)



# With person-level data, each participant has a single row
tolerance %>% 
  count()

tolerance %>% 
  nrow()


# matrix Pearson's correlation 
cor(tolerance[ , 2:6]) %>%
  round(digits = 2)

psych::lowerCor(tolerance[ , 2:6])



# Person-period dataset -------------------------------------------------------
tolerance_pp <- read_csv("https://stats.idre.ucla.edu/wp-content/uploads/2016/02/tolerance1_pp.txt",
                         col_names = T)

tolerance_pp %>%
  slice(c(1:9, 76:80))


# With data like these, the simple use of count() or nrow() won’t help us 
# discover how many participants there are in the tolerance_pp data. One 
# quick way is to count() the number of distinct() id values.

tolerance_pp %>% 
  distinct(id) %>% 
  count()


## transformar a tabela person-level em person-period -------------------------
tolerance %>% 
  pivot_longer(-c(id, male, exposure),
               names_to = "age",
               values_to = "tolerance") %>% 
  mutate(age = str_remove(age, "tol") %>% as.integer()) %>% 
  arrange(id, age)


## transformar a tabela person-period em person-level -------------------------
tolerance_pp %>% 
  mutate(age = str_c("tol", age)) %>% 
  select(-time) %>% 
  pivot_wider(names_from = age, values_from = tolerance)
  




# Descriptive analysis of individual change over time -------------------------

tolerance_pp %>% 
  ggplot(aes(x = age, y = tolerance)) +
  geom_point() +
  coord_cartesian(ylim = c(1, 4)) + 
  theme(panel.grid = element_blank()) +
  facet_wrap(~id)

tolerance_pp %>% 
  ggplot(aes(x = age, y = tolerance)) +
  geom_point() +
  geom_line() + 
  coord_cartesian(ylim = c(1, 4)) + 
  theme(panel.grid = element_blank()) +
  facet_wrap(~id)

tolerance_pp %>% 
  ggplot(aes(x = age, y = tolerance)) +
  geom_point() +
  stat_smooth(method = "loess", se = F, span = .9) +
  coord_cartesian(ylim = c(1, 4)) + 
  theme(panel.grid = element_blank()) +
  facet_wrap(~id)
  

# Nested tibble ---------------------------------------------------------------
# pedagogical utility
by_id <-
  tolerance_pp %>%
  group_by(id) %>%
  nest()

by_id # que loko!!!

by_id$data[[1]]


by_id$data[[1]] %>% 
  mutate(age_minus_11 = age - 11)


## Modelo nested -----------------------------------------------------------
library(brms)

fit2.1 <-
  brm(data = by_id$data[[1]],
      formula = tolerance ~ 1 + time,
      prior = prior(normal(0, 2), class = b),
      iter = 4000, chains = 4, cores = 4,
      seed = 2,
      file = "fits/fit02.01")

print(fit2.1)
fit2.1$prior



fit2.2 <-
  update(fit2.1, 
         newdata = by_id$data[[2]],
         control = list(adapt_delta = .9),
         file = "fits/fit02.02")

print(fit2.2)

# coeficiente de determinacao (R2)
bayes_R2(fit2.2)

# posterior do R2
bayes_R2(fit2.2, summary = F) %>% 
  str()

# visualisacao da posterior do R2
bayes_R2(fit2.2, summary = F) %>% 
  data.frame() %>% 
  
  ggplot(aes(x = R2)) +
  geom_density(fill = "black") +
  scale_x_continuous(expression(italic(R)[Bayesian]^2), limits = c(0, 1)) +
  scale_y_continuous(NULL, breaks = NULL) +
  theme(panel.grid = element_blank())
## exite uma massiva incerteza 

# sumarizando
bayes_R2(fit2.2, summary = F) %>% 
  data.frame() %>% 
  summarise(mean   = mean(R2),
            median = median(R2),
            mode   = tidybayes::Mode(R2))

# By default, bayes_R2() returns the mean. You can get the median with the 
# robust = TRUE argument. To pull the mode, you’ll need to use summary = F
# and feed the results into a mode function, like tidybayes::Mode()





