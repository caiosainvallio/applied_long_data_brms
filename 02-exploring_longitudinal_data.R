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

# sumarizando de forma completa
bayes_R2(fit2.2, summary = F) %>% 
  data.frame() %>% 
  summarise(mean   = mean(R2),
            median = median(R2),
            mode   = tidybayes::Mode(R2),
            Q2.5   = quantile(R2, .025),
            q97.5  = quantile(R2, .975))

# By default, bayes_R2() returns the mean. You can get the median with the 
# robust = TRUE argument. To pull the mode, you’ll need to use summary = F
# and feed the results into a mode function, like tidybayes::Mode()

bayes_R2(fit2.2, robust = T) 
bayes_R2(fit2.2)



# serializacao dos modelos para rodar em paralelo
fits <- 
  by_id %>%
  mutate(model = map(data, ~update(fit2.1, newdata = ., seed = 2)))

fits

# pegando as informacaoes de todos
mean_structure <-
  fits %>% 
  mutate(coefs = map(model, ~ posterior_summary(.)[1:2, 1:2] %>% 
                       data.frame() %>% 
                       rownames_to_column("coefficients"))) %>% 
  unnest(coefs) %>% 
  select(-data, -model) %>% 
  unite(temp, Estimate, Est.Error) %>% 
  pivot_wider(names_from = coefficients,
              values_from = temp) %>% 
  separate(b_Intercept, into = c("init_stat_est", "init_stat_sd"), sep = "_") %>% 
  separate(b_time, into = c("rate_change_est", "rate_change_sd"), sep = "_") %>% 
  mutate_if(is.character, ~ as.double(.) %>% round(digits = 2)) %>% 
  ungroup()

head(mean_structure)


# extraindo a variancia resdual de todos
residual_variance <-
  fits %>% 
  mutate(residual_variance = map_dbl(model, ~ posterior_summary(.)[3, 1])^2) %>% 
  mutate_if(is.double, round, digits = 2) %>% 
  select(id, residual_variance)

head(residual_variance)


# extraindo o R2_Bayesian de todos
r2 <-
  fits %>% 
  mutate(r2 = map_dbl(model, ~ bayes_R2(., robust = T)[1])) %>% 
  mutate_if(is.double, round, digits = 2) %>% 
  select(id, r2)

head(r2)


# combine informations
table <-
  fits %>% 
  unnest(data) %>% 
  group_by(id) %>% 
  slice(1) %>% 
  select(id, male, exposure) %>% 
  left_join(mean_structure,    by = "id") %>% 
  left_join(residual_variance, by = "id") %>% 
  left_join(r2,                by = "id") %>% 
  rename(residual_var = residual_variance) %>% 
  select(id, init_stat_est:r2, everything()) %>% 
  ungroup()

table %>% 
  flextable::flextable() # LINDOOOOO!!!



# fitted initial status
table %>% 
  pull(init_stat_est) %>% 
  stem(scale = 2)

# fitted rate of change
table %>% 
  pull(rate_change_est) %>% 
  stem(scale = 2)

# residual variance
table %>% 
  pull(residual_var) %>% 
  stem(scale = 2)

# r2 statistic
table %>% 
  pull(r2) %>% 
  stem(scale = 2)



by_id %>% 
  unnest(data) %>% 
  
  ggplot(aes(x = time, y = tolerance, group = id)) +
  geom_point() +
  geom_abline(data = mean_structure,
              aes(intercept = init_stat_est,
                  slope = rate_change_est, group = id),
              color = "blue") +
  scale_x_continuous(breaks = 0:4, labels = 0:4 + 11) +
  coord_cartesian(ylim = c(0, 4)) +
  theme(panel.grid = element_blank()) +
  facet_wrap(~id)



# explorando diferencas entre as pessoas -----------------------------------


tolerance_pp %>%
  ggplot(aes(x = age, y = tolerance)) +
  stat_smooth(method = "loess", se = F, span = .9, size = 2) +
  stat_smooth(aes(group = id),
              method = "loess", se = F, span = .9, size = 1/4) +
  coord_cartesian(ylim = c(0, 4)) +
  theme(panel.grid = element_blank())


tolerance_pp %>%
  ggplot(aes(x = age, y = tolerance)) +
  stat_smooth(method = "lm", se = F, span = .9, size = 2) +
  stat_smooth(aes(group = id),
              method = "lm", se = F, span = .9, size = 1/4) +
  coord_cartesian(ylim = c(0, 4)) +
  theme(panel.grid = element_blank())


fit2.3 <-
  update(fit2.1, 
         newdata = tolerance_pp,
         file = "fits/fit02.03")
summary(fit2.3)
fixef(fit2.3)



tol_fitted <-
  mean_structure %>% 
  mutate(`11` = init_stat_est + rate_change_est * 0,
         `15` = init_stat_est + rate_change_est * 4) %>% 
  select(id, `11`, `15`) %>% 
  pivot_longer(-id, 
               names_to = "age", 
               values_to = "tolerance") %>% 
  mutate(age = as.integer(age))

head(tol_fitted)



tol_fitted %>% 
  ggplot(aes(x = age, y = tolerance, group = id)) +
  geom_line(color = "blue", size = 1/4) +
  geom_abline(intercept = fixef(fit2.3)[1, 1] + fixef(fit2.3)[2, 1] * -11,
              slope     = fixef(fit2.3)[2, 1],
              color = "blue", size = 2) +
  coord_cartesian(ylim = c(0, 4)) +
  theme(panel.grid = element_blank()) 



## questions about change ------------------------------------------------------

mean_structure %>% 
  pivot_longer(ends_with("est")) %>% 
  group_by(name) %>% 
  summarise(mean = mean(value),
            sd   = sd(value)) %>% 
  mutate_if(is.double, round, digits = 2)



mean_structure %>% 
  select(init_stat_est, rate_change_est) %>% 
  cor() %>% 
  round(digits = 2)



fit2.4 <-
  update(fit2.1, 
         newdata = tolerance_pp,
         tolerance ~ 1 + time + male + time:male,
         file = "fits/fit02.04")

print(fit2.4)


tol_fitted_male <-
  tibble(male = rep(0:1, each = 2),
         age  = rep(c(11, 15), times = 2)) %>% 
  mutate(time = age - 11) %>% 
  mutate(tolerance = fixef(fit2.4)[1, 1] + 
           fixef(fit2.4)[2, 1] * time + 
           fixef(fit2.4)[3, 1] * male + 
           fixef(fit2.4)[4, 1] * time * male)

tol_fitted_male


tol_fitted %>% 
  # we need to add `male` values to `tol_fitted`
  left_join(tolerance_pp %>% select(id, male),
            by = "id") %>% 
  
  ggplot(aes(x = age, y = tolerance, color = factor(male))) +
  geom_line(aes(group = id),
            size = 1/4) +
  geom_line(data = tol_fitted_male,
            size = 2) +
  scale_color_viridis_d(end = .75) +
  coord_cartesian(ylim = c(0, 4)) +
  theme(legend.position = "none",
        panel.grid = element_blank()) +
  facet_wrap(~male)


tolerance_pp <-
  tolerance_pp %>% 
  mutate(exposure_01 = if_else(exposure > median(exposure), 1, 0))

fit2.5 <-
  update(fit2.4, 
         newdata = tolerance_pp,
         tolerance ~ 1 + time + exposure_01 + time:exposure_01,
         file = "fits/fit02.05")

print(fit2.5)


tol_fitted_exposure <-
  crossing(exposure_01 = 0:1,
           age         = c(11, 15)) %>% 
  mutate(time = age - 11) %>% 
  mutate(tolerance = fixef(fit2.5)[1, 1] + 
           fixef(fit2.5)[2, 1] * time + 
           fixef(fit2.5)[3, 1] * exposure_01 + 
           fixef(fit2.5)[4, 1] * time * exposure_01,
         exposure = if_else(exposure_01 == 1, "high exposure", "low exposure") %>% 
           factor(., levels = c("low exposure", "high exposure")))

tol_fitted_exposure



tol_fitted %>% 
  # we need to add `exposure_01` values to `tol_fitted`
  left_join(tolerance_pp %>% select(id, exposure_01),
            by = "id") %>% 
  mutate(exposure = if_else(exposure_01 == 1, "high exposure", "low exposure") %>% 
           factor(., levels = c("low exposure", "high exposure"))) %>% 
  
  ggplot(aes(x = age, y = tolerance, color = exposure)) +
  geom_line(aes(group = id),
            size = 1/4) +
  geom_line(data = tol_fitted_exposure,
            size = 2) +
  scale_color_viridis_d(option = "A", end = .75) +
  coord_cartesian(ylim = c(0, 4)) +
  theme(legend.position = "none",
        panel.grid = element_blank()) +
  facet_wrap(~exposure)









p1 <-
  mean_structure %>% 
  pivot_longer(ends_with("est")) %>% 
  mutate(name = factor(name, labels = c("Fitted inital status", "Fitted rate of change"))) %>% 
  # we need to add `male` values to `tol_fitted`
  left_join(tolerance_pp %>% select(id, male),
            by = "id") %>% 
  
  ggplot(aes(x = factor(male), y = value, color = name)) +
  geom_point(alpha = 1/2) +
  scale_color_viridis_d(option = "B", begin = .2, end = .7) +
  labs(x = "male",
       y = NULL) +
  theme(legend.position = "none",
        panel.grid = element_blank()) +
  facet_wrap(~name, scale = "free_y", ncol = 1)

p1



p2 <-
  mean_structure %>% 
  pivot_longer(ends_with("est")) %>% 
  mutate(name = factor(name, labels = c("Fitted inital status", "Fitted rate of change"))) %>% 
  # we need to add `male` values to `tol_fitted`
  left_join(tolerance_pp %>% select(id, exposure),
            by = "id") %>% 
  
  ggplot(aes(x = exposure, y = value, color = name)) +
  geom_point(alpha = 1/2) +
  scale_color_viridis_d(option = "B", begin = .2, end = .7) +
  scale_x_continuous(breaks = 0:2,
                     limits = c(0, 2.4)) +
  labs(y = NULL) +
  theme(legend.position = "none",
        panel.grid = element_blank()) +
  facet_wrap(~name, scale = "free_y", ncol = 1)

p2




library(patchwork)

p1 + p2 + scale_y_continuous(breaks = NULL)
