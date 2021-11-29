##############################################################################
#                              R Setup                                       #
##############################################################################

packages <- c("bayesplot", 
              "brms", 
              "broom", 
              "devtools", 
              "flextable", 
              "GGally", 
              "ggmcmc", 
              "ggrepel", 
              "gtools", 
              "loo", 
              "patchwork", 
              "psych", 
              "Rcpp", 
              "remotes", 
              "rstan", 
              "StanHeaders", 
              "survival", 
              "tidybayes", 
              "tidyverse")

install.packages(packages, dependencies = T)


devtools::install_github("stan-dev/cmdstanr")
remotes::install_github("stan-dev/posterior")
devtools::install_github("rmcelreath/rethinking")
