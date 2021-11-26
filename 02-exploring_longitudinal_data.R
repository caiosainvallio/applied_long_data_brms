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












