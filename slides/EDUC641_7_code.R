library(here)
library(tidyverse)


### This tells R where the script is located in relationship to the "root" directory of your project
# Using this command you can then use shortened versions of file pathways that will work across different users' systems
# A non-preferred alternative is to read the data in using the full filepath

i_am("slides/EDUC641_7_code.r")


###############################################################################
####                    UNIT 3 Describing and Summarizing Categorical Data
###############################################################################


# Let's first access the data

who <- read.csv(here("data/life_expectancy.csv")) %>%
  # going to do some data cleaning; first making variable names take a common format
  janitor::clean_names() %>% 
  # filtering to focus only on 2015
  filter(year == 2015) %>%
  # selecting only the variables we need
  select(country, status, life_expectancy, schooling) %>% 
  # renaming one of the variables
  rename(region = country) %>% 
  # rounding life expectancy to nearest year
  mutate(life_expectancy = round(life_expectancy, digits = 0))

############################################################
#####         Distributions visualized              ########
############################################################

##############
## Histogram

# Default histogram
hist(who$life_expectancy)

# Add more breaks
hist(who$life_expectancy, breaks = 16)

# Use ggplot
ggplot(who, aes(life_expectancy)) + 
  geom_histogram()

#################
## Stem-and-leaf
stem(who$life_expectancy)


##################
## Density plot
ggplot(who, aes(life_expectancy)) +
  geom_density()


#################
##  Boxplot
boxplot(who$life_expectancy, horizontal=T)


# Boxplot over categories (in this case, years)
## bring in a few more years

who_more <- read.csv(here("data/life_expectancy.csv")) %>% 
  janitor::clean_names() %>% 
  filter(year >= 2010 & year <= 2015) %>% 
  select(country, status, year, life_expectancy) %>% 
  rename(region = country) %>% 
  mutate(life_expectancy = round(life_expectancy, digits = 0))

who_more %>% 
  ggplot(aes(factor(year), life_expectancy, fill = factor(year))) +
  geom_boxplot() +
  theme_minimal() +
  theme(legend.position = "none")





############################################################
#####         Measures of central tendency          ########

mean(who$life_expectancy, na.rm = T)

## If there are missing values of life_expectancy (NA), we will get an error.
## So, we generally want to default to setting na.rm = T which tells R to ignore missing values

## Are there any NA?
sum(is.na(who$life_expectancy))

median(who$life_expectancy)




############################################################
#####         Measures of variation                 ########

range(who$life_expectancy)

IQR(who$life_expectancy)

quantile(who$life_expectancy)

# By default will give quartiles
# Can change with probs sub-command
# For example, by quintiles
quantile(who$life_expectancy, probs = seq(0, 1, 0.2))

