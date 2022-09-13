library(here)
library(tidyverse)
library(ggplot2)


###This tells R where the script is located in relationship to the "root" directory of your project
# Using this command you can then use shortened versions of file pathways that will work across different users' systems
# A non-preferred alternative is to read the data in using the full filepath

i_am("slides/EDUC641_7_code.r")


###############################################################################
####                    UNIT 3 Describing and Summarizing Categorical Data
###############################################################################


# Let's first access the data

who <- read.csv(here("data/life_expectancy.csv")) %>% 
  janitor::clean_names() %>% 
  filter(year == 2015) %>% 
  select(country, status, life_expectancy) %>% 
  rename(region = country) %>% 
  mutate(life_expectancy = round(life_expectancy, digits = 0))



## Histogram
ggplot(who, aes(life_expectancy)) +
  geom_histogram()

# Stem-and-leaf
stem(who$life_expectancy)


# Density plot
ggplot(who, aes(life_expectancy)) +
  geom_density()


# IQR
ggplot(who, aes(x = life_expectancy)) +
  geom_density() +
  geom_vline(xintercept = quantile(who$life_expectancy, .25)) +
  geom_label(
    x = quantile(who$life_expectancy, .25),
    y = .03,
    label = paste("25th %ile =", quantile(who$life_expectancy, .25))
  ) +
  geom_vline(xintercept = quantile(who$life_expectancy, .75)) +
  geom_label(
    x = quantile(who$life_expectancy, .75),
    y = .03,
    label = paste("75th %ile =", quantile(who$life_expectancy, .75))
  ) +
  geom_segment(
    x = quantile(who$life_expectancy, .25),
    xend = quantile(who$life_expectancy, .75),
    y = .015,
    yend = .015,
    linetype = 2
  ) +
  geom_label(x = 71.5,
             y = 0.01,
             label = "IQR = 11") +
  theme_classic()


# Transformation
who$life_expectancy_zscore <- 
  (who$life_expectancy - mean(who$life_expectancy)) /
  sd(who$life_expectancy)

head(who$life_expectancy_zscore)
hist(who$life_expectancy_zscore)

#### Compare transformed and raw data
# Raw
ggplot(who, aes(life_expectancy)) +
  geom_histogram()

# Standardized
who_stand <- ggplot(who, aes(life_expectancy_zscore)) +
  geom_histogram()



# Boxplot
boxplot(who$life_expectancy, horizontal=T)


# Boxplot over categories
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
  theme_minimal()
