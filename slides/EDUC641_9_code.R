library(here)
library(tidyverse)


### This tells R where the script is located in relationship to the "root" directory of your project
# Using this command you can then use shortened versions of file pathways that will work across different users' systems
# A non-preferred alternative is to read the data in using the full filepath

i_am("slides/EDUC641_9_code.r")


###############################################################################
####                    UNIT 3 Describing and Summarizing Continuous Data
###############################################################################


# Let's first access the data

who <- read.csv(here("data/life_expectancy.csv")) %>%
  # going to do some data cleaning; first making variable names take a common format
  janitor::clean_names() %>% 
  # filtering to focus only on 2015
  filter(year == 2015) %>%
  # selecting only the variables we need
  select(country, status, life_expectancy) %>% 
  # renaming one of the variables that is really misnamed
  rename(region = country) %>% 
  # rounding life expectancy to nearest year
  mutate(life_expectancy = round(life_expectancy, digits = 0))


# Transformation
who$life_expectancy_zscore <- 
  (who$life_expectancy - mean(who$life_expectancy)) /
  sd(who$life_expectancy)

# Look at the first and last observations in the data
head(who$life_expectancy_zscore)
hist(who$life_expectancy_zscore)

#### Compare transformed and raw data
# Raw
ggplot(who, aes(life_expectancy)) +
  geom_histogram()

# Standardized
who_stand <- ggplot(who, aes(life_expectancy_zscore)) +
  geom_histogram()

who_stand

#### Look at just the values for a single observations

# Can do this two ways
# (1) Filter
can <- filter(who, region == "Canada")
mean(can$life_expectancy)

# (2) Subset
mean(subset(who$life_expectancy, who$region == "Canada"))



#### Unit 3, Part 3: Conducting a one-sample t-test

# Going to focus in on just "developing" countries
low_inc <- filter(who, status=="Developing")

# Conduct a t-test against a known population mean of 71.64
t.test(low_inc$life_expectancy, mu = 71.64)

