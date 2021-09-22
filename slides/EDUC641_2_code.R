
library(here)
library(tidyverse)
library(ggplot2)


###This tells R where the script is located in relationship to the "root" directory of your project
# Using this command you can then use shortened versions of file pathways that will work across different users' systems
# A non-preferred alternative is to read the data in using the full filepath

i_am("slides/EDUC641_2_code.r")

# Let's first access the data

df <- read.csv(here("data/deathpenalty.csv"))
# it is common in R to name our datasets short names 
# so as to reduce typing (df for dataframe)

###############################################################################
####                    UNIT 1 Describing Categorical Data
###############################################################################

# Understanding data structure

df                 # typing the dataframe name spits out the full data file
names(df)          # names() lists the column names
head(df)           # head() prints to the console the first six records (you can alter this to show more or fewer)
str(df)            # str() provides basic details on the columns, their data type, etc.

# We know from our codebook what this long list of numbers means, so let's make our dataset a little more readable...

df$rvictim <- factor(df$rvictim,
                     levels = c(1,2), labels=c("Black", "White"))
df$rdefend <- factor(df$rdefend,
                     levels = c(1,2), labels=c("Black", "White"))
df$deathpen <- factor(df$deathpen, 
                      levels = c(0,1), labels = c("No", "Yes"))

# Look at the first 6 observations
head(df)

# The default is to see 6, but you can change that!!
head(df, 20)

#How many were sentenced to the death penalty?
table(df$deathpen)


# Summarizing data: Charts

counts <- table(df$deathpen)
barplot(counts, xlab = "Sentenced to Death?")


# Calculating proportions and visualizing
prop <- prop.table(table(df$rvictim))
barplot(prop, xlab = "Race of Victim")



# Another way to visualize:  Let's start to get familiar with the beauty of `ggplot`

# This is the basic version of the graph in the lecture slides
rd <- ggplot(df, aes(rdefend)) + geom_bar() +
         xlab("Race of Defendant")
rd

# Here we add bar value labels to the bars
rd + geom_text(aes(label = ..count..), stat='count', vjust = -0.5)

# And now can make it a little prettier
rd + geom_text(aes(label = ..count..), stat='count', vjust = -0.5) +
  theme_minimal(base_size = 16)

###############################################################################
####                    UNIT 2 Relationships Between Categorical Data
###############################################################################

# Two-way tables

table(df$deathpen, df$rvictim)

# Two-way table of percents

round(prop.table(table(df$deathpen, df$rvictim), margin=2)*100, 2)

# A grouped chart
ggplot(df, aes(x = rvictim,
              fill = deathpen)) +
              geom_bar(position = "dodge") + 
              xlab("Race of victim") +
               theme_minimal(base_size = 16)

# A two-way table with "marginal" values
two_way <- table(df$deathpen, df$rvictim)
addmargins(two_way)

## Constructing the "expected" values in R -- YOU DO NOT NEED TO KNOW HOW TO DO ANY OF THIS!!!
# df <- df %>% group_by(rvictim) %>% mutate(sum_rvictim = n())
# df <- df %>% group_by(deathpen) %>% mutate(sum_deathpen = n())
# 
# df <- ungroup(df) %>% mutate(prop_rvictim = sum_rvictim / n())
# df <- ungroup(df) %>% mutate(prop_deathpen = sum_deathpen / n())
# 
# df <- df %>% group_by(rvictim) %>% mutate(expec_death = sum_rvictim * prop_deathpen)
# 
# df2 <- df %>% group_by(deathpen, rvictim) %>% summarise(expec_death = round(mean(expec_death), 0))
# sum_ref <- df %>% group_by(deathpen) %>% summarise(Sum = n())
# df2 <- left_join(df2, sum_ref, by = c("deathpen"))
# 
# df3 <- df2 %>% spread(rvictim, expec_death) %>% relocate(Sum, .after = last_col())
# 
# sum_vict <- df %>% group_by(rvictim) %>% summarise(sum_rvictim = n())
# sum_vict <- sum_vict %>% spread(rvictim, sum_rvictim)
# tot <- ungroup(df) %>% summarise(Sum = n())
# death <- c("Sum") %>% as.data.frame() %>% setNames(c("deathpen"))
# 
# sum_vict <- cbind(death, sum_vict, tot)
# 
# df3 <- rbind(df3, sum_vict)
# 
# kable(df3, format='html')

## Sub-setting the data to convicted Black defendants

df_b <- filter(df, rdefend == "Black")
table(df_b$deathpen, df_b$rvictim)

## Conduct the chi-square test
chi_b <- chisq.test(df_b$deathpen, df_b$rvictim)
chi_b

## Two-way table of observed frequencies
chi_b$observed

## Two-way table of expected frequencies
round(chi_b$expected, 0) # rounded

## Display the chi^2 statistic:
chi_b$statistic

## Display the p-value
chi_b$p.value
