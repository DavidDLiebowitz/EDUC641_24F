

library(pacman)
p_load(here, tidyverse, ggplot2, xaringan, knitr, kableExtra)

i_am("slides/EDUC641_2_code.r")

# Let's first access the data

df <- read.csv(here("data/deathpenalty.csv"))
# it is common in R to name our datasets short names 
# so as to reduce typing (df for dataframe)


# Understanding data structure
  

head(df)
str(df)

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


# Calculating percents and visualizing
prop <- prop.table(table(df$rvictim))
barplot(prop, xlab = "Race of Victim")



# Another way to visualize:  Let's start to get familiar with the beauty of `ggplot`
ggplot(df, aes(rdefend)) + geom_bar() +
         xlab("Race of Defendant")



