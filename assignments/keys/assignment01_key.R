# Assignment 1 key

# 1. Read in the dataset

if (!require(pacman)) install.packages('pacman', repos = 'https://cran.rstudio.com')
pacman::p_load(tidyverse, knitr)

pd <- read_csv(here::here("data", "cat.csv"))

# 2. Understand the structure of the data

head(pd)
# head(pd, 10) to show the first 10 observations
# head(pd %>% arrange(desc(vocabulary)), 10) to show 10 observations with highest vocabulary scores

str(pd)

pd$treat <- factor(pd$treat,
                   levels = c(0, 1),
                   labels = c("Control", "Treat"))

pd$absenteeism <- factor(pd$absenteeism,
                         levels = c(0, 1),
                         labels = c("No", "Yes"))

pd$cgender <- factor(pd$cgender,
                     levels = c(0, 1),
                     labels = c("Boy", "Girl"))

# 3. Describe and summarize the two key variables

table(pd$treat)

barplot(table(pd$treat),
        space = 1.5,
        xlab = "Experimental Group",
        ylab = "Number of Children")

prop.table(table(pd$absenteeism))

barplot(prop.table(table(pd$absenteeism)),
        space = 1.5,
        xlab = "Chronically Absent",
        ylab = "Proportion of Children")
