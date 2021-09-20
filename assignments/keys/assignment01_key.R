# EDUC 641 Assignment 1 key

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
                   labels = c("Control Group", "Treatment Group"))

pd$absenteeism <- factor(pd$absenteeism,
                         levels = c(0, 1),
                         labels = c("No", "Yes"))

pd$cgender <- factor(pd$cgender,
                     levels = c(0, 1),
                     labels = c("Male", "Female"))

# 3. Describe and summarize the two key variables

tbl <- table(pd$treat)
tbl

text(barplot(tbl,
             space = 1.2,
             xlab = "Participants",
             ylab = "Number of Participants",
             ylim = c(0, 600)),
     tbl + 16,
     tbl)

tbl2 <- round(prop.table(table(pd$absenteeism)), 2)
tbl2

text(barplot(tbl2,
             space = 1.2,
             xlab = "Chronically Absent",
             ylab = "Proportion of Participants",
             ylim = c(0, 1)),
     tbl2 + 0.05,
     tbl2)

