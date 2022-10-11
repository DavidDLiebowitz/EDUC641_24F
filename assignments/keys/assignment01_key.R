##########################################################################################
# EDUC 641 Assignment 1 Key 
## David Liebowitz, Claire Zhang, Havisha Khurana
### First created: 8/1/21
### Last update: 10/11/22
### Inputs: cat.csv
### Purpose: load in data, understand structure, prepare descriptive tables and figures
##########################################################################################

# Load necessary packages
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("here")

library(tidyverse)
library(ggplot2)
library(here)

# 1. Read in the dataset

pd <- read.csv(here("data", "cat.csv"))

# 2. Understand the structure of the data

# 2.1
names(pd)
head(pd)
# head(pd, 10) to show the first 10 observations
# head(pd %>% arrange(desc(vocabulary)), 10) to show 10 observations with highest vocabulary scores

str(pd)

#2.2

# Recode the variables treat, absenteeism, cgender into factor variables

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

# 3.1 Treatment/control counts
tbl <- table(pd$treat)
tbl

barplot(tbl,
        xlab = "Treatment status",
        ylab = "Number of Participants",
        ylim = c(0, 600))

## Can also do this via ggplot

ggplot(pd, aes(treat)) + geom_bar() +
        xlab("Treatment status") +
        ylab("Number of participants") +
        #Add bar labels   
        geom_text(aes(label = ..count..), stat='count', vjust = -0.5, size = 4) +
        #Make it look prettier
        ylim(0, 600) +
        theme_minimal()

# Save the plot
ggsave(filename = "assignments/keys/treat2.png")

# 3.2 Chronically absent proportion
tbl2 <- round(prop.table(table(pd$absenteeism)), 2)
tbl2

barplot(tbl2,
        xlab = "Chronically Absent",
        ylab = "Proportion of Participants",
        ylim = c(0, 1))

