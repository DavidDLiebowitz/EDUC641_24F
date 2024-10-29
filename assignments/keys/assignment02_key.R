##########################################################################################
# EDUC 641 Assignment 2 Key 
## David Liebowitz, Claire Zhang, Havisha Khurana
### First created: 8/1/21
### Last update: 10/29/24
### Inputs: cat.csv
### Purpose: load in data, understand structure, prepare descriptive tables and figures
##########################################################################################

# Load necessary packages
library(tidyverse)
library(here)

### 1. Read in the data
pd <- read.csv(here("data", "cat.csv"))

pd$treat <- factor(pd$treat,
                   levels = c(0, 1),
                   labels = c("Control", "Treatment"))

pd$absenteeism <- factor(pd$absenteeism,
                         levels = c(0, 1),
                         labels = c("No", "Yes"))

pd$cgender <- factor(pd$cgender,
                     levels = c(0, 1),
                     labels = c("Male", "Female"))


### 2. Observed relationship between *treat* and *absenteeism*

# 2.1 How many students in each group?
table(pd$absenteeism, pd$treat) 

# 2.2 What percent of students in each group?
round(prop.table(table(pd$absenteeism, pd$treat), margin=2)*100, 2)

# 2.3 Figure of numbers in each group
ggplot(pd, aes(x = treat, fill = absenteeism)) +
  geom_bar(position = "dodge") +
  xlab("Treatment status") +
  ylab("Number of children") +
  geom_text(aes(label = after_stat(count)), stat = "count", vjust = -0.2, position = position_dodge(.9), color = "grey30") +
  scale_fill_discrete(name = "Chronically absent?")

ggsave(filename = "assignments/keys/treat_absent.png")

### 3. Chi-Squared statistic

# 3.2 2x2 EXPECTED contingency table

## Note: this is a way to construct the table in R,
## In your assignment, you could either construct the
## table by hand or via the chisq.test command
percent_yes <- sum(pd$absenteeism == "Yes")/nrow(pd)
percent_no <- sum(pd$absenteeism == "No")/nrow(pd)

percent_yes
percent_no

total_treat <- sum(pd$treat == "Treatment")
total_control <- sum(pd$treat == "Control")

control_no <- round(total_control*percent_no, 0)
treat_no <- round(total_treat*percent_no, 0)
control_yes <- round(total_control*percent_yes, 0)
treat_yes <- round(total_treat*percent_yes, 0)

margin <- matrix(c(control_no, treat_no, control_yes, treat_yes), ncol = 2, byrow = TRUE)
rownames(margin) <- c("No", "Yes")
colnames(margin) <- c("Control", "Treatment")
as.table(margin)

# 3.3 and 3.4 Chi-square statistic and goodness-of-fit test
chisq.test(pd$absenteeism, pd$treat)

# Can do this without the Yates continuity and get the same value as in 3.3
chisq.test(pd$absenteeism, pd$treat, correct=FALSE)

# 4. Sub-group comparisons

# 4.1 Female students

# Filter to just the female students
pd_female <- pd %>% 
  filter(cgender == "Female")

chisq.test(pd_female$absenteeism, pd_female$treat)

# 4.2 Male students
pd_male <- pd %>% 
  filter(cgender == "Male")

chisq.test(pd_male$absenteeism, pd_male$treat)