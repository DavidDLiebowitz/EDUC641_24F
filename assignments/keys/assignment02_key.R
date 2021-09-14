# EDUC 641 Assignment 2 key

# 1. Dataset

if (!require(pacman)) install.packages('pacman', repos = 'https://cran.rstudio.com')
pacman::p_load(tidyverse, knitr)

pd <- read_csv(here::here("data", "cat.csv"))

pd$treat <- factor(pd$treat,
                   levels = c(0, 1),
                   labels = c("Control Group", "Treatment Group"))

pd$absenteeism <- factor(pd$absenteeism,
                         levels = c(0, 1),
                         labels = c("No", "Yes"))

pd$cgender <- factor(pd$cgender,
                     levels = c(0, 1),
                     labels = c("Male", "Female"))

# 2. Observed relationship between *treat* and *absenteeism*

table(pd$absenteeism, pd$treat) 

round(prop.table(table(pd$absenteeism, pd$treat), margin=2), 2)

ggplot(pd, aes(x = absenteeism, fill = treat)) +
  geom_bar(position = "dodge") +
  xlab("Chronically Absent") +
  ylab("Number of Children") +
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.2, position = position_dodge(.9), color = "grey30")

# 3. Chi-Squared statistic

percent_yes <- sum(pd$absenteeism == "Yes")/nrow(pd)
percent_no <- sum(pd$absenteeism == "No")/nrow(pd)

percent_yes
percent_no

total_treat <- sum(pd$treat == "Treatment Group")
total_control <- sum(pd$treat == "Control Group")

control_no <- round(total_control*percent_no, 0)
treat_no <- round(total_treat*percent_no, 0)
control_yes <- round(total_control*percent_yes, 0)
treat_yes <- round(total_treat*percent_yes, 0)

margin <- matrix(c(control_no, treat_no, control_yes, treat_yes), ncol = 2, byrow = TRUE)
rownames(margin) <- c("No", "Yes")
colnames(margin) <- c("Control Group", "Treatment Group")
as.table(margin)

chisq.test(pd$absenteeism, pd$treat)

# 4. Sub-group comparisons

pd_female <- pd %>% 
  filter(cgender == "Female")

chisq.test(pd_female$absenteeism, pd_female$treat)

pd_male <- pd %>% 
  filter(cgender == "Male")

chisq.test(pd_male$absenteeism, pd_male$treat)

