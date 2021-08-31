# EDUC 641 Assignment 4 key

if (!require(pacman)) install.packages('pacman', repos = 'https://cran.rstudio.com')
pacman::p_load(tidyverse, knitr)

pd <- read_csv(here::here("data", "cont2.csv"))

head(pd)

pd$treat <- factor(pd$treat)

# 1. Descriptive Statistics

# 1.1. 

summary(pd$vocabulary)
sd(pd$vocabulary)

summary(pd$coursework)
sd(pd$coursework)

summary(pd$treat)

# 1.2. 

pd_treat <- pd %>% 
  filter(treat == 1)

summary(pd_treat$vocabulary)
sd(pd_treat$vocabulary)

pd_control <- pd %>% 
  filter(treat == 0)

summary(pd_control$vocabulary)
sd(pd_control$vocabulary)

boxplot(pd_treat$vocabulary,
        main = "Treatment Group",
        xlab = "Vocabulary Score", 
        ylab = "Distribution")

boxplot(pd_control$vocabulary,
        main = "Control Group",
        xlab = "Vocabulary Score", 
        ylab = "Distribution")

# alternatively
pd %>% 
  mutate(treat = factor(treat, levels = c(0, 1), labels = c("Control Group", "Treatment Group"))) %>% 
  ggplot(aes(treat, vocabulary)) +
  geom_boxplot(fill = "cornflowerblue") +
  labs(x = "Consaltancy Groups",
       y = "Average Student Score") +
  theme_bw(base_size = 14)

# 1.3.

pd %>% 
  ggplot(aes(coursework, vocabulary)) +
  geom_point(color = "cornflowerblue") +
  labs(x = "Coursework Attendance",
       y = "Average Student Score") +
  theme(text=element_text(size = 20)) +
  theme_bw(base_size = 14)

# 2. Research question (a)

