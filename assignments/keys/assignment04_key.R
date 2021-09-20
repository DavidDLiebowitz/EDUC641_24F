# EDUC 641 Assignment 4 key

if (!require(pacman)) install.packages('pacman', repos = 'https://cran.rstudio.com')
pacman::p_load(tidyverse, knitr)

pd <- read_csv(here::here("data", "cont.csv"))

head(pd)

pd$treat <- factor(pd$treat)

# 1. Descriptive Statistics

# 1.1. 
skimr::skim(pd %>% select(-tchid))

pd %>% 
  select(-tchid) %>% 
  gtsummary::tbl_summary(statistic = list(coursework ~ "{mean} ({sd})",
                                          vocabulary ~ "{mean} ({sd})",
                                          treat ~ "{n} / {N} ({p}%)")) %>%
  gtsummary::modify_header(label ~ "**Variables**") 

# 1.2. 

pd %>% 
  select(-tchid, -coursework) %>% 
  gtsummary::tbl_summary(by = treat,
                         statistic = list(vocabulary ~ "{mean} ({sd})")) %>%
  gtsummary::modify_header(label ~ "**Variables**")

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
pd$treat <- as.factor(pd$treat)
t.test(formula = vocabulary ~ treat, data = pd)
# alternatively,
t.test(pd$vocabulary[pd$treat == 0], pd$vocabulary[pd$treat == 1])

# 3. Research question (b)
cor.test(pd$coursework, pd$vocabulary, method = "pearson")

