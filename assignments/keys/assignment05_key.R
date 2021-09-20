# EDUC 641 Assignment 5 key

if (!require(pacman)) install.packages('pacman', repos = 'https://cran.rstudio.com')
pacman::p_load(tidyverse, knitr)

# 1. Mentor relates to GPA above 3.0?

ah01 <- read_csv(here::here("data", "ah01.csv"))

head(ah01)

ah01$mentor <- factor(ah01$mentor)
ah01$gpa_3 <- factor(ah01$gpa_3)

ah01 %>% 
  select(mentor, gpa_3) %>% 
  gtsummary::tbl_summary(statistic = list(gpa_3 ~ "{n} / {N} ({p}%)",
                                          mentor ~ "{n} / {N} ({p}%)")) %>%
  gtsummary::modify_header(label ~ "**Variables**") 

ggplot(ah01, aes(x = gpa_3, fill = mentor)) +
  geom_bar(position = "dodge") +
  xlab("GPA above 3.0") +
  ylab("Number of individuals") +
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.2, position = position_dodge(.9), color = "grey30")

chisq.test(ah01$mentor, ah01$gpa_3)
# 2. Mentor relates to high school GPA?

ah01 %>% 
  select(mentor, gpa) %>% 
  gtsummary::tbl_summary(statistic = list(gpa ~ "{mean} ({sd})",
                                          mentor ~ "{n} / {N} ({p}%)")) %>%
  gtsummary::modify_header(label ~ "**Variables**") 


ah01 %>% 
  mutate(mentor = factor(mentor, levels = c(0, 1), labels = c("No", "Yes"))) %>% 
  ggplot(aes(mentor, gpa)) +
  geom_boxplot(fill = "cornflowerblue") +
  labs(x = "Whether Had A Mentor",
       y = "High School GPA") +
  theme_bw(base_size = 14)

t.test(formula = gpa ~ mentor, data = ah01)

# 3. Mentee age relates to high school GPA?

ah02 <- read_csv(here::here("data", "ah02.csv"))

head(ah02)

ah02 %>% 
  select(mentee_age, gpa) %>% 
  gtsummary::tbl_summary(statistic = list(gpa ~ "{mean} ({sd})",
                                          mentee_age ~ "{mean} ({sd})")) %>%
  gtsummary::modify_header(label ~ "**Variables**")

ah02 %>% 
  ggplot(aes(mentee_age, gpa)) +
  geom_point(color = "cornflowerblue") +
  geom_smooth(method = lm, se = F, color = "brown") +
  labs(x = "Earliest Age Interacting with Mentor",
       y = "High School GPA") +
  theme(text=element_text(size = 20)) +
  theme_bw(base_size = 14)

cor.test(ah02$gpa, ah02$mentee_age, method = "pearson")


