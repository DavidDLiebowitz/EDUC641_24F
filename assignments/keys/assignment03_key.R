# Assignment 3 key

# 1. Dataset

if (!require(pacman)) install.packages('pacman', repos = 'https://cran.rstudio.com')
pacman::p_load(tidyverse, knitr)

pd <- read_csv(here::here("data", "cont.csv"))

# 2. Understand the structure of the data

head(pd)
# head(pd, 10) to show the first 10 observations
# head(pd %>% arrange(desc(vocabulary)), 10) to show 10 observations with highest vocabulary scores

str(pd)

pd$treat <- factor(pd$treat)

# 2. Descriptive statistics of the outcome variable

# 2.1. Central tendency

mean(pd$vocabulary)
# mean(pd$vocabulary, na.rm = TRUE) to ignore the missing values

median(pd$vocabulary)

# pd %>%
#   select(vocabulary) %>%
#   group_by(vocabulary) %>%
#   mutate(n = n()) %>%
#   arrange(desc(n)) %>%
#   head(10)

table(cut(pd$vocabulary, seq(min(pd$vocabulary), max(pd$vocabulary), by = 5)))

hist(pd$vocabulary,
     xlab = "Student Vocabulary Score",
     ylab = "Frequency",
     main = "Distribution of Vocabulary (Histogram)",
     cex.main = 1.2)

plot(density(pd$vocabulary),
     xlab = "Student Vocabulary Score",
     ylab = "Proportion",
     main = "Distribution of Vocabulary (Kernel Density Plot)",
     cex.main = 1.2)

# 2.2. Variability

var(pd$vocabulary)

sd(pd$vocabulary)

quantile(pd$vocabulary, probs = c(.25, .5, .75))
# round(quantile(pd$vocabulary, probs = c(.25, .5, .75)), 2)

quantile(pd$vocabulary, probs = seq(.1, .9, by = .1))
# round(quantile(pd$vocabulary, probs = seq(.1, .9, by = .1)), 2)

IQR(pd$vocabulary)

range(pd$vocabulary)

# 3. Inferential statistics of the outcome variable

# 3.1
set.seed(123)
pd$vocabulary_random <- rnorm(length(pd$vocabulary), mean(pd$vocabulary), sd(pd$vocabulary))

plot(density(pd$vocabulary_random),
     xlab = "Student Vocabulary Score",
     ylab = "Proportion",
     main = "Distribution of Random Sample Score",
     cex.main = 1.2)

ggplot(pd, aes(vocabulary)) +
  geom_density(color = "cornflowerblue") + 
  geom_density(aes(vocabulary_random), color = "coral2") +
  labs(x = "Student Vocabulary Score",
       y = "Proportion",
       title = "Compare Distributions (Blue: Observed, Red: Random Sample from Population)") +
  theme_classic()
  
# 3.2.

mean(pd$vocabulary_random)
mean(pd$vocabulary)

t.test(pd$vocabulary, mu = mean(pd$vocabulary_random), alternative = "two.sided")

