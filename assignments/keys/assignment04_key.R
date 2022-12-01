##########################################################################################
# EDUC 641 Assignment 4 Key 
## David Liebowitz, Claire Zhang, Havisha Khurana
### First created: 8/1/21
### Last update: 11/30/22
### Inputs: cont.csv
### Purpose: load in data, understand structure, conduct descriptive analysis
##########################################################################################

# Load necessary packages
library(tidyverse)
library(here)
library(modelsummary)

####################################
#  1. Read in the data and convert

pd <- read.csv(here("data/cont.csv"))

# Inspect the data
head(pd)
str(pd)

# Convert tchid to factor (not strictly necessary)
pd$tchid <- as.factor(as.character(pd$tchid))

# Convert treat to factor
pd$treat <- factor(pd$treat,
                   levels = c(0, 1),
                   labels = c("Control", "Treatment"))


# 1. Descriptive Statistics

# 1.1. 

# Numeric variables
datasummary_skim(pd,
                 histogram = F,
                 output = "Assignments/keys/table1.docx")

# Categorical variables
datasummary_skim(pd,
                 type = "categorical",
                 output = "Assignments/keys/table1a.docx")

# Could also do it this way to show vocabulary and coursework by treatment status
datasummary_balance(~ treat,
                    data=pd)



# 1.2. 

pd %>% 
  ggplot(aes(coursework, vocabulary)) +
  geom_point(color = "cornflowerblue") +
  geom_smooth(method = lm, se = F, color = "deeppink") +
  labs(x = "Coursework Attendance (sessions)",
       y = "Average Student Score") +
  theme_minimal(base_size = 14)

ggsave("Assignments/keys/bivariate.png")



# 2. Research question

# 2.2 OLS fit

fit <- lm(vocabulary ~ coursework,
          data = pd)

summary(fit)

modelsummary(fit, stars=T,
             gof_omit = "Adj.|AIC|BIC|RMSE",
             coef_rename = c("coursework" = "Coursework Attendance (sessions)"),
             notes = c("Notes: Cells represent coefficients and standard errors in parentheses."),
             output = "Assignments/keys/table2.docx")

# 2.4 Regression assumptions

pd$predict <- predict(fit)
pd$resid <- residuals(fit)

summary(pd$resid)
sd(pd$resid)

boxplot(resid(fit))

hist(pd$resid)

ggplot(data=pd,
       aes(resid)) +
       geom_density() +
       xlab("Raw residuals") + ylab("Proportion") +
       theme_minimal(base_size = 16)

ggsave("Assignments/keys/resid_distr.png")


# Residuals v. fitted plot
ggplot(data=pd,
       aes(x = predict, y = resid)) +
       geom_point() +
       geom_hline(yintercept = 0, color = "red", linetype="dashed") +
       ylab("Residuals") + xlab("Fitted values") +
       theme_minimal(base_size = 16)

ggsave("Assignments/keys/resid_fit.png")
