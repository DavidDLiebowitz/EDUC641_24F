##########################################################################################
# EDUC 641 Assignment 3 Key 
## David Liebowitz, Claire Zhang, Havisha Khurana
### First created: 8/1/21
### Last update: 11/12/24
### Inputs: cont.csv
### Purpose: load in data, understand structure, conduct descriptive analysis
##########################################################################################

# Load necessary packages
library(tidyverse)
library(here)

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


#######################################################
## 2. Descriptive statistics of the outcome variable

# 2.1. Mean/median of vocabulary

mean(pd$vocabulary, na.rm = T)

median(pd$vocabulary, na.rm = T)

# 2.2 Mean/median of coursework

mean(pd$coursework, na.rm = T)

median(pd$coursework, na.rm = T)

# 2.3 Distribution of vocabulary

#### A few ways to do this

# Base R histogram
hist(pd$vocabulary,
     xlab = "Student Vocabulary Score",
     ylab = "Frequency",
     main = "Distribution of Vocabulary (Histogram)")

# Base R density plot
plot(density(pd$vocabulary),
     xlab = "Student Vocabulary Score",
     ylab = "Proportion",
     main = "Distribution of Vocabulary")

# ggplot2 density plot
vocab <- ggplot(pd, aes(vocabulary)) + 
            xlab("Student Vocabulary Score") +
            ylab("Proportion") +
            geom_density() +
            theme_minimal()

## we can add to this last plot vertical lines at the mean/median

vocab + 
  geom_vline(xintercept = 88.12, color="blue") +
  geom_vline(xintercept = 88.35, color="red") +
  annotate('text', label = "Median", color = "red", x = 92, y = 0.003, size = 4) +
  annotate('text', label = "Mean", color = "blue", x = 85, y = 0.003, size = 4)

# Saving the last plot
ggsave("assignments/keys/vocabulary.png")

# 2.4. Range and IQR
IQR(pd$vocabulary)

range(pd$vocabulary)

quantile(pd$vocabulary)


# 2.5 Variance/SD

var(pd$vocabulary)

sd(pd$vocabulary)


##############################
### Transformations

# 3.1 Standardize coursework and vocabulary

# Coursework
pd$std_coursework <- 
      (pd$coursework - mean(pd$coursework)) / 
            sd(pd$coursework)
# Vocabulary
pd$std_vocabulary <- 
  (pd$vocabulary - mean(pd$vocabulary)) / 
     sd(pd$vocabulary)

# 3.2 Compare coursework and vocabulary
mean(subset(pd$std_coursework,
            pd$tchid=="1832"))
# -3.365239

mean(subset(pd$std_vocabulary,
            pd$tchid=="1832"))
# -1.435849

###################################################################
### Compare observed mean of vocabulary to population mean

# 4.2 t-test
t.test(pd$vocabulary, mu = 87, alternative = "two.sided")


