####### INST314 -- HW03 data wrangling (fastfood) #####

### Cleaning workspace and loading packages ####
rm(list = ls())

# If tidyverse works for you, use:
# library(tidyverse)

# If tidyverse is broken, use the two you actually need:
library(dplyr)
library(ggplot2)

### Reading in data ####
# Read in the dataframe and call it fastfoodData.
fastfoodData <- read.csv("hw03/datasets/fastfood.csv", stringsAsFactors = TRUE)

# Take a quick look at the data
summary(fastfoodData)

### filter(), or subsetting observations ####

# Create a smaller dataframe named mcdonaldsData that only includes Mcdonalds
mcdonaldsData <- fastfoodData %>%
  filter(restaurant == "Mcdonalds")

# Look at calories from fat
summary(mcdonaldsData$cal_fat)

# ANSWER: The 25th percentile (Q1) of cal_fat for Mcdonalds is _160___.
# ANSWER: The 75th percentile (Q3) of cal_fat for Mcdonalds is __320__.


# Create a smaller dataframe named dairyqueenData that only includes Dairy Queen
dairyqueenData <- fastfoodData %>%
  filter(restaurant == "Dairy Queen")

summary(dairyqueenData$cal_fat)

# ANSWER: The 25th percentile (Q1) of cal_fat for Dairy Queen is 120____.
# ANSWER: The 75th percentile (Q3) of cal_fat for Dairy Queen is __310__.


# Create a dataframe with Dairy Queen OR Mcdonalds
dqMcdData <- fastfoodData %>%
  filter(restaurant == "Dairy Queen" | restaurant == "Mcdonalds")

### Boxplots ####
ggplot(dqMcdData, aes(x = restaurant, y = cal_fat)) +
  geom_boxplot()

### Single long command summary statistics (pipeline) ####
fastfoodData %>%
  filter(restaurant == "Dairy Queen" | restaurant == "Mcdonalds") %>%
  group_by(restaurant) %>%
  summarize(
    mean_cal_fat = mean(cal_fat, na.rm = TRUE),
    median_cal_fat = median(cal_fat, na.rm = TRUE),
    sd_cal_fat = sd(cal_fat, na.rm = TRUE),
    Q1 = quantile(cal_fat, 0.25, na.rm = TRUE),
    Q3 = quantile(cal_fat, 0.75, na.rm = TRUE),
    IQR = IQR(cal_fat, na.rm = TRUE),
    n = n(),
    min = min(cal_fat, na.rm = TRUE),
    max = max(cal_fat, na.rm = TRUE)
  )

# ANSWER (skew):
# Dairy Queen has the more skewed distribution of calories from fat.
# I can tell because:
# (1) mean (260) vs median (220) are farther apart for Dairy Queen
# (2) The upper side is longer: Q3 (310) to max (1270) compared to Q1 (120) to min (0)
# (3) The boxplot shows more/extreme outliers on the higher end (use the plot + numbers above).