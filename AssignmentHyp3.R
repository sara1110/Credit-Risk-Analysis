# Setting work directory
setwd("/Users/sara/Desktop/")

# Importing necessary libraries
library(dplyr)
library(ggplot2)
library(tidyverse)
library(readr)
library(corrplot)
library(ggcorrplot)
library(ggrepel)
library(caret)
library(grid)
library(ggnewscale)
library(ggtext)
library(shadowtext)
library(patchwork)
library(e1071)

# Load the data
data <- read.csv("assignment.csv") # Adjust file name/path if needed

# Check structure of the dataset
str(data)
head(data)

# Verify and convert relevant columns to factors if not already
data$age <- as.numeric(data$age) # Ensure 'age' is numeric
data$class <- as.factor(data$class)

# Bin the age variable for better visualization (e.g., 18-25, 26-35, etc.)
data$age_group <- cut(data$age, breaks = seq(0, 100, by = 10), right = FALSE, 
                      labels = c("0-9", "10-19", "20-29", "30-39", "40-49", 
                                 "50-59", "60-69", "70-79", "80-89", "90-99"))

# Create subsets for age groups based on 'class'
age_group_good <- data$age_group[data$class == "good"]
age_group_bad <- data$age_group[data$class == "bad"]

# Count occurrences for each age group and class
age_group_good_counts <- as.data.frame(table(age_group_good))
age_group_bad_counts <- as.data.frame(table(age_group_bad))

# Rename columns for clarity
colnames(age_group_good_counts) <- c("Age_Group", "Count")
colnames(age_group_bad_counts) <- c("Age_Group", "Count")

# Add grouping column to differentiate 'good' and 'bad'
age_group_good_counts$Group <- "Good"
age_group_bad_counts$Group <- "Bad"

# Combine the counts into one data frame
age_group_combined <- rbind(age_group_good_counts, age_group_bad_counts)

# Check combined data structure
print(age_group_combined)

# Plotting the stacked bar chart with custom colors
ggplot(age_group_combined, aes(x = Age_Group, y = Count, fill = Group)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Age Group vs Class", x = "Age Group", y = "Count") +
  scale_fill_manual(values = c("Good" = "skyblue", "Bad" = "navy")) + # Custom colors
  theme_classic()

########## Hypothesis Testing ##########
# Hypothesis:
# Null (H0): There is no association between age_group and class.
# Alternative (H1): There is an association between age_group and class.

# Create a contingency table
contingency_table <- table(data$age_group, data$class)

# Perform Chi-Square Test for Independence
chi_square_test <- chisq.test(contingency_table)

# Print test results
print(chi_square_test)


# Hypothesis:
# Null (H0): There is no linear relationship between age and class.
# Alternative (H1): There is a linear relationship between age and class.

# Convert class to numeric for regression (e.g., good = 1, bad = 0)
data$class_numeric <- ifelse(data$class == "good", 1, 0)

# Fit a linear regression model
linear_model <- lm(class_numeric ~ age, data = data)

# Print model summary
summary(linear_model)

# Plot the regression
ggplot(data, aes(x = age, y = class_numeric)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Linear Regression: Age vs Class", 
       x = "Age", 
       y = "Class (Numeric)") +
  theme_classic()
