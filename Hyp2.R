# Setting the working directory
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

# Load the data
data <- read.csv("assignment.csv") # Adjust file name/path if needed

# Check structure of the dataset
str(data)
head(data)

# Verify and convert relevant columns to factors if not already
data$housing <- as.factor(data$housing)
data$class <- as.factor(data$class)

########## Data Visualization ##########

# Create subsets for housing based on 'class'
housing_good <- data$housing[data$class == "good"]
housing_bad <- data$housing[data$class == "bad"]

# Count occurrences for each category
housing_good_counts <- as.data.frame(table(housing_good))
housing_bad_counts <- as.data.frame(table(housing_bad))

# Rename columns for clarity
colnames(housing_good_counts) <- c("Category", "Count")
colnames(housing_bad_counts) <- c("Category", "Count")

# Add grouping column to differentiate 'good' and 'bad'
housing_good_counts$Group <- "Good"
housing_bad_counts$Group <- "Bad"

# Combine the counts into one data frame
housing_combined <- rbind(housing_good_counts, housing_bad_counts)

# Check combined data structure
print(housing_combined)

# Plotting the bar chart
ggplot(housing_combined, aes(x = Category, y = Count, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Good" = "hotpink", "Bad" = "purple")) + # Custom colors
  labs(title = "Housing Status Distribution by Class",
       x = "Housing Status",
       y = "Count",
       fill = "Class") +
  theme_classic()

# Perform Chi-Square Test for Housing vs Class
contingency_table <- table(data$housing, data$class)
chi_square_test <- chisq.test(contingency_table)

# Print test results
print(chi_square_test)


