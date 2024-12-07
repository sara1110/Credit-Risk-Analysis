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

# Reading the CSV file
data <- read.csv("assignment.csv")

# Check structure and preview the data
str(data)
head(data)

# Convert relevant columns to factors if not already
data$foreign_worker <- as.factor(data$foreign_worker)
data$class <- as.factor(data$class)

# Divide the foreign_worker variable into two groups based on class
foreignWorker_good <- data$foreign_worker[data$class == "good"]
foreignWorker_bad <- data$foreign_worker[data$class == "bad"]

# Debug counts
print(table(foreignWorker_good))
print(table(foreignWorker_bad))

# Create count tables for each group
foreignWorkerGood_CountTable <- table(foreignWorker_good)
foreignWorkerBad_CountTable <- table(foreignWorker_bad)

# Convert the count tables to data frames
foreignWorkerGood_CountDF <- as.data.frame(foreignWorkerGood_CountTable)
names(foreignWorkerGood_CountDF) <- c("Category", "Count")
foreignWorkerGood_CountDF$Group <- "Good"

foreignWorkerBad_CountDF <- as.data.frame(foreignWorkerBad_CountTable)
names(foreignWorkerBad_CountDF) <- c("Category", "Count")
foreignWorkerBad_CountDF$Group <- "Bad"

# Combine the two data frames
foreignWorkerCombined <- rbind(foreignWorkerGood_CountDF, foreignWorkerBad_CountDF)

# Debug combined data
print(foreignWorkerCombined)

# Visualization: Bar chart for Foreign Worker vs Class
ggplot(foreignWorkerCombined, aes(x = Category, y = Count, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Foreign Worker vs Class",
    x = "Foreign Worker Status",
    y = "Count"
  ) +
  scale_fill_manual(values = c("Good" = "green", "Bad" = "red")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16), # Center the title and adjust size
    axis.text = element_text(size = 14),              # Increase the size of axis text
    axis.title = element_text(size = 14),             # Increase the size of axis titles
    legend.text = element_text(size = 12),            # Increase the size of legend text
    legend.title = element_text(size = 14)            # Increase the size of legend title
  )


########## Additional Feature: Hypothesis Testing ##########

# Hypothesis:
# Null (H0): Foreign worker status has no impact on the class (good/bad).
# Alternative (H1): Foreign worker status impacts the class.

# Perform Chi-Square Test for Foreign Worker vs Class
contingency_table <- table(data$foreign_worker, data$class)
chi_square_test <- chisq.test(contingency_table)

# Print test results
print(chi_square_test)


