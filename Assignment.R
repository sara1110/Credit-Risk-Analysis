#setting work directory
setwd("/Users/sara/Desktop/")

#importing libraries
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

#reading csv document
data <- read.csv("assignment.csv")

#viewing summary
summary(data)

#checking for empty cells
is.na(data)

#framing data
df <- data.frame(data)

#checking for any duplicated cells/ data
duplicated(df)

# Selecting all categorical variables and converting them to numeric
correlation_matrix <- df %>%
  select_if(~ is.factor(.) || is.character(.)) %>%  # Select all categorical variables
  mutate(across(everything(), ~ as.numeric(as.factor(.)))) %>%  # Convert all to numeric
  cor(method = "spearman")

# Renaming the columns and rows with their respective variable names
colnames(correlation_matrix) <- names(df %>% select_if(~ is.factor(.) || is.character(.)))
rownames(correlation_matrix) <- names(df %>% select_if(~ is.factor(.) || is.character(.)))

# Plotting the correlation matrix
par(mar = c(5, 4, 3, 2))

corrplot(correlation_matrix, method = "circle", 
         addCoef.col = "black", number.cex = 0.7, tl.col = "black", tl.cex = 1.2, cl.cex = 1.2)

title(main = "Spearman Correlation Matrix of Categorical Variables", col.main = "black", font.main = 2, cex.main = 1.7)

#Okay the correlation plot has been formed, and multiple hypothesis can be formed from the plot

#H1 Age vs. Credit Amount: The higher the age, the higher the higher the credit amount 
ggplot(data = df, aes(x = age, y = credit_amount)) +
  geom_point(shape = 23, size = 2, fill = "blue", color = "navy") + # Add regression line
  labs(
    title = "Scatter Plot of Age vs. Credit Amount with Regression Line",
    x = "Age",
    y = "Credit Amount"
  ) +
  theme_classic()

#testing the correlation through Pearson's test 
cor.test(df$age, df$credit_amount)

#H2. The higher the credit amount, the higher the duration

#Plotting Graph to test H2
ggplot(df, aes(x = duration, y = credit_amount)) + 
  geom_point(aes(size = installment_commitment, color = existing_credits)) + 
  labs(
    x = "Duration (Months)", 
    y = "Credit Amount",
    color = "Existing Credits",
    size = "Installment Commitment"
  ) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "#5C6BC0") + # Quadratic regression
  theme_minimal() +
  ggtitle("Relationship Between Duration and Credit Amount (Quadratic Regression)") +
  theme(
    axis.title.x = element_text(color = "#0099f9", size = 16, face = "bold"),
    axis.title.y = element_text(color = "#0099f9", size = 16, face = "bold"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 18, face = "bold", color = "#0099f9", hjust = 0.5)
  )

#testing the correlation through Pearson's test 
cor.test(df$duration, df$credit_amount)





