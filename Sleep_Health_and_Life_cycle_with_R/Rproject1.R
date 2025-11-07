# Load necessary libraries
# Install tidyverse for data manipulation and ggplot2
install.packages("tidyverse")

# Install plotly for interactive charts
install.packages("plotly")

# Install DT for interactive data tables
install.packages("DT")

# Install corrplot for correlation heatmaps
install.packages("corrplot")

# Install flexdashboard for dashboard creation
install.packages("flexdashboard")

# If you want to build a Shiny-based dashboard
install.packages("shiny")

# Optional: Install shinyWidgets for additional UI components in Shiny apps
install.packages("shinyWidgets")

library(tidyverse)
library(ggplot2)
library(plotly)
library(DT)

# Read the dataset
sleep_data <- read.csv("Sleep_health_and_lifestyle_dataset.csv")

# View the structure and summary
str(sleep_data)
summary(sleep_data)
head(sleep_data)

# Check for missing values
cat("Total missing values in dataset:", sum(is.na(sleep_data)), "\n")
cat("Missing values per column:\n")
colSums(is.na(sleep_data))

# Check for duplicate rows
num_duplicates <- sum(duplicated(sleep_data))
cat("Number of duplicate rows:", num_duplicates, "\n")



# Convert columns to factors where appropriate
sleep_data$Gender <- as.factor(sleep_data$Gender)
sleep_data$Occupation <- as.factor(sleep_data$Occupation)
sleep_data$BMI.category <- as.factor(sleep_data$BMI.Category)
sleep_data$Sleep.disorder <- as.factor(sleep_data$Sleep.Disorder)


# Box Plot
boxplot(sleep_data$Age)
boxplot(sleep_data$Stress.Level)
boxplot(sleep_data$Quality.of.Sleep)
boxplot(sleep_data$Sleep.Duration)

# Average Sleep Duration By Occupation
ggplot(sleep_data, aes(x = reorder(Occupation, Sleep.Duration, FUN = median), y = Sleep.Duration)) +
  geom_boxplot(fill = "skyblue") +
  coord_flip() +
  labs(title = "Sleep Duration by Occupation", x = "Occupation", y = "Sleep Duration (hrs)")

# Sleep Duration VS Age By Gender
ggplot(sleep_data, aes(x = Age, y = Sleep.Duration, color = Gender)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Sleep Duration by Age and Gender", x = "Age", y = "Sleep Duration (hrs)")


# Stress Level VS Sleep Duration By Sleep Disorder
ggplot(sleep_data, aes(x = Stress.Level, y = Sleep.Duration, color = Sleep.Disorder)) +
  geom_point(alpha = 0.7) +
  labs(title = "Stress vs Sleep Duration by Sleep Disorder", x = "Stress Level", y = "Sleep Duration")

# Correlation Heatmap of Numaric Health Variable
library(corrplot)

# Select numeric columns
numeric_cols <- sleep_data %>% select_if(is.numeric)

# Remove rows with missing values
numeric_data <- na.omit(numeric_cols)

# Compute and plot correlation matrix
cor_matrix <- cor(numeric_data)
corrplot(cor_matrix, method = "color", type = "lower", tl.cex = 0.8)

# BMI Category VS SleepDisorder
ggplot(sleep_data, aes(x = BMI.Category, fill = Sleep.Disorder)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Sleep Disorders by BMI Category", y = "Proportion")
