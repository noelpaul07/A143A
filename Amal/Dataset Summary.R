library(ggplot2)
library(dplyr)
library(tidyr)

#Load the dataset
data <- read.csv("GooglePlayStoreApps.csv", stringsAsFactors = FALSE)

#Data Cleaning
#Remove rows with missing values
data <- na.omit(data)

#Convert necessary columns to numeric
data$Reviews <- as.numeric(data$Reviews)
data$Installs <- as.numeric(gsub("[+,]", "", data$Installs))
data$Rating <- as.numeric(data$Rating)

#Descriptive Statistics
#Summary statistics for numerical columns
summary(data[c("Rating", "Reviews", "Installs")])

#Frequency distribution for categories
category_dist <- data %>% group_by(Category) %>% summarise(Count = n())

#Visualizations
#Bar plot for app categories
ggplot(category_dist, aes(x = reorder(Category, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "App Categories Distribution", x = "Category", y = "Count")

#Box plot for ratings by category
ggplot(data, aes(x = Category, y = Rating)) +
  geom_boxplot() +
  coord_flip() +
  labs(title = "Ratings by App Category", x = "Category", y = "Rating")

#Histogram for app ratings
ggplot(data, aes(x = Rating)) +
  geom_histogram(binwidth = 0.5, fill = "green", color = "black") +
  labs(title = "Distribution of App Ratings", x = "Rating", y = "Frequency")

#Correlation Analysis
cor_matrix <- cor(data[c("Reviews", "Installs", "Rating")], use = "complete.obs")
print(cor_matrix)

#Hypothesis Testing
#ANOVA for ratings by category
anova_results <- aov(Rating ~ Category, data = data)
summary(anova_results)
