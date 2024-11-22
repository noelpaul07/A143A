library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(ggpubr)


data <- read_csv("GooglePlayStoreApps.csv")

#summary,data cleaning and standerdization
data
summary(data)
missing_values <- colSums(is.na(data))
print(missing_values)

data <- data %>%
  mutate(Type = ifelse(Type == "Free", "Free", "Paid"),
         Rating = as.numeric(Rating),
         Size_in_MB = as.numeric(Size_in_MB))

summary(data)

data$Rating_std <- scale(data$Rating)
hist(data$Rating)
hist(data$Rating_std)

summary(data$Rating_std)

mean_ratings <- data %>%
  group_by(Type) %>%
  summarise(mean_rating = mean(Rating, na.rm = TRUE),
            sd = sd(Rating, na.rm = TRUE),
            count = n())
print(mean_ratings)

#Visualize ratings with histograms
ggplot(data, aes(x = Rating, fill = Type)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  labs(title = "Histogram of App Ratings by Type", x = "Rating", y = "Count") +
  theme_minimal()

#visualise mean rating with histograms
ggplot(mean_ratings, aes(x = Type, y = mean_rating, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean Ratings by App Type",x = "Type",y = "Mean Rating") +
  theme_minimal()

#Boxplot for Ratings by Type
ggplot(data, aes(x = Type, y = Rating, fill = Type)) +
  geom_boxplot() +
  labs(title = "Boxplot of Ratings by App Type", x = "Type", y = "Rating") +
  theme_minimal()

#Chi-Square Test
contingency_table <- table(data$Type, cut(data$Rating, breaks = seq(1, 5, by = 0.5)))
chi_square_test <- chisq.test(contingency_table)
print(chi_square_test)

#Trend Line
ggplot(data, aes(x = as.numeric(Installs), y = Rating)) +
  geom_point(aes(color = Type), alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Trend of Ratings with Installs", x = "Number of Installs", y = "Rating") +
  theme_minimal()

ggsave("histogram.png")
ggsave("boxplot.png")
ggsave("trend_line.png")


#2. Proportion, Chi-Squre Test, Bar Plot
library(dplyr)
library(ggplot2)
library(readr)

data <- read_csv("GooglePlayStoreApps.csv")
data <- data %>%
  mutate(High_Rating = ifelse(Rating >= 4, "High", "Low"))

proportion_table <- data %>%
  group_by(Type) %>%
  summarise(High_Rating_Proportion = mean(High_Rating == "High"),
            Count = n())
print(proportion_table)

contingency_table <- table(data$Type, data$High_Rating)
chi_square_test <- chisq.test(contingency_table)
print(chi_square_test)

ggplot(proportion_table, aes(x = Type, y = High_Rating_Proportion, fill = Type)) +
  geom_bar(stat = "identity") +
  labs(title = "Proportion of High Ratings (4+ Stars) by App Type",
       x = "App Type",
       y = "Proportion of High Ratings") +
  theme_minimal()

#3. Correlation, Scatter pLot with trend line
library(ggplot2)
library(dplyr)
library(readr)

data <- read_csv("GooglePlayStoreApps.csv")
data$Installs <- as.numeric(gsub(",", "", data$Installs))

correlation_result <- cor(data$Rating, data$Installs, use = "complete.obs")
print(paste("Correlation coefficient between Rating and Installs:", correlation_result))

ggplot(data, aes(x = Installs, y = Rating)) +
  geom_point(alpha = 0.5, color = 'blue') +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Correlation between App Ratings and Number of Installs",
       x = "Number of Installs",
       y = "Rating") +
  theme_minimal()
