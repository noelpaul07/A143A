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
summary(data$Rating_std)

#histogram with a KDE curve for the raw Rating values
hist(data$Rating, 
     main = "Histogram with Density Curve (Rating)", 
     xlab = "Rating", 
     col = "lightblue", 
     border = "black", 
     prob = TRUE)  # Set `prob = TRUE` for density scale
#curve
lines(density(data$Rating, na.rm = TRUE), 
      col = "red", 
      lwd = 2)

#histogram with a KDE curve for the standardized Rating values
hist(data$Rating_std, 
     main = "Histogram with Density Curve (Standardized Rating)", 
     xlab = "Standardized Rating", 
     col = "lightgreen", 
     border = "black", 
     prob = TRUE)
#curve
lines(density(data$Rating_std, na.rm = TRUE), 
      col = "blue", 
      lwd = 2)

#mean rating by type
mean_ratings <- data %>%
  group_by(Type) %>%
  summarise(mean_rating = mean(Rating, na.rm = TRUE),
            sd = sd(Rating, na.rm = TRUE),
            count = n())
print(mean_ratings)

#VISUALISATION

#Visualize ratings with histograms
ggplot(data, aes(x = Rating, fill = Type)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  labs(title = "Histogram of App Ratings by Type", x = "Rating", y = "Count") +
  theme_minimal()

#Boxplot for Ratings by Type
ggplot(data, aes(x = Type, y = Rating, fill = Type)) +
  geom_boxplot() +
  labs(title = "Boxplot of Ratings by App Type", x = "Type", y = "Rating") +
  theme_minimal()

#Boxplot for Standerdized Ratings by Type
ggplot(data, aes(x = Type, y = Rating_std, fill = Type)) +
  geom_boxplot() +
  labs(title = "Boxplot of Standerdized Ratings by App Type", x = "Type", y = "Rating") +
  theme_minimal()

summary(data$Rating_std)
summary(data$Rating)


#plotting raw rating alone
ggplot(data, aes(x = Rating)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black", alpha = 0.7) +
  labs(
    title = "Distribution of Raw Ratings",
    x = "Rating",
    y = "Frequency"
  ) +
  theme_minimal()

#plotting standardized rating alone
ggplot(data, aes(x = Rating_std)) +
  geom_histogram(bins = 30, fill = "lightgreen", color = "black", alpha = 0.7) +
  labs(
    title = "Distribution of Standardized Ratings",
    x = "Standardized Rating",
    y = "Frequency"
  ) +
  theme_minimal()

#density plot of standardized rating
ggplot(data, aes(x = Rating_std)) +
  geom_density(fill = "lightgreen", color = "darkgreen", alpha = 0.7) +
  labs(
    title = "Density Plot of Standardized Ratings",
    x = "Standardized Rating",
    y = "Density"
  ) +
  theme_minimal()

#plotting second variable - Type
ggplot(data, aes(x = Type, fill = Type)) +
  geom_bar() +
  labs(
    title = "Count of Free and Paid Apps",
    x = "Type",
    y = "Count"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("Free" = "lightblue", "Paid" = "orange"))

#pie-chart
data %>%
  count(Type) %>%
  ggplot(aes(x = "", y = n, fill = Type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(
    title = "Proportion of Free and Paid Apps",
    x = "",
    y = ""
  ) +
  theme_void() +
  scale_fill_manual(values = c("Free" = "lightblue", "Paid" = "orange"))




