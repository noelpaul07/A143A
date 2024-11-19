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

