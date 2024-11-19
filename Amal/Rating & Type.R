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

#mean rating by type
mean_ratings <- data %>%
  group_by(Type) %>%
  summarise(mean_rating = mean(Rating, na.rm = TRUE),
            sd = sd(Rating, na.rm = TRUE),
            count = n())
print(mean_ratings)