
library(readr)
data <- read_csv('GooglePlayStoreApps.csv')

#Step 1: RQ is about Comparison of Means
#Step 2: Check Normality of the Dependant variable

library(ggplot2)
ggplot(data, aes(x = Rating)) +
  geom_histogram(bins = 20, fill = 'blue', alpha = 0.7) +
  labs(title = 'Histogram of Ratings', x = 'Rating', y = 'Frequency') +
  theme_minimal()

# Kolmogorov-Smirnov test for normality
ks_test <- ks.test(data$Rating, "pnorm", mean = mean(data$Rating, na.rm = TRUE), sd = sd(data$Rating, na.rm = TRUE))
cat('Kolmogorov-Smirnov Test Statistic:', ks_test$statistic, '\n')
cat('Kolmogorov-Smirnov Test p-value:', ks_test$p.value, '\n')

# Q-Q Plot
qqnorm(data$Rating)
qqline(data$Rating, col = 'red')
title('Q-Q Plot of Ratings')


#Step 3 : check unique Value for 'Type", indepednant variable 

unique_types <- unique(data$Type)
count_unique_types <- length(unique_types)
cat('Number of unique values for Type:', count_unique_types, '\n')
cat('Unique values for Type:', unique_types, '\n')


#Step 4 :Conduct wilcox test

# Assuming your dataset is already loaded and named 'data'
# Filter data based on the two unique types
group1 <- data$Rating[data$Type == unique(data$Type)[1]]
group2 <- data$Rating[data$Type == unique(data$Type)[2]]
wilcox_test <- wilcox.test(group1, group2)
cat('Wilcoxon Test Statistic:', wilcox_test$statistic, '\n')
cat('Wilcoxon Test p-value:', wilcox_test$p.value, '\n')

# Conclusion based on p-value
alpha <- 0.05
if (wilcox_test$p.value < alpha) {
  cat("There is a significant difference in ratings between the two groups (reject H0).\n")
} else {
  cat("There is no significant difference in ratings between the two groups (fail to reject H0).\n")
}


# Boxplot to show Ratings by Type
ggplot(data, aes(x = Type, y = Rating)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  labs(title = "Boxplot of Ratings by App Type", x = "App Type", y = "Rating") +
  theme_minimal()

# Histogram of Ratings with normal curve overlay
ggplot(data, aes(x = Rating)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "blue", alpha = 0.7) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(data$Rating, na.rm = TRUE), 
                            sd = sd(data$Rating, na.rm = TRUE)), 
                color = "red", size = 1) +
  labs(title = "Histogram of Ratings with Normal Curve", x = "Rating", y = "Density") +
  theme_minimal()

