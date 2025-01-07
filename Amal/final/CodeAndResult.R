#Google PlayStore Data
#A143
#Codes & Result

library(readr)
data <- read_csv('GooglePlayStoreApps.csv')

#Step 1: RQ is about Comparison of Means

#Step 2: Check Normality of the Dependant variable
#Kolmogorov-Smirnov test for normality
ks_test <- ks.test(data$Rating, "pnorm", mean = mean(data$Rating, na.rm = TRUE), sd = sd(data$Rating, na.rm = TRUE))
cat('Kolmogorov-Smirnov Test Statistic:', ks_test$statistic, '\n')
cat('Kolmogorov-Smirnov Test p-value:', ks_test$p.value, '\n')

#Step 3 : check unique Value for 'Type", indepednant variable 
unique_types <- unique(data$Type)
count_unique_types <- length(unique_types)
cat('Number of unique values for Type:', count_unique_types, '\n')
cat('Unique values for Type:', unique_types, '\n')

#Step 4 :Conduct wilcox test
#Filter data based on the two unique types
group1 <- data$Rating[data$Type == unique(data$Type)[1]]
group2 <- data$Rating[data$Type == unique(data$Type)[2]]
wilcox_test <- wilcox.test(group1, group2)
cat('Wilcoxon Test Statistic:', wilcox_test$statistic, '\n')
cat('Wilcoxon Test p-value:', wilcox_test$p.value, '\n')

#Conclusion based on p-value
alpha <- 0.05
if (wilcox_test$p.value < alpha) {
  cat("There is a significant difference in ratings between the two groups (reject H0).\n")
} else {
  cat("There is no significant difference in ratings between the two groups (fail to reject H0).\n")
}


#Plots : 
library(ggplot2)

#Boxplot to show Ratings by Type
boxplot <- ggplot(data, aes(x = Type, y = Rating)) + 
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  labs(title = "Boxplot of Ratings by App Type", x = "App Type", y = "Rating") +
  theme_minimal()
boxplot

#Histogram for Free apps
hist_free <- ggplot(data[data$Type == "Free", ], aes(x = Rating)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "blue",color = "black", alpha = 0.7) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(data[data$Type == "Free", ]$Rating, na.rm = TRUE), 
                            sd = sd(data[data$Type == "Free", ]$Rating, na.rm = TRUE)), 
                color = "red", size = 1) +
  labs(title = "Histogram of Ratings for Free Apps", x = "Rating", y = "Density") +
  theme_minimal()
hist_free

#Histogram for Paid apps
hist_paid <- ggplot(data[data$Type == "Paid", ], aes(x = Rating)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "green",color = "black", alpha = 0.7) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(data[data$Type == "Paid", ]$Rating, na.rm = TRUE), 
                            sd = sd(data[data$Type == "Paid", ]$Rating, na.rm = TRUE)), 
                color = "red", size = 1) +
  labs(title = "Histogram of Ratings for Paid Apps", x = "Rating", y = "Density") +
  theme_minimal()
hist_paid

ggsave("histogram_free.png", hist_free, width = 6, height = 4)
ggsave("histogram_paid.png", hist_paid, width = 6, height = 4)
ggsave("boxplot.png", boxplot, width = 6, height = 4)


#Result Step 2 : Test for Normality of dependant variable
  #Kolmogorov-Smirnov Test Statistic: 0.1925921 
  #Kolmogorov-Smirnov Test p-value: 0 
    #since p value < alpha (0.05) :
    #so we reject the null hypothesis H0
    #ie, dpendant variable is not normally distributed

#Result Step 3  : 
  #there are exactly two indepenadant variables 
  #ie, free and paid
  
#Result Step 4 :
  #Wilcoxon Test Statistic: 3496100 
  #Wilcoxon Test p-value: 7.51031e-10 
  #There is a significant difference in ratings between the two groups (reject H0).
  #The p-value is significantly less than 0.05, 
    #suggesting that there is a statistically significant difference in the ratings between free and paid apps

