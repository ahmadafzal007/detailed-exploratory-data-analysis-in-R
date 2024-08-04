# Load necessary library
library(ggplot2)

# Load the airquality dataset
data("airquality")
print(airquality)


# 1. Boxplot for the "Ozone" variable
boxplot(airquality$Ozone, main="Boxplot of Ozone", ylab="Ozone", col="lightblue", na.rm=TRUE)

# 2. Scatterplot of "Ozone" against "Wind"
plot(airquality$Wind, airquality$Ozone, main="Scatterplot of Ozone vs Wind", xlab="Wind", ylab="Ozone", pch=19, col="blue", na.rm=TRUE)

# 3. Histogram for the "Ozone" variable
hist(airquality$Ozone, main="Histogram of Ozone", xlab="Ozone", col="lightgreen", breaks=20, na.rm=TRUE)

# 4. Density plot for "Ozone" variable
ozone_density <- density(airquality$Ozone, na.rm=TRUE)
plot(ozone_density, main="Density Plot of Ozone", xlab="Ozone", ylab="Density", col="purple")
polygon(ozone_density, col="orange", border="purple")









#  Task 1.1

# Load necessary libraries
library(outliers)
library(dplyr)

# Load the airquality dataset
data("airquality")

# Extract the Ozone variable
ozone <- airquality$Ozone

# Remove NA values for outlier detection
ozone_clean <- na.omit(ozone)

# Z-score method
z_scores <- (ozone_clean - mean(ozone_clean)) / sd(ozone_clean)
z_score_outliers <- ozone_clean[abs(z_scores) > 2]

# IQR method
Q1 <- quantile(ozone_clean, 0.25)
Q3 <- quantile(ozone_clean, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
iqr_outliers <- ozone_clean[ozone_clean < lower_bound | ozone_clean > upper_bound]

# Tukey's fences
tukey_fences <- boxplot.stats(ozone_clean)$out

# Grubbs' test
grubbs_test <- grubbs.test(ozone_clean, opposite=FALSE, two.sided=TRUE)
grubbs_outliers <- if (grubbs_test$p.value < 0.05) grubbs_test$alternative else NULL

# Print the results
cat("Z-score outliers:\n")
print(z_score_outliers)

cat("IQR outliers:\n")
print(iqr_outliers)

cat("Tukey's fences outliers:\n")
print(tukey_fences)

cat("Grubbs' test outliers:\n")
print(grubbs_outliers)








# Task 1.2


set.seed(42) # For reproducibility
data <- rnorm(100, mean=50, sd=10)
print(data)



data_with_outliers <- c(data, 100, 105, 110, 120, 130)
print(data_with_outliers)





library(DescTools)

# Winsorize the dataset
winsorized_data <- Winsorize(data_with_outliers, probs=c(0.05, 0.95))
print(winsorized_data)






# Remove the top and bottom 5% of the data
trimmed_data <- data_with_outliers
lower_bound <- quantile(data_with_outliers, 0.05)
upper_bound <- quantile(data_with_outliers, 0.95)
trimmed_data <- trimmed_data[trimmed_data >= lower_bound & trimmed_data <= upper_bound]
print(trimmed_data)




mean_value <- mean(data_with_outliers)
iqr_bounds <- quantile(data_with_outliers, c(0.25, 0.75))
iqr_value <- IQR(data_with_outliers)
lower_bound <- iqr_bounds[1] - 1.5 * iqr_value
upper_bound <- iqr_bounds[2] + 1.5 * iqr_value

mean_imputed_data <- data_with_outliers
mean_imputed_data[mean_imputed_data < lower_bound | mean_imputed_data > upper_bound] <- mean_value
print(mean_imputed_data)





log_transformed_data <- log(data_with_outliers)
print(log_transformed_data)






#Task 2



library(tidyverse)
library(ggplot2)
library(DT)
library(caret)


wine <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv", sep = ";")


# Display the first few rows of the dataset
datatable(head(wine))


# Check for missing values
sum(is.na(wine))

# Summary statistics
summary(wine)





# Univariate

# Histogram of wine quality
ggplot(wine, aes(x = quality)) + 
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Histogram of Wine Quality", x = "Quality", y = "Frequency")

# Boxplot for fixed acidity
ggplot(wine, aes(y = `fixed.acidity`)) + 
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Boxplot of Fixed Acidity", y = "Fixed Acidity")



# Bivariate

# Scatterplot of quality vs alcohol
ggplot(wine, aes(x = alcohol, y = quality)) + 
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Scatterplot of Quality vs Alcohol", x = "Alcohol", y = "Quality")

# Boxplot of quality vs pH
ggplot(wine, aes(x = factor(quality), y = pH)) + 
  geom_boxplot(fill = "purple", color = "black") +
  labs(title = "Boxplot of Quality vs pH", x = "Quality", y = "pH")



# Multivariate

# Correlation matrix
cor_matrix <- cor(wine)
library(corrplot)
corrplot(cor_matrix, method = "circle")

# Pair plot
library(GGally)
ggpairs(wine[, c("quality", "alcohol", "pH", "fixed.acidity")])








# Identify outliers using IQR method for alcohol
Q1 <- quantile(wine$alcohol, 0.25)
Q3 <- quantile(wine$alcohol, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
alcohol_outliers <- wine[wine$alcohol < lower_bound | wine$alcohol > upper_bound, ]
print(alcohol_outliers)

# Handling outliers by Winsorisation
wine$alcohol <- Winsorize(wine$alcohol, probs = c(0.05, 0.95))
print(wine$alcohol)







# Log transformation for highly skewed variables if needed
wine$log_fixed_acidity <- log(wine$`fixed.acidity` + 1) # Adding 1 to avoid log(0)

# Check distribution after transformation
ggplot(wine, aes(x = log_fixed_acidity)) + 
  geom_histogram(binwidth = 0.1, fill = "green", color = "black") +
  labs(title = "Histogram of Log Transformed Fixed Acidity", x = "Log(Fixed Acidity)", y = "Frequency")









# Summary statistics after cleaning and transformations
summary(wine)

# Explore relationships using additional visualizations
# Density plot for alcohol content by quality
ggplot(wine, aes(x = alcohol, fill = factor(quality))) + 
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Alcohol Content by Quality", x = "Alcohol", fill = "Quality")

# Violin plot for pH by quality
ggplot(wine, aes(x = factor(quality), y = pH, fill = factor(quality))) + 
  geom_violin() +
  labs(title = "Violin Plot of pH by Quality", x = "Quality", y = "pH")




















# Task No 3



library(tidyverse)
library(ggplot2)
library(ggthemes)

# Load the Titanic dataset from the Titanic package
library(titanic)
data("titanic_train")


datatable(head(titanic_train))



# Check for missing values
sum(is.na(titanic_train))

# Summary statistics
summary(titanic_train)

# Data type conversion if necessary
str(titanic_train)

# Handling missing values
titanic_train <- titanic_train %>%
  drop_na()



# Bar plot for 'Survived'
ggplot(titanic_train, aes(x = factor(Survived))) + 
  geom_bar(fill = "steelblue", color = "black") +
  labs(title = "Survival Count", x = "Survived", y = "Count") +
  theme_minimal()

# Bar plot for 'Pclass'
ggplot(titanic_train, aes(x = factor(Pclass))) + 
  geom_bar(fill = "coral", color = "black") +
  labs(title = "Passenger Class Count", x = "Pclass", y = "Count") +
  theme_minimal()









# Stacked bar plot for 'Survived' by 'Sex'
ggplot(titanic_train, aes(x = factor(Survived), fill = factor(Sex))) + 
  geom_bar(position = "stack") +
  labs(title = "Survival by Sex", x = "Survived", y = "Count", fill = "Sex") +
  scale_fill_manual(values = c("pink", "lightblue")) +
  theme_minimal()

# Stacked bar plot for 'Survived' by 'Pclass'
ggplot(titanic_train, aes(x = factor(Survived), fill = factor(Pclass))) + 
  geom_bar(position = "stack") +
  labs(title = "Survival by Passenger Class", x = "Survived", y = "Count", fill = "Pclass") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal()






# Mosaic plot for 'Survived' by 'Sex' and 'Pclass'
library(vcd)
mosaic(~ Survived + Sex + Pclass, data = titanic_train,
       main = "Survival by Sex and Passenger Class",
       shade = TRUE,
       legend = TRUE)









# Task 3.1



# Histogram for Age
ggplot(titanic_train, aes(x = Age)) + 
  geom_histogram(binwidth = 5, fill = "darkblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Age", x = "Age", y = "Count") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, color = "darkred", size = 14, face = "bold"),
    axis.title.x = element_text(color = "darkblue", size = 12, face = "bold"),
    axis.title.y = element_text(color = "darkblue", size = 12, face = "bold")
  )





# Box plot for Fare
ggplot(titanic_train, aes(y = Fare)) + 
  geom_boxplot(fill = "orange", color = "darkred", outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  labs(title = "Boxplot of Fare", y = "Fare") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, color = "darkgreen", size = 14, face = "bold"),
    axis.title.y = element_text(color = "darkgreen", size = 12, face = "bold")
  )





library(GGally)


numeric_vars <- titanic_train %>%
  select(Age, Fare, Pclass, SibSp, Parch, Survived)

# Correlation matrix plot
ggpairs(numeric_vars, 
        upper = list(continuous = wrap("cor", size = 3)),
        lower = list(continuous = wrap("points", alpha = 0.6)),
        diag = list(continuous = wrap("barDiag", fill = "blue")),
        axisLabels = "show") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, color = "darkred", size = 14, face = "bold")
  )










library(reshape2)


cor_matrix <- cor(numeric_vars, use = "complete.obs")


melted_cor_matrix <- melt(cor_matrix)

# Heatmap of correlation matrix
ggplot(data = melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) + 
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab", name = "Correlation") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
    plot.title = element_text(hjust = 0.5, color = "darkred", size = 14, face = "bold")
  ) +
  labs(title = "Correlation Heatmap")









# Scatter plot for Age vs. Fare
ggplot(titanic_train, aes(x = Age, y = Fare, color = factor(Survived))) + 
  geom_point(shape = 17, size = 3, alpha = 0.7) +
  scale_color_manual(values = c("red", "green")) +
  labs(title = "Scatter Plot of Age vs. Fare", x = "Age", y = "Fare", color = "Survived") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, color = "darkblue", size = 14, face = "bold"),
    axis.title.x = element_text(color = "darkblue", size = 12, face = "bold"),
    axis.title.y = element_text(color = "darkblue", size = 12, face = "bold"),
    legend.position = "top"
  )




