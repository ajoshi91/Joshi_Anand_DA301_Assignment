## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Week 4 assignment: EDA using R

###############################################################################

# 1. Load and explore the data

# Install and import Tidyverse.
install.packages("tidyverse")
library(tidyverse)

# Determine the working directory.
getwd() 

# Import the CSV file as a data frame
sales <- read_csv("turtle_sales.csv")

# Print the data frame
print(sales)

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns. 
# Create a new data frame with selected columns
clean_sales <- sales %>% 
  select(-Ranking, -Year, -Genre, -Publisher)

# Print the new data frame
print(clean_sales)

# View the descriptive statistics.
summary(clean_sales)

################################################################################

# 2. Review plots to determine insights into the data set.

# Load the required packages
library(ggplot2)

## 2a) Scatterplots
# Create scatterplots.
# Scatterplot
scatterplot <- ggplot(clean_sales, aes(x = NA_Sales, y = EU_Sales)) +
  geom_point() +
  labs(x = "NA Sales", y = "EU Sales", title = "Scatterplot of NA Sales vs. EU Sales")

# Display the plots
scatterplot

## 2b) Histograms
# Create histograms.
# Histogram
histogram <- ggplot(clean_sales, aes(x = Global_Sales)) +
  geom_histogram(binwidth = 2, fill = "blue", color = "black") +
  labs(x = "Global Sales", y = "Frequency", title = "Histogram of Global Sales")

# Display the plots
histogram

## 2c) Boxplots
# Create boxplots.
# Boxplot
boxplot <- ggplot(clean_sales, aes(x = Platform, y = Global_Sales)) +
  geom_boxplot() +
  labs(x = "Platform", y = "Global Sales", title = "Boxplot of Global Sales by Platform")

# Display the plots
boxplot

# Save the new data frame created as a CSV file
write_csv(clean_sales, "clean_sales.csv") 


###############################################################################

# 3. Observations and insights

## Your observations and insights here ......
# The scatter plot of "EU Sales" against "NA Sales" suggests a weak positive correlation 
# between the two variables. The relationship between
# European sales and North American sales is not particularly strong. The data points tend 
# to form a loose cluster, indicating that some products may perform better in one region 
# compared to the other. Further analysis is required to explore the specific factors 
# influencing the sales patterns in each region.

# The histogram of "Global Sales" exhibits a positive skewness in the distribution. 
# This indicates that there are relatively more products with lower global sales 
# compared to those with higher global sales. The majority of products seem to have 
# modest sales figures, while a smaller subset of products might have achieved 
# substantial sales success. Understanding the reasons behind this distribution 
# asymmetry could provide valuable insights into the market dynamics and product performance.

# The box plot of "Global Sales" by "Platform" reveals that the gaming consoles "Wii, 
# "NES", and "GB" were the most popular 
# products in terms of global sales. The presence of 
# outliers in the box plot suggests that there might be a few exceptional products, 
# either in terms of high or low global sales, beyond the typical range for the majority 
# of products. 

###############################################################################
###############################################################################


# Week 5 assignment: Cleaning and manipulating data using R

################################################################################

# 1. Load and explore the data
# Load the saved CSV file into a new data frame
clean_sales1 <- read_csv("clean_sales.csv")

# View data frame created in Week 4.
print(clean_sales1)

# Check output: Determine the min, max, and mean values.
na_sales_min <- min(clean_sales1$NA_Sales)
na_sales_max <- max(clean_sales1$NA_Sales)
na_sales_mean <- mean(clean_sales1$NA_Sales)

eu_sales_min <- min(clean_sales1$EU_Sales)
eu_sales_max <- max(clean_sales1$EU_Sales)
eu_sales_mean <- mean(clean_sales1$EU_Sales)

global_sales_min <- min(clean_sales1$Global_Sales)
global_sales_max <- max(clean_sales1$Global_Sales)
global_sales_mean <- mean(clean_sales1$Global_Sales)

# Print the calculated statistics
cat("NA Sales - Min:", na_sales_min, "\n")
cat("NA Sales - Max:", na_sales_max, "\n")
cat("NA Sales - Mean:", na_sales_mean, "\n")

cat("EU Sales - Min:", eu_sales_min, "\n")
cat("EU Sales - Max:", eu_sales_max, "\n")
cat("EU Sales - Mean:", eu_sales_mean, "\n")

cat("Global Sales - Min:", global_sales_min, "\n")
cat("Global Sales - Max:", global_sales_max, "\n")
cat("Global Sales - Mean:", global_sales_mean, "\n")


# View the descriptive statistics.
summary(clean_sales1[c("NA_Sales", "EU_Sales", "Global_Sales")]) 

###############################################################################

# 2. Determine the impact on sales per product_id.
# Load the required package
library(dplyr)

## 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.
# Group by Product_id and sum the sales values
sales_per_product <- clean_sales1 %>%
  group_by(Product) %>%
  summarise(
    Total_NA_Sales = sum(NA_Sales),
    Total_EU_Sales = sum(EU_Sales),
    Total_Global_Sales = sum(Global_Sales)
  )

# Print the summary of the new data frame
print(sales_per_product)

# Explore the data frame.
summary(sales_per_product)
str(sales_per_product)


## 2b) Determine which plot is the best to compare game sales.

install.packages("plotly") 

# Load the required packages
library(ggplot2)

# Create scatterplots.
# Scatterplot: Total NA Sales vs. Total EU Sales
scatterplot_na_eu <- ggplot(sales_per_product, aes(x = Total_NA_Sales, y = Total_EU_Sales)) +
  geom_point() +
  labs(x = "Total NA Sales", y = "Total EU Sales", title = "Scatterplot of Total NA Sales vs. Total EU Sales")

# Display the plots
scatterplot_na_eu


# Create histograms.
# Histogram: Total Global Sales
histogram_global_sales <- ggplot(sales_per_product, aes(x = Total_Global_Sales)) +
  geom_histogram(binwidth = 2, fill = "blue", color = "black") +
  labs(x = "Total Global Sales", y = "Frequency", title = "Histogram of Total Global Sales")

# Display the plots
histogram_global_sales


# Create boxplots.
# Boxplot: Total Global Sales by Product ID
boxplot_global_sales <- ggplot(sales_per_product, aes(x = Product, y = Total_Global_Sales, group = Product)) +
  geom_boxplot() +
  labs(x = "Product ID", y = "Total Global Sales", title = "Boxplot of Total Global Sales by Product ID") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the plots
boxplot_global_sales 

###############################################################################


# 3. Determine the normality of the data set.

## 3a) Create Q-Q Plots
library(ggplot2)

# Create Q-Q Plots.
qq_plot_na_sales <- ggplot(sales_per_product, aes(sample = Total_NA_Sales)) +
  geom_qq() +
  labs(title = "Q-Q Plot of Total NA Sales")

qq_plot_eu_sales <- ggplot(sales_per_product, aes(sample = Total_EU_Sales)) +
  geom_qq() +
  labs(title = "Q-Q Plot of Total EU Sales")

qq_plot_global_sales <- ggplot(sales_per_product, aes(sample = Total_Global_Sales)) +
  geom_qq() +
  labs(title = "Q-Q Plot of Total Global Sales")

# Display the Q-Q plots
qq_plot_na_sales
qq_plot_eu_sales
qq_plot_global_sales


## 3b) Perform Shapiro-Wilk test
# Install and import Moments.
install.packages("moments")
library(moments)

# Perform Shapiro-Wilk test.
shapiro_test_na_sales <- shapiro.test(sales_per_product$Total_NA_Sales)
shapiro_test_eu_sales <- shapiro.test(sales_per_product$Total_EU_Sales)
shapiro_test_global_sales <- shapiro.test(sales_per_product$Total_Global_Sales)

# Print the test results
print(shapiro_test_na_sales)
print(shapiro_test_eu_sales)
print(shapiro_test_global_sales)


## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.
skewness_na_sales <- skewness(sales_per_product$Total_NA_Sales)
skewness_eu_sales <- skewness(sales_per_product$Total_EU_Sales)
skewness_global_sales <- skewness(sales_per_product$Total_Global_Sales)

kurtosis_na_sales <- kurtosis(sales_per_product$Total_NA_Sales)
kurtosis_eu_sales <- kurtosis(sales_per_product$Total_EU_Sales)
kurtosis_global_sales <- kurtosis(sales_per_product$Total_Global_Sales)

# Print the calculated skewness and kurtosis values
cat("Skewness of Total NA Sales:", skewness_na_sales, "\n")
cat("Skewness of Total EU Sales:", skewness_eu_sales, "\n")
cat("Skewness of Total Global Sales:", skewness_global_sales, "\n")

cat("Kurtosis of Total NA Sales:", kurtosis_na_sales, "\n")
cat("Kurtosis of Total EU Sales:", kurtosis_eu_sales, "\n")
cat("Kurtosis of Total Global Sales:", kurtosis_global_sales, "\n")

## 3d) Determine correlation
# Calculate the correlation matrix for all sales data columns
sales_correlation <- cor(sales_per_product[, c("Total_NA_Sales", "Total_EU_Sales", "Total_Global_Sales")])

# Print the correlation matrix
print(sales_correlation) 

###############################################################################

# 4. Plot the data
# Create plots to gain insights into data.
# Choose the type of plot you think best suits the data set and what you want 
# to investigate. Explain your answer in your report.
# Create scatter plots with trend lines for all sales data
scatterplot_na_eu <- ggplot(sales_per_product, aes(x = Total_NA_Sales, y = Total_EU_Sales)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Total NA Sales", y = "Total EU Sales", title = "Scatter Plot with Trend Line: NA Sales vs. EU Sales")

scatterplot_na_global <- ggplot(sales_per_product, aes(x = Total_NA_Sales, y = Total_Global_Sales)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Total NA Sales", y = "Total Global Sales", title = "Scatter Plot with Trend Line: NA Sales vs. Global Sales")

scatterplot_eu_global <- ggplot(sales_per_product, aes(x = Total_EU_Sales, y = Total_Global_Sales)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "green") +
  labs(x = "Total EU Sales", y = "Total Global Sales", title = "Scatter Plot with Trend Line: EU Sales vs. Global Sales")

# Display the scatter plots with trend lines
scatterplot_na_eu
scatterplot_na_global
scatterplot_eu_global

# Save the sales_per_product data frame as a CSV file
write.csv(sales_per_product, "sales_per_product.csv")

###############################################################################

# 5. Observations and insights
# Your observations and insights here...
# In summary, the skewness and kurtosis values indicate that the sales data is not 
# normally distributed and exhibits heavy tails and a sharp peak (leptokurtic) 
# compared to a normal distribution. 

# The null hypothesis of the Shapiro-Wilk test is that the data follows a normal 
# distribution. A small p-value (typically less than 0.05) indicates that you reject 
# the null hypothesis, suggesting that the data does not follow a normal distribution.
# In all three cases, the p-values are very small (much smaller than 0.05), indicating 
# strong evidence to reject the null hypothesis. Therefore, you can conclude that the
# "Total_NA_Sales," "Total_EU_Sales," and "Total_Global_Sales" data do not follow a 
# normal distribution. 

# The correlation matrix indicates that there are positive correlations between 
# the sales in different regions (North America and Europe) with total global sales. 
# This implies that sales in these regions tend to move together concerning the overall
# global sales. The strongest correlation is observed between Total_NA_Sales and 
# Total_Global_Sales. 


###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R

###############################################################################

# 1. Load and explore the data
# View data frame created in Week 5.
sales_per_product1 <- read.csv("sales_per_product.csv")

# View data frame created in Week 5.
print(sales_per_product1)

# Check the structure of the loaded data frame
str(sales_per_product1) 

###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
sales_correlation1 <- cor(sales_per_product1[, c("Total_NA_Sales", "Total_EU_Sales")])

# Print the correlation matrix
print(sales_correlation1)

# Perform linear regression between Total_NA_Sales and Total_EU_Sales
linear_model <- lm(Total_EU_Sales ~ Total_NA_Sales, data = sales_per_product1)

# View the output of the linear regression model
summary(linear_model)


## 2b) Create a plot (simple linear regression)
# Create a scatter plot with the linear regression line
scatterplot_reg <- ggplot(sales_per_product1, aes(x = Total_NA_Sales, y = Total_EU_Sales)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Total NA Sales", y = "Total EU Sales", title = "Scatter Plot with Linear Regression Line")

# Display the scatter plot with the regression line
scatterplot_reg 

###############################################################################

# 3. Create a multiple linear regression model
# Load the required package
library(dplyr)

# Perform multiple linear regression
multiple_linear_model <- lm(Total_Global_Sales ~ Total_NA_Sales + Total_EU_Sales, data = sales_per_product1)

# View the output of the multiple linear regression model
summary(multiple_linear_model) 

###############################################################################

# 4. Predictions based on given values

# The multiple linear regression model that was previously created:
# Total_Global_Sales = b0 + b1 * Total_NA_Sales + b2 * Total_EU_Sales 
# where b0, b1, and b2 are the coefficients estimated from the multiple linear regression.

# Compare with observed values for a number of records.
NA_Sales_sum_values <- c(34.02, 3.93, 2.73, 2.26, 22.08)
EU_Sales_sum_values <- c(23.80, 1.56, 0.65, 0.97, 0.52)

# Create a data frame with the provided values
observed_data <- data.frame(Total_NA_Sales = NA_Sales_sum_values, Total_EU_Sales = EU_Sales_sum_values)

# Use the multiple linear regression model to predict Total_Global_Sales
predicted_global_sales <- predict(multiple_linear_model, newdata = observed_data)

# Print the predicted Total_Global_Sales for each scenario
predicted_global_sales 


###############################################################################

# 5. Observations and insights
# Your observations and insights here...
# The predictions suggest that variations in NA_Sales_sum and EU_Sales_sum have 
# different impacts on the predicted global sales. For example, higher NA_Sales_sum 
# and EU_Sales_sum tend to result in higher predicted global sales, while lower values 
# lead to lower global sales predictions.

# Overall, the data and the multiple linear regression model suggest a positive 
# relationship between North American, European, and global sales, where sales in 
# one region tend to be positively associated with sales in the other regions. 
# This information can be valuable for businesses to understand the interplay of 
# regional sales and their impact on the overall global performance. 


###############################################################################
###############################################################################




