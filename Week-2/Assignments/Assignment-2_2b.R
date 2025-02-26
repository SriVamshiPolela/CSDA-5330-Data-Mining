# Load the dataset using read_csv function
cars_df <- read_csv("usedcars.csv")
# Displays the first 6 rows in the dataset
head(cars_df)
# R will display a summary of statistics for each column in the dataset, including measures such as minimum, 1st quartile, median (2nd quartile), mean, 3rd quartile, and maximum
summary(cars_df)
# summary statistics of the price and mileage columns
summary(cars_df$price)
summary(cars_df$mileage)

# Calculate quartiles 
quartiles <- quantile(cars_df$price, probs = c(0.25, 0.50, 0.75))

# Print the quartiles quantile() function with the desired probabilities
print(quartiles)
# Calculate desired quantiles
quantiles <- quantile(cars_df$price, probs = c(0.0, 0.25, 0.50, 0.75, 0.90, 1.00))

# Print the quantiles
print(quantiles)

# is.na(cars) function checks for missing values in the dataset. The sum() function is then used to count the total number of missing values.
missing_values <- sum(is.na(cars_df))

# Remove rows with missing values
# complete.cases(cars_df) function creates a logical vector indicating which rows are complete (i.e., rows without missing values).
cleaned_cars <- cars_df[complete.cases(cars_df), ]

# Print the number of missing values
print("Number of missing values:", missing_values, "\n")

# Print the number of rows after removing missing values
print("Number of rows after removing missing values:", nrow(cleaned_cars), "\n")


# Get the frequency of each model
model_frequency <- table(cars_df$model)
print(model_frequency)
# Get the frequency of each color
color_frequency <- table(cars_df$color)
print(color_frequency)
# Get the frequency of each transmission
transmission_frequency <- table(cars_df$transmission)
print(transmission_frequency)

# Create a box plot to visualize outliers in the Price column
#The box plot visualizes the distribution of the "Price" column, highlighting potential outliers.
boxplot(cars_df$price, main="Boxplot of Price")

# Calculate the interquartile range (IQR)
#The interquartile range (IQR) is calculated to help determine the range for identifying outliers.
Q1 <- quantile(cars_df$price, 0.25)
Q3 <- quantile(cars_df$price, 0.75)
IQR <- Q3 - Q1

# Define upper and lower bounds for outlier detection
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Identify outliers
outliers <- cars_df$price[cars_df$price < lower_bound | cars_df$price > upper_bound]

# Print the identified outliers
print("Identified Outliers:")
print(outliers)


# Create a histogram for the Price column
hist(cars_df$price, main = "Histogram of Price", xlab = "Price")

# Create a histogram for the Mileage column
hist(cars_df$mileage, main = "Histogram of Mileage", xlab = "Mileage")

# Create categorical intervals for the Price column
price_intervals <- c(0, 10000, 20000, 30000, 40000, Inf)
price_labels <- c("0-9999 (VeryLow)", "10000-19999 (Low)", "20000-29999 (Medium)", "30000-39999 (High)", "40000+ (VeryHigh)")

# Add a new column "PriceCategory" to the dataset
#The cut() function is used to create categorical bins or intervals based on numerical data
cars_df$PriceCategory <- cut(cars_df$price, breaks = price_intervals, labels = price_labels, include.lowest = TRUE)

# Display the modified dataset
head(cars_df)
tail(cars_df)
