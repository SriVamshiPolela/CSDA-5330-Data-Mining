library(readxl)

# Load the data from "Beverage Sales.xlsx"
sales_data <- read_excel("Beverage Sales.xlsx")

# Function to normalize a vector
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Normalize the Temperature and Sales columns
sales_data$Temperature_Normalized <- normalize(sales_data$Temperature)
sales_data$Sales_Normalized <- normalize(sales_data$Sales)
sales_data$Temperature_Normalized
sales_data$Sales_Normalized

# Display the normalized data
print(sales_data[, c("Temperature_Normalized", "Sales_Normalized")])


# Fit a linear regression model on normalized data
model_normalized <- lm(Sales_Normalized ~ Temperature_Normalized, data = sales_data)

# Print the summary of the regression model
summary(model_normalized)


# Create a scatterplot with your data
plot(sales_data$Temperature_Normalized, sales_data$Sales_Normalized, 
     main = "Scatterplot with Trendline", xlab = "Temperature (Normalized)", ylab = "Sales (Normalized)")

# Fit a linear regression model on the normalized data
model_normalized <- lm(Sales_Normalized ~ Temperature_Normalized, data = sales_data)

# Add the linear trendline to the plot
abline(model_normalized, col = "black")

# Extract the coefficients of the linear model
slope <- coef(model_normalized)[2]
intercept <- coef(model_normalized)[1]

# Create the equation text to display on the plot
eq_text <- paste("y = ", round(slope, 2), "* x + ", round(intercept, 2))

# Add the equation text to the plot
text(0.6, 0.8, eq_text, col = "red", cex = 1.2)


# KNN

# Load the necessary library for reading Excel files
library(readxl)

# Read the training data and new data from the Excel file
emp_salary.df <- read_excel("KNN Employee Salaries.xlsx", sheet = "Data")
new_data <- read_excel("KNN Employee Salaries.xlsx", sheet = "New Data")
# Remove irrelevant columns from the training data
emp_salary.df <- emp_salary.df[, c("Employee", "Salary", "Age", "MBA")]

# Remove irrelevant columns from the new data
new_data <- new_data[, c("Employee", "Salary", "Age")]

# Normalize the training data (excluding the Employee and MBA columns)
normalized_data <- scale(emp_salary.df[, c("Salary", "Age")])

# Create a new data frame with the normalized data and non-numeric columns
normalized_emp_salary.df <- data.frame(Employee = emp_salary.df$Employee, MBA = emp_salary.df$MBA, normalized_data)

# Normalize the new data
normalized_new_data <- scale(new_data[, c("Salary", "Age")])

# Create a new data frame with the normalized new data and non-numeric columns
normalized_new_data <- data.frame(Employee = new_data$Employee, normalized_new_data)

# Load the necessary library for K-NN
library(class)

# Extract the predictor variables from the training data
predictors <- normalized_emp_salary.df[, c("Salary", "Age")]

# Extract the response variable from the training data
response <- emp_salary.df$MBA

# Set the number of neighbors (you can change this)
k <- 5

# Fit the K-NN model on the training data
knn_model <- knn(train = predictors, test = normalized_new_data[, c("Salary", "Age")], cl = response, k = k)

# Display the classification results for the new data
new_data_results <- data.frame(Employee = normalized_new_data$Employee, MBA_Predicted = knn_model)
print(new_data_results)

