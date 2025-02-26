library(caret)
# Load the rpart library
library(rpart)
library(rpart.plot)
# Load the dataset
carseats <- read.csv("carseats.csv")

# It will Display the structure of the dataset means the datatype of each variable 
str(carseats)

# Display summary statistics of the numeric variables
summary(carseats)

# Create the binary response variable High_Sales
carseats$High_Sales <- ifelse(carseats$Sales > 8, "High", "Not High")
head(carseats)
# Box plot of Sales by ShelveLoc
boxplot(Sales ~ ShelveLoc, data = carseats, ylab = "Sales")

# Histogram of Income
hist(carseats$Income, main = "Income Distribution")

# Split the data into training (70%) and testing (30%) sets
set.seed(2023)  # For reproducibility
carseats_partition <- createDataPartition(carseats$High_Sales, p=0.7, list = FALSE)
train_data <- carseats[carseats_partition, ]
test_data <- carseats[-carseats_partition, ]


# Build the decision tree model
tree_model <- rpart(High_Sales ~ ., data = train_data, method = "class")

# Make predictions on the test data
predictions <- predict(tree_model, test_data, type = "class")
predictions.head()
# Create a confusion matrix
confusion_matrix <- table(test_data$High_Sales, predictions, dnn=c("Actual", "Predicted"))

# Display the confusion matrix
confusion_matrix

# The model accuracy is 100% 

# Visualize the decision tree
prp(tree_model, type = 2, extra = 101, fallen.leaves = TRUE)

#accuracy, sensitivity, and specificity from the confusion matrix
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
sensitivity <- confusion_matrix[1, 1] / sum(confusion_matrix[1, ])
specificity <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])


# Print 
cat("Accuracy:", round(accuracy, 2), "\n")
cat("Sensitivity (True Positive Rate):", round(sensitivity, 2), "\n")
cat("Specificity (True Negative Rate):", round(specificity, 2), "\n")


#It correctly predicted all instances of "High" sales.
#It also correctly predicted all instances of "Not High" sales.
#As a result, both sensitivity (True Positive Rate) and specificity (True Negative Rate) are 100%, and the model achieved a perfect accuracy of 100%
