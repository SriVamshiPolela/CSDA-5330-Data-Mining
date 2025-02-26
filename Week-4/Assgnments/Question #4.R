# Read the BostonHousing dataset which is in mlba package
BostonHousing.df<-read.csv("boston-housing-classification.csv")
# Display the first 6 rows of the dataset
head(BostonHousing.df)

# Remove columns 2, 4, 9, and 12
UpdatedBostonHousing.df<-BostonHousing.df[, -c(2,4,9,12)]
# Display the first 6 rows of the updated dataframe
head(UpdatedBostonHousing.df)

#Create boxplot for each predictor
par(mfcol=c(3, 7))
boxplot(CRIM ~ MEDV_CAT, data = UpdatedBostonHousing.df, ylab = "CRIM")
boxplot(INDUS ~ MEDV_CAT, data = UpdatedBostonHousing.df, ylab = "INDUS")
boxplot(NOX ~ MEDV_CAT, data = UpdatedBostonHousing.df, ylab = "NOX")
boxplot(RM ~ MEDV_CAT, data = UpdatedBostonHousing.df, ylab = "RM")
boxplot(AGE ~ MEDV_CAT, data = UpdatedBostonHousing.df, ylab = "AGE")
boxplot(DIS ~ MEDV_CAT, data = UpdatedBostonHousing.df, ylab = "DIS")
boxplot(TAX ~ MEDV_CAT, data = UpdatedBostonHousing.df, ylab = "TAX")
boxplot(PTRATIO ~ MEDV_CAT, data = UpdatedBostonHousing.df, ylab = "PTRATIO")
boxplot(LSTAT ~ MEDV_CAT, data = UpdatedBostonHousing.df, ylab = "LSTAT")

par(mfcol=c(3, 7))
#Create histogram for each predictor
hist(UpdatedBostonHousing.df$CRIM, main = "Histogram of CRIM", xlab = "CRIM",col = "lightblue",border = "black")
hist(UpdatedBostonHousing.df$INDUS, main = "Histogram of INDUS", xlab = "INDUS",col = "lightblue",border = "black")
hist(UpdatedBostonHousing.df$NOX, main = "Histogram of NOX", xlab = "NOX",col = "blue",border = "black")
hist(UpdatedBostonHousing.df$RM, main = "Histogram of RM", xlab = "RM",col = "blue",border = "black")
hist(UpdatedBostonHousing.df$AGE, main = "Histogram of AGE", xlab = "AGE",col = "red",border = "black")
hist(UpdatedBostonHousing.df$DIS, main = "Histogram of DIS", xlab = "DIS",col = "green",border = "black")
hist(UpdatedBostonHousing.df$TAX, main = "Histogram of TAX", xlab = "TAX",col = "red",border = "black")
hist(UpdatedBostonHousing.df$PTRATIO, main = "Histogram of PTRATIO", xlab = "PTRATIO",col = "red",border = "black")
hist(UpdatedBostonHousing.df$LSTAT, main = "Histogram of LSTAT", xlab = "LSTAT",col = "red",border = "black")



normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#Normalize the data using above function 
UpdatedBostonHousing.df[, c("CRIM", "INDUS","NOX","RM","AGE","DIS","TAX", "PTRATIO","LSTAT")] <- lapply(UpdatedBostonHousing.df[, c("CRIM", "INDUS","NOX","RM","AGE","DIS","TAX", "PTRATIO","LSTAT")], normalize)
head(UpdatedBostonHousing.df)
# Partition data into training 60%, evaluation 20%, and test 20%
library(caret)
set.seed(2023)
data_partition1<-createDataPartition(UpdatedBostonHousing.df$MEDV_CAT, p = 0.6, list = FALSE)
train.df<-UpdatedBostonHousing.df[data_partition1,]
rest.df<-UpdatedBostonHousing.df[-data_partition1,]
data_partition2<-createDataPartition(rest.df$MEDV_CAT, p = 0.5, list = FALSE)
eval.df<-rest.df[data_partition2,]
test.df<-rest.df[-data_partition2,]

# Model-1
# Specify the predictor variables and response variable
predictors <- c("CRIM","INDUS","NOX","RM","AGE","DIS","TAX","PTRATIO","LSTAT")  # Include all relevant predictor variables
predictors
response <- "MEDV_CAT"
# Train the KNN model on the training data
#k = 3  # Choose an appropriate value for k
library(class)
knn_model <- knn(train.df[, predictors], eval.df[, predictors], train.df[, response], 3)
print(knn_model)
# Evaluate the KNN model on the test data
knn_predictions <- knn(train.df[, predictors], test.df[, predictors], train.df[, response], 3)
print(knn_predictions)

# read new file
new_data<-read_excel("BH_classification_New.xlsx")
head(new_data)
new_data[, c("CRIM","INDUS","NOX","RM","AGE","DIS","TAX","PTRATIO","LSTAT")] <- lapply(new_data[, c("CRIM","INDUS","NOX","RM","AGE","DIS","TAX","PTRATIO","LSTAT")], normalize)
head(new_data)
predictors_new<-c("CRIM","INDUS","NOX","RM","AGE","TAX","PTRATIO","LSTAT")
# Apply the model on normalized new dataset
Prediction_New_data <- knn(train.df[, predictors], new_data[,predictors_new], train.df$response, k=3)
#print(PredictionNew_data)

# tried to predict new data but having some NAs in DIS variable 

