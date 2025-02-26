library(caret)
# Load the rpart library
library(rpart)
library(rpart.plot)
# Load the dataset
Wine.df <- read.csv("Wine.csv")
set.seed(2023)
Wine_partition <- createDataPartition(Wine.df$Type, p=0.7, list=FALSE )
train.df<-Wine.df[Wine_partition,]
test.df<-Wine.df[-Wine_partition,]
# Build the model using rpart
model<-rpart(Type ~., data=train.df, method="class")
# Predict the model using test data 
Predict_model <- predict(model, test.df, type = "class")
# Create confusion matrix 
confusion_matrix <- table(test.df$Type, Predict_model, dnn=c("Actual", "Predicted"))
confusion_matrix

# Calculate Accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

accuracy
#The accuracy calculated from confusion matrix is approximately 0.846 or 84.6%. 
#This means that the model correctly classified 84.6% of the instances in the validation dataset.


