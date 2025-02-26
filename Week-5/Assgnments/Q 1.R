
#a.	What is the target attribute?
# Dependent variable or predictive variable which is Result in this dataset
#b.	What are classifier attributes?
# Independent variable or predictors are Income and Family_Size

# Read the file
vacationtrip<-read.csv("vacation-trip-classification.csv")
summary(vacationtrip$Result)

#Data Partitioning
library(caret)
set.seed(2015)
Samp_data<-createDataPartition(vacationtrip$Result, p=0.7, list = FALSE)
train<-vacationtrip[Samp_data,]
test<-vacationtrip[-Samp_data,]

#Building Tree (on training dataset):
#install.packages("rpart")
library(rpart)
vacationtrip.tree<-rpart(Result ~ ., data = train, control = rpart.control(minsplit=2, cp=0))

#Visualizing Tree:
#install.packages("rpart.plot")
library(rpart.plot)
prp(vacationtrip.tree, type = 2, extra = 104, nn = TRUE, fallen.leaves = TRUE, faclen = 4, varlen = 8, shadow.col = "green")

# Predicting Training and Test Dataset and Create CM:
pred.train<-predict(vacationtrip.tree, train, type="class")
conf_matrix_train <- table(train$Result,pred.train, dnn=c("Actual", "Predicted"))
print(conf_matrix_train)
pred.test<-predict(vacationtrip.tree, test, type="class")
conf_matrix_test <- table(test$Result, pred.test, dnn=c("Actual", "Predicted"))
print(conf_matrix_test)

# Calculate Accuracy, Sensitivity, and Specificity
accuracy <- sum(diag(conf_matrix_test)) / sum(conf_matrix_test)
specificity <- conf_matrix_test[2, 2] / sum(conf_matrix_test[2, ])
sensitivity <- conf_matrix_test[1, 1] / sum(conf_matrix_test[1, ])

cat("Accuracy:", accuracy)
cat("specificity:", specificity)
cat("sensitivity:", sensitivity)

# Building a Larger Tree
vacationtrip.tree.B<-rpart(Result ~ ., data = train, control = rpart.control(minsplit = 10, cp=0))
prp(vacationtrip.tree.B, type = 2, extra = 104, nn = TRUE, fallen.leaves = TRUE, faclen = 4, varlen = 8, shadow.col = "red")

#Pruning Tree
printcp(vacationtrip.tree.B)
plotcp(vacationtrip.tree.B) #(Visualize the complexity parameters)
vacationtrip.tree.B.Pruned<-prune(vacationtrip.tree.B, 0.018)

# Visualizing Tree
prp(vacationtrip.tree.B.Pruned, type = 2, extra = 104, nn = TRUE, fallen.leaves = TRUE, faclen = 4, varlen = 8, shadow.col = "gray")

# Apply the model to new data
new_data <- read.csv("New vacation-trip.csv")
new_data$Predicted_Result <- predict(vacationtrip.tree, new_data, type = "class")
new_data
# Write the predicted values to new dataset 
write.csv(new_data, "New vacation-trip.csv", row.names = FALSE)

