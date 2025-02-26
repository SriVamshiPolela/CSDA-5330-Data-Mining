# Load all required packages 
library(e1071)
library(tidyverse)
library(caret)
# Load the flight delays dataset and preprocess 
FlightDelays = read.csv("FlightDelays.csv")%>%
  mutate(
    DEP_TIME_BLK = factor(DEP_TIME_BLK),
    CARRIER = factor(CARRIER), 
    DEST = factor(DEST), 
    ORIGIN = factor(ORIGIN),
    DAY_WEEK = factor(DAY_WEEK),
    Flight.Status = factor(Flight.Status)
) %>% 
  select(DEP_TIME_BLK,CARRIER,DEST,ORIGIN,DAY_WEEK,Flight.Status)

summary(FlightDelays)

set.seed(2023)

# b) Partitioning the dataset into training and testing 
data_partition<-createDataPartition(FlightDelays$Flight.Status, p=0.6, list = FALSE)
train.df<-FlightDelays[data_partition,]
test.df<-FlightDelays[-data_partition,]

#c) Building the model using training dataset
FlightDelays.nb=naiveBayes(Flight.Status~.,data = train.df)
# Display  A-priori probabilities 
FlightDelays.nb


# d)	Extract the posterior and prior probability of your model
prior_probability <- FlightDelays.nb$apriori
posterior_probability <- FlightDelays.nb$tables

prior_probability
posterior_probability

pred_train<-predict(FlightDelays.nb, train.df)
# e)	Use the model and predict the flight status on test dataset.
pred_test <- predict(FlightDelays.nb, test.df)


# CONFUSION MATRICES USING A NAIVE BAYES CLASSIFIER for train data
confusion_matrix_train= confusionMatrix(data = pred_train, reference = train.df$Flight.Status)
confusion_matrix_train
#f) CONFUSION MATRICES USING A NAIVE BAYES CLASSIFIER for test data
confusion_matrix_test= confusionMatrix(data = pred_test, reference = test.df$Flight.Status)
confusion_matrix_test

# g)Create a data frame for the test dataset which has the actual and predicted values.
test_result <- data.frame(test.df$Flight.Status, pred_test)

#h)	Save the data frame in a csv file. 
write.csv(test_result, "flight_results.csv", row.names = FALSE)


test.df$flight_status_predictions = pred_test
view(test.df)
write.csv(test.df, "FlightResults.csv", row.names = FALSE)
