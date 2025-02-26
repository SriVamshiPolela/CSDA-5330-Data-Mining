library(dplyr)
library(rpart)
library(caret)
library(ROSE)

# Read the car cancellation dataset
Car_Cancel.df = read.csv("Taxi_Cancellation_Cleaned.csv")

# Explore the dataset
head(Car_Cancel.df)

# Rowid, vehicle_model_id, package_id, travel_type_id, from_area_id, to_area_id, from_city_id, to_city_id
# The above columns are not used to build a model because IDs won't be involved in any classification, they are used when we want to retrieve a record from dataset.

# Remove these IDs from the dataframe
Car_Cancel.df = Car_Cancel.df[, !names(Car_Cancel.df) %in% c("Rowid", "vehicle_model_id", "package_id", "travel_type_id", "from_area_id", "to_area_id", "from_city_id", "to_city_id")]

head(Car_Cancel.df)

# Count missing values in each column of Car_Cancel.df
missing_counts <- colSums(is.na(Car_Cancel.df))
print(missing_counts)
 
nrow(Car_Cancel.df) # The total number of observations is 10000
ncol(Car_Cancel.df) # Total number of variables we are considering to build a model

Car_Cancel.df$online_booking = factor(Car_Cancel.df$online_booking)
Car_Cancel.df$mobile_site_booking = factor(Car_Cancel.df$mobile_site_booking)
Car_Cancel.df$from_date_DOW = factor(Car_Cancel.df$from_date_DOW)
Car_Cancel.df$from_date_Hour = factor(Car_Cancel.df$from_date_Hour)
Car_Cancel.df$booking_created_DOW = factor(Car_Cancel.df$booking_created_DOW)
# convert the trip lenght to categorical by using cutoff 50%
max(Car_Cancel.df$trip_length)
Car_Cancel.df$trip_length = ifelse(Car_Cancel.df$trip_length >= 26, "High", "Low")
Car_Cancel.df$trip_length = factor(Car_Cancel.df$trip_length)
Car_Cancel.df$Car_Cancellation = factor(Car_Cancel.df$Car_Cancellation)

str(Car_Cancel.df) # Check the type of all variables 



#  ***************** Data partition ***************
set.seed(2023)
data_partition = createDataPartition(Car_Cancel.df$Car_Cancellation, p=0.6, list = FALSE)
train.df= Car_Cancel.df[data_partition,]
test.df = Car_Cancel.df[-data_partition,]


# Check class distribution in train data
table(train.df$Car_Cancellation)
prop.table(table(train.df$Car_Cancellation))



# Without using Imbalancing methods apply classification model
Model_Class = rpart(Car_Cancellation ~., data = train.df, method = "class")
predict_train = predict(Model_Class, train.df, type = "class")
head(predict_train)

predict_test = predict(Model_Class,test.df, type = "class")
head(predict_test)
levels(predict_test)
test.df$Car_Cancellation = factor(test.df$Car_Cancellation)
levels(test.df$Car_Cancellation)
# Confusion matrix 
Test_CM = confusionMatrix(predict_test, test.df$Car_Cancellation)
Test_CM



# **************Using the package ROSE*********************************
Balanced_train_data = ROSE(Car_Cancellation ~., data = train.df, seed=1)$data
# check the class distribution
table(Balanced_train_data$Car_Cancellation)


# ***************** Building a classification Model with ROSE data******************
TreeModel_ROSE = rpart(Car_Cancellation ~., data = Balanced_train_data, method = "class")
predict_test_ROSE = predict(TreeModel_ROSE, newdata = test.df, type = "class")
levels(predict_test_ROSE)
# Confusion Matrix
Test_CM_ROSE = confusionMatrix(predict_test_ROSE, test.df$Car_Cancellation)
Test_CM_ROSE



# ******************Using Oversampling and Undersampling*******************************
Both_Train_data = ovun.sample(Car_Cancellation ~., data = train.df, method = "both" ,p=0.5, N=7000, seed=1)$data
table(Both_Train_data$Car_Cancellation)


# ***************** Building a classification Model with Over and Undersampling data******************
TreeModel_Both = rpart(Car_Cancellation~., data = Both_Train_data, method = "class")
predict_test_Both = predict(TreeModel_Both, newdata = test.df, type = "class")

Test_CM_Both = confusionMatrix(predict_test_Both, test.df$Car_Cancellation)
Test_CM_Both


#Using inbuilt function roc.curve allows us to capture roc metric.

roc.curve(test.df$Car_Cancellation, predict_test)
roc.curve(test.df$Car_Cancellation, predict_test_ROSE)
roc.curve(test.df$Car_Cancellation, predict_test_Both)

# The ROSE and (undersampling & oversmapling (i.e., method ="both"))  gave me almost same result ( Area under the curve (AUC): 0.673) and 0.671
# Also, the accuracy is slightly differ when compared to ROSE and (undersampling & oversmapling)
# Accuracy using ROSE : 0.7187
# Accuracy using Both(undersampling & oversmapling) : 0.7229
