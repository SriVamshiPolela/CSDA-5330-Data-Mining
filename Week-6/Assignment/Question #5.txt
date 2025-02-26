#a.	What is the target attribute?
# acceptability is the target attribute. 

#b.	What are the possible values (classes) of the target attribute?
#   There are 4 classes for the target attribute are as follows:
#   1. unacc
#   2. acc
#   3. good
#   4.vgood


# Naïve Bayes function, “navieBayes”, is in “e1071” package
library(e1071)
library(tidyverse)
library(caret)

#Load the cars-classification 
cars.df=read.csv("car-classification.csv") %>%
  mutate(
    price = factor(price),
    maint = factor(maint), 
    doors = factor(doors), 
    persons = factor(persons),
    boot = factor(boot),
    safety = factor(safety),
    acceptability = factor(acceptability)
    
  ) %>% 
  select(price, maint, doors, persons, boot, safety, acceptability)

# Make sure reading the dataset into dataframe
head(cars.df)

View(cars.df)


set.seed(2023)

# Partitioning the dataset into training and testing 
data_partition<-createDataPartition(cars.df$acceptability, p=0.7, list = FALSE)
train.df<-cars.df[data_partition,]
test.df<-cars.df[-data_partition,]

#Building the model using training dataset
cars.nb=naiveBayes(acceptability~.,data = train.df)

# Display  A-priori probabilities 
cars.nb

#e.	From the model created, what is the a-priori probability of acceptability?
# Y
#acc       good      unacc      vgood 
#0.22213047 0.04046243 0.69942197 0.03798514

# f.Which value of acceptability has the highest a-priori* probability?
# unacc (unaccepted) has the highest a-priori* probability

# g. Look and conditional probabilities for safety. 
# Explain what the three elements on the first row mean. What conditional probabilities they represent?

# At safety level 1, there are no acceptance because the probability is 0
# At safety level 2, there is a 48.33% chance of acceptance.
# At safety level 3, there is a 51.67% chance of acceptance.

# Run the model with trained data to create predictions
pred_train <- predict(cars.nb, train.df)

#Run the model with the test data to create predictions 
pred_test <- predict(cars.nb, test.df)

#CONFUSION MATRICES USING A NAIVE BAYES CLASSIFIER
# For training dataset
confusion_matrix_train = confusionMatrix(pred_train, train.df$acceptability)
confusion_matrix_train
# For testing dataset
confusion_matrix_test = confusionMatrix(pred_test, test.df$acceptability)
confusion_matrix_test
