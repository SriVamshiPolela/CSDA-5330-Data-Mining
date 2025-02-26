# Libraries needed
library(caret)
library(tidyverse)

# read the UniversalBank.csv 
UnvBank = read.csv("UniversalBank.csv ")

# Explore the dataset
head(UnvBank)

# Get th stats
summary(UnvBank)
# drop the irrelevant attributes
UnvBank <- UnvBank[, !(names(UnvBank) %in% c("ID", "ZIP.Code"))]
colnames(UnvBank)

# Check the Education attribute
unique(UnvBank$Education)
# Convert the education attribute into categorical format using factor
UnvBank$Education = factor(UnvBank$Education, levels = c(1,2,3), labels = c("Undergrad","Graduate","Advanced/Professional"))
levels(UnvBank$Education)

# Check the target attribute personal Loan
unique(UnvBank$Personal.Loan) # convert the personal loan attribute into categorical format

UnvBank$Personal.Loan = factor(UnvBank$Personal.Loan, levels = c(0,1), labels = c("No","Yes"))
levels(UnvBank$Personal.Loan)

# partition the data
set.seed(2023)
Bank_data_partition = createDataPartition(UnvBank$Personal.Loan, p=0.6, list = FALSE)
train.df = UnvBank[Bank_data_partition,]
test.df = UnvBank[-Bank_data_partition,]

# Apply logistic regression model
logit.model = glm(train.df$Personal.Loan~., data = train.df, family = "binomial")
summary(logit.model)

# Apply the model on test data
PersonalLoan_predicted = predict(logit.model, test.df, type = "response")
test.df$Predicted_loan1 = PersonalLoan_predicted
head(test.df)


# g.	Rebuild the model with only with significant attributes.
logit.model.new = glm(train.df$Personal.Loan ~ Income + Family + CCAvg + Education + Securities.Account + CD.Account + Online + CreditCard,
                       data = train.df, family = "binomial")
summary(logit.model.new)

# h.	Apply the model on the validation set. 
PersonalLoan_predicted_Significant = predict(logit.model.new, test.df, type = "response")
test.df$Predicted_loan2 = PersonalLoan_predicted_Significant
head(test.df)

test.df$predictions <- ifelse(test.df$Predicted_loan2 >= 0.5, "Yes", "No")

test.df$predictions <- factor(test.df$predictions)
head(test.df$predictions)

head(test.df)

# i.	Build the validation dataset confusion matrix with 50% cutoff.

CM_test = confusionMatrix(test.df$predictions, test.df$Personal.Loan)
CM_test

test.df


# Write the data frame to a CSV file
write.csv(test.df, file = "TestPredictions.csv", row.names = FALSE)
