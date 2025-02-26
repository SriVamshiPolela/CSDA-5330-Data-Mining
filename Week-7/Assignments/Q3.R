# Read data from mlba package
RidingMowers.df= mlba::RidingMowers


head(RidingMowers.df)
view(RidingMowers.df)

# a. What percentage of households in the study were owners of a riding mower?
n = count(RidingMowers.df) # Total no of rows
Total_RidingMowers_Owner = sum(RidingMowers.df$Ownership == "Owner")
percentage_owners = (Total_RidingMowers_Owner / n) * 100
print(percentage_owners)

# Create a scatter plot of Income vs. Lot Size using color or symbol to distinguish owners from nonowners.
library(ggplot2)

# Create the scatter plot
ggplot(RidingMowers.df, aes(x = Income, y = Lot_Size, color = Ownership)) +
  geom_point() +
  labs(title = "Scatter Plot of Income vs. Lot Size by Ownership") +
  scale_color_manual(values = c("Owner" = "blue", "Nonowner" = "red"))

# From the scatter plot, which class seems to have a higher average income, owners or nonowners?
# Calculate the mean income for owners and non-owners
mean_income_by_ownership <- aggregate(Income ~ Ownership, data = RidingMowers.df, FUN = mean)

# Print the mean income for each class
print(mean_income_by_ownership)

# use factor before applying the model
RidingMowers.df$Ownership = factor(RidingMowers.df$Ownership)

# Apply logistic Regression on this data where target attribute is Ownership
Riding_LR_Model = glm(RidingMowers.df$Ownership~., data= RidingMowers.df, family = "binomial")

summary(Riding_LR_Model)
# Applying the model on test data
Model_Predictions = predict(Riding_LR_Model, data = RidingMowers.df, type = "response")
Model_Predictions

# Set the cutoff 50% 
RidingMowers.df$Predictions<- ifelse(Model_Predictions>= 0.5, "Owner", "Nonowner")
print(RidingMowers.df)

# Check the levels 
levels(RidingMowers.df$Ownership)
levels(RidingMowers.df$Predictions)
# convert the predictions to categorical using factor
RidingMowers.df$Predictions = factor(RidingMowers.df$Predictions)
levels(RidingMowers.df$Predictions)

confusionMatrix(RidingMowers.df$Predictions, RidingMowers.df$Ownership)

# c. Among nonowners, what is the percentage of households classified correctly?
# based on confusion matrix, percentage is 83%

# d. To increase the percentage of correctly classified nonowners, should the threshold probability be increased or decreased?
#By decreasing the threshold probability, you will classify more observations as "Nonowner" (including some that were previously classified as "Owner"), which can potentially increase the number of True Negatives (TN). 
#This will increase the percentage of correctly classified non-owners.

# e. What are the odds that a household with a $60K income and a lot size of 20,000 ft2 is an owner?
# Given Input values
income = 60
lotsize=20
# Create a data frame with the input values
new_data <- data.frame(Income = income, Lot_Size = lotsize)

Probability_newdata = predict(Riding_LR_Model, new_data, type = "response")
Probability_newdata

# Calculate the odds for Y= Owner
odds = Probability_newdata / (1 - Probability_newdata)
odds

# f)	What is the classification of a household with a $60K income and a lot size of 20,000 ft 2? Use threshold = 0.5.

# Classify based on the threshold
classification <- ifelse(Probability_newdata >= 0.5, "Owner", "Non-Owner")
classification

