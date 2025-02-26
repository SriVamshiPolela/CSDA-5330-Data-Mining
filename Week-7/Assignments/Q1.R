library(rpart)
library(caret)
library(rpart.plot)

# a.	Load the bostonhousing.csv into RStudio.
BostonHousing.df = read.csv("boston-housing-classification.csv")

#b.	Explore data. 
#i.	How many rows of data do we have?
nrow(BostonHousing.df)
#ii.	How many attributes?
colnames(BostonHousing.df)
# Check the target attribute
head(BostonHousing.df$MEDV_CAT)
# basic stats
summary(BostonHousing.df)

#c.	Apply classification trees to the MEDV_CAT 
BostonHousing.df.tree = rpart(MEDV_CAT~., data=BostonHousing.df, method = "class")
prp(BostonHousing.df.tree, type = 2, extra = 104, nn = TRUE, fallen.leaves = TRUE, faclen = 4, varlen = 8, shadow.col = "green")

# d.	Compute the a-priori (prior probability of each class) probabilities in the data file. 
# Calculate the count of each class
class_counts = table(BostonHousing.df$MEDV_CAT)
class_counts
# Calculate the total number of instances
total_instances = sum(class_counts)
total_instances
# Compute the a-priori probabilities
prior_probabilities = class_counts / total_instances
prior_probabilities

set.seed(2021)
BostonHousing_partition = createDataPartition(BostonHousing.df$MEDV_CAT, p=0.7, list=FALSE)
train.df = BostonHousing.df[BostonHousing_partition,]
test.df = BostonHousing.df[-BostonHousing_partition,]
head(train.df)
head(test.df)


# f.	Build the tree model. Ignore minisplit, use minibucket = 10 and cp = 0
BostonHousing.df.tree1 = rpart(MEDV_CAT~., data=train.df, method = "class", control = rpart.control(minbucket = 10, cp = 0))
prp(BostonHousing.df.tree1, type = 2, extra = 104, nn = TRUE, fallen.leaves = TRUE, faclen = 4, varlen = 8, shadow.col = "green")

# Interpret the root and the level 1 nodes
# Here the only column used "LSTAT" , 48% of observations are satified as high and 52% as low

# h.	Evaluating the tree by creating confusion matrix of the training and test datasets. 

predict_train= predict(BostonHousing.df.tree1, train.df, type="class")
head(predict_train)

# Check the predicted attribute level and actual attribute level and create a confusion matrix on train data
train.df$MEDV_CAT <- factor(train.df$MEDV_CAT, levels = c("High", "Low"))
levels(predict_train)
levels(train.df$MEDV_CAT)
CM_train = confusionMatrix(predict_train, train.df$MEDV_CAT)
CM_train

# Predicting on test data
predict_test= predict(BostonHousing.df.tree1, test.df, type="class")
head(predict_test)

# Check the predicted attribute level and actual attribute level and create a confusion matrix on test data
test.df$MEDV_CAT <- factor(test.df$MEDV_CAT, levels = c("High", "Low"))
levels(predict_test)
levels(test.df$MEDV_CAT)

CM_test = confusionMatrix(predict_test, test.df$MEDV_CAT)
CM_test


# i.	Rebuild the tree on training dataset. Ignore minibucket, use minisplit = 10 and cp = 0.
BostonHousing.df.tree2 = rpart(MEDV_CAT~., data = train.df, control = rpart.control(minsplit = 10, cp=0), method = "class")
# Create a new tree graph
prp(BostonHousing.df.tree2, type = 2, extra = 104, nn = TRUE, fallen.leaves = TRUE, faclen = 4, varlen = 8, col = "darkblue", shadow.col = "yellow")

# k.	Plot the CP graph of the new tree model. 
plotcp(BostonHousing.df.tree2)
printcp(BostonHousing.df.tree2)
best_cp <- BostonHousing.df.tree2$cptable[which.min(BostonHousing.df.tree2$cptable[, "xerror"]),"CP"]
# What value of the CP gives the best tree model?
# The cp value is 0.13 by graph and accuracy is 87%
# but the accuracy is increased by 3% (i.e., 90%) when i used cp = 0.02 

# l.	Prune the tree with the best CP value.
BostonHousing.df.tree2.prune = prune(BostonHousing.df.tree2, cp = 0.02)
# m.	Create the graph of the pruned tree. 
prp(BostonHousing.df.tree2.prune, type = 2, extra = 104, nn = TRUE, fallen.leaves = TRUE, faclen = 4, varlen = 8, col = "darkblue", shadow.col = "yellow")

# n.	Apply the pruned tree model on the test dataset. Evaluate the confusion matrix of result.
predict_test2 = predict(BostonHousing.df.tree2.prune, test.df, type = "class")
head(predict_test2)

CM_test.prune = confusionMatrix(predict_test2, test.df$MEDV_CAT)
CM_test.prune





