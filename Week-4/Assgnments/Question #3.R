library(readxl)
#Reading the Excel file 
emp_salary.df <- read_excel("KNN Employee Salaries.xlsx", sheet = "Data") 
New.df <- read_excel("KNN Employee Salaries.xlsx", sheet = "New Data")
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
#Normalize the data using above function for Salary and Age
emp_salary.df$Salary<-normalize(emp_salary.df$Salary)
emp_salary.df$Age<-normalize(emp_salary.df$Age)
# Display the normalized data
emp_salary.df

# Normalize new data
New.df$Salary<-normalize(New.df$Salary)
New.df$Age<-normalize(New.df$Age)
New.df
# Explore the data using boxplot
boxplot(Salary ~ MBA, data = emp_salary.df, ylab = "Salary")
boxplot(Age ~ MBA, data = emp_salary.df, ylab = "Age")

library(caret)
set.seed(2023)
# data partioning 
data_partiton<-createDataPartition(emp_salary.df$MBA, p = 0.6, list = FALSE)
train_data<-emp_salary.df[data_partiton,]
rest_data<-emp_salary.df[-data_partiton,]

data_partiton1<-createDataPartition(rest_data$MBA, p = 0.5, list = FALSE)
Eval_data<-rest_data[data_partiton1,]
test_data<-rest_data[-data_partiton1,]

library(class)
# Choosing value k value
k=5
model<-knn(train_data[,c("Age","Salary")], Eval_data[,c("Age","Salary")], train_data$MBA, k)
print(model)
# Apply the model on
model_test<-knn(train_data[,c("Age","Salary")], test_data[,c("Age","Salary")], train_data$MBA, k)
print(model_test)

# Apply the model on normalized new dataset
PredictionNew_data <- knn(train_data[,c("Age","Salary")], New.df[,c("Age","Salary")], train_data$MBA, k=5)
print(PredictionNew_data)


