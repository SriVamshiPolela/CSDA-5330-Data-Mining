#Chapter 2 
#Table 2.3
#The file is imported from Github/mlba
#Data Exploration (light)
library(devtools)
install_github("gedeck/mlba/mlba", force=TRUE)
housing.df <- mlba::WestRoxbury # load data from mlba package
dim(housing.df)  # find the dimension of data frame
head(housing.df)  # show the first six rows
View(housing.df)  # show all the data in a new tab

# Practice showing different subsets of the data
housing.df[1:10, 1]  # show the first 10 rows of the first column only
housing.df[1:10, ]  # show the first 10 rows of each of the columns
housing.df[5, 1:10]  # show the fifth row of the first 10 columns
housing.df[5, c(1:2, 4, 8:10)]  # show the fifth row of some columns
housing.df[, 1]  # show the whole first column
housing.df$TOTAL.VALUE  # a different way to show the whole first column
housing.df$TOTAL.VALUE[1:10]  # show the first 10 rows of the first column
length(housing.df$TOTAL.VALUE)  # find the length of the first column
mean(housing.df$TOTAL.VALUE)  # find the mean of the first column
summary(housing.df)  # find summary statistics for each column
#_______________________________________________________________________________
#Table 2.4
#Sampling 
#install_github("gedeck/mlba/mlba", force=TRUE) <- not needed since we already 
#installed it
library(caret)
housing.df <- mlba::WestRoxbury #repeated

# random sample of 5 observations
s <- sample(row.names(housing.df), 5)
housing.df[s,]

# oversample houses with over 10 rooms
s <- sample(row.names(housing.df), 5, prob=ifelse(housing.df$ROOMS>10, 0.9, 0.01))
housing.df[s,]

# rebalance
housing.df$REMODEL <- factor(housing.df$REMODEL)
table(housing.df$REMODEL)
upsampled.df <- caret::upSample(housing.df, housing.df$REMODEL, list=TRUE)$x
table(upsampled.df$REMODEL)

#_______________________________________________________________________________
#Table 2.6
##Creating binary dummies for categorical column (Remodel)
library(fastDummies)
library(tidyverse)
housing.df <- mlba::WestRoxbury #repeated
housing.df <- dummy_cols(mlba::WestRoxbury,
                         remove_selected_columns=TRUE,  # remove the original column
                         remove_first_dummy=TRUE)  # removes the first created dummy variable
housing.df %>% head(2)


#_______________________________________________________________________________
#Table 2.7
#### Missing Values
## replacing with the column values median

# To illustrate missing data procedures, we first convert a few entries for
# BEDROOMS to NA's. Then we impute these missing values using the median of the
# remaining values. 
housing.df <- mlba::WestRoxbury #repeated
Bhousing.df<- as_tibble(housing.df) # to see better the type of column values
print(Bhousing.df)
rows.to.missing <- sample(row.names(housing.df), 10)
housing.df[rows.to.missing,]$BEDROOMS <- NA
summary(housing.df$BEDROOMS)
# Now we have 10 NA's and the median of the remaining values is 3.

# replace the missing values using the median of the remaining values
# use median() with na.rm=TRUE to ignore missing values when computing the median.
housing.df <- housing.df %>%
  replace_na(list(BEDROOMS=median(housing.df$BEDROOMS, na.rm=TRUE)))

summary(housing.df$BEDROOMS)

#_______________________________________________________________________________
#Table 2.9
## Predictive Power and Overfitting
### Creating and Using Data Partitions
#### Holdout Partition
library(dplyr)
housing.df <- mlba::WestRoxbury %>%
  mutate(REMODEL=factor(REMODEL)) #Chanhge the type of the Target column from Char to factor

# use set.seed() to get the same partitions when re-running the R code.
set.seed(2023)

## partitioning into training (60%) and holdout (40%)
# randomly sample 60% of the row IDs for training; the remaining 40% serve
# as holdout
train.rows <- sample(rownames(housing.df), nrow(housing.df)*0.6)
# collect all the columns with training row ID into training set:
train.df <- housing.df[train.rows, ]
# assign row IDs that are not already in the training set, into holdout
holdout.rows <- setdiff(rownames(housing.df), train.rows)
holdout.df <- housing.df[holdout.rows, ]

## partitioning into training (50%), validation (30%), holdout (20%)
# randomly sample 50% of the row IDs for training
train.rows <- sample(rownames(housing.df), nrow(housing.df)*0.5)

# sample 30% of the row IDs into the validation set, drawing only from records
# not already in the training set
# use setdiff() to find records not already in the training set
valid.rows <- sample(setdiff(rownames(housing.df), train.rows), nrow(housing.df)*0.3)

# assign the remaining 20% row IDs serve as holdout
holdout.rows <- setdiff(rownames(housing.df), union(train.rows, valid.rows))

# create the 3 data frames by collecting all columns from the appropriate rows
train.df <- housing.df[train.rows, ]
valid.df <- housing.df[valid.rows, ]
holdout.df <- housing.df[holdout.rows, ]

## partitioning into training (60%) and holdout (40%) using caret
set.seed(2023)
idx <- caret::createDataPartition(housing.df$TOTAL.VALUE, p=0.6, list=FALSE)
#you can do the same as follow. 
#library(caret)
#idx <- createDataPartition(housing.df$TOTAL.VALUE, p=0.6, list=FALSE)
train.df <- housing.df[idx, ]
holdout.df <- housing.df[-idx, ]

#_______________________________________________________________________________
#Table 2.11
library(tidyverse)
library(mlba)
library(fastDummies)

housing.df <- mlba::WestRoxbury %>% drop_na() %>% # remove rows with missing values
  # remove column TAX
  select(-TAX) %>%
  # make REMODEL a factor and convert to dummy variables
  mutate(REMODEL=factor(REMODEL)) %>%
  dummy_cols(select_columns=c('REMODEL'),
             remove_selected_columns=TRUE, remove_first_dummy=TRUE)
#remember that you can write above

#_______________________________________________________________________________
#Question C

#Reading academic xl file
#make sure the working directory is set correctly
path = "Z:/CSDA 5330/FA 2023/Assignment 1/"
setwd(path)
library(readxl)
academic.df<-read_xlsx("academic.xlsx")
#before normalizing
academic.df[,-c(3)]
euclidean.dist <- dist(academic.df[,-c(3)], method = "euclidean")
euclidean.dist
'"
         1         2         3         4         5         6
2  7.000714                                                  
3  0.200000  7.006426                                        
4  7.000714  0.000000  7.006426                              
5  4.019950  3.014963  4.044750  3.014963                    
6  4.004997  3.014963  4.000000  3.014963  0.600000          
7  4.000000 11.000455  4.004997 11.000455  8.009994  8.002500
"'
#normalize dataset
academic.norm <- scale(academic.df[,-c(3)])
euclidean.dist <- dist(academic.norm, method = "euclidean")
euclidean.dist

'"
          1         2         3         4         5         6
2 1.7844829                                                  
3 0.9715041 2.2521145                                        
4 1.7844829 0.0000000 2.2521145                              
5 2.1767020 1.6325265 3.0752452 1.6325265                    
6 1.3807863 1.6325265 0.9811986 1.6325265 2.9145123          
7 0.9811986 2.7416705 1.3807863 2.7416705 2.7615727 2.1897084





"'



