
#Table 2.3 (WORKING WITH FILES IN R)

install.packages("devtools")
if (!require(mlba)) {
  library(devtools)
  install_github("gedeck/mlba/mlba", force=TRUE)
}
packageDescription("mlba")# it gives the description of the mlba package
housing.df <- read.csv('WestRoxbury.csv') # load data from file
housing.df <- mlba::WestRoxbury # loading the WestRoxbury dataset from mlba package
dim(housing.df)  # dim() function is used to display number of rows and coloumns
head(housing.df)  # by default head() displays the first 6 rows
View(housing.df)  # Displays the data in new tab

# Practice showing different subsets of the data
housing.df[1:10, 1] # show the first 10 rows of the first column only
housing.df[1:10,] # show the first 10 rows of each of the columns
housing.df[5, 1:10] # show the fifth row of the first 10 columns
housing.df[5, c(1:2, 4, 8:10)] # show the fifth row of some columns
housing.df[, 1] # show the whole first column
housing.df$TOTAL.VALUE  # a different way to show the whole first column
housing.df$TOTAL.VALUE[1:10] # show the first 10 rows of the first column
length(housing.df$TOTAL.VALUE)  # find the length of the first column
mean(housing.df$TOTAL.VALUE)  # find the mean of the first column
summary(housing.df)  # find summary statistics for each column


#---------------------------------------------------------------------------------
#TABLE 2.4  SAMPLING IN R

# random sample of 5 observations
s <- sample(row.names(housing.df), 5) # random() function will get the 5 random rows in housing.df
housing.df[s,]

# oversample houses with over 10 rooms
s <- sample(row.names(housing.df), 5, prob=ifelse(housing.df$ROOMS>10, 0.9, 0.01))
housing.df[s,]

# rebalance
housing.df$REMODEL <- factor(housing.df$REMODEL)
table(housing.df$REMODEL)
upsampled.df <- caret::upSample(housing.df, housing.df$REMODEL, list=TRUE)$x
table(upsampled.df$REMODEL)

#----------------------------------------------------------------------------------

#TABLE 2.6  CREATING DUMMY VARIABLES IN R
install.packages("fastDummies")
library(fastDummies)
install.packages("tidyverse")
library(tidyverse)

housing.df <- dummy_cols(mlba::WestRoxbury,
                         remove_selected_columns=TRUE,  # remove the original column
                         remove_first_dummy=TRUE)  # removes the first created dummy variable
housing.df %>% head(2)

#----------------------------------------------------------------------------------------------------\
#TABLE 2.7  IMPUTING MISSING DATA
# To illustrate missing data procedures, we first convert a few entries for
# BEDROOMS to NA's. Then we impute these missing values using the median of the
# remaining values.
rows.to.missing <- sample(row.names(housing.df), 10)
housing.df[rows.to.missing,]
housing.df[rows.to.missing,]$BEDROOMS <- NA
summary(housing.df$BEDROOMS)
# Now we have 10 NA's and the median of the remaining values is 3.
housing.df[rows.to.missing,median(),na.rm=TRUE]
# replace the missing values using the median of the remaining values
# use median() with na.rm=TRUE to ignore missing values when computing the median.
housing.df <- housing.df %>%
  mutate(BEDROOMS=replace_na(BEDROOMS, median(housing.df$BEDROOMS, na.rm=TRUE)))

summary(housing.df$BEDROOMS)


#---------------------------------------------------------------------------------------------------
#TABLE 2.9  DATA PARTITIONING IN R
housing.df <- mlba::WestRoxbury %>%
  mutate(REMODEL=factor(REMODEL))

# use set.seed() to get the same partitions when re-running the R code.
set.seed(1)

## partitioning into training (60%) and holdout (40%)
# randomly sample 60% of the row IDs for training; the remaining 40% serve as holdout
train.rows <- sample(rownames(housing.df), nrow(housing.df)*0.6)
# collect all the columns with training row ID into training set:
train.df <- housing.df[train.rows,]
# assign row IDs that are not already in the training set, into holdout
holdout.rows <- setdiff(rownames(housing.df), train.rows)
holdout.df <- housing.df[holdout.rows,]

## partitioning into training (50%), validation (30%), holdout (20%)
# randomly sample 50% of the row IDs for training
train.rows <- sample(rownames(housing.df), nrow(housing.df)*0.5)

# sample 30% of the row IDs into the validation set, drawing only from records
# not already in the training set
# use setdiff() to find records not already in the training set
valid.rows <- sample(setdiff(rownames(housing.df), train.rows),
                     nrow(housing.df)*0.3)

# assign the remaining 20% row IDs serve as holdout
holdout.rows <- setdiff(rownames(housing.df), union(train.rows, valid.rows))

# create the 3 data frames by collecting all columns from the appropriate rows
train.df <- housing.df[train.rows,]
valid.df <- housing.df[valid.rows,]
holdout.df <- housing.df[holdout.rows,]

## partitioning into training (60%) and holdout (40%) using caret
set.seed(1)
idx <- caret::createDataPartition(housing.df$TOTAL.VALUE, p=0.6, list=FALSE)
train.df <- housing.df[idx,]
holdout.df <- housing.df[-idx,]

head(holdout.df)
#-----------------------------------------------------------------------------------------------------
#TABLE 2.11  CLEANING AND PREPROCESSING DATA
library(tidyverse)
library(mlba)
library(fastDummies)

housing.df <- mlba::WestRoxbury %>%
  # remove rows with missing values
  drop_na() %>%
  # remove column TAX
  select(-TAX) %>%
  # make REMODEL a factor and convert to dummy variables
  mutate(REMODEL=factor(REMODEL)) %>%
  dummy_cols(select_columns=c('REMODEL'),
             remove_selected_columns=TRUE, remove_first_dummy=TRUE)
summary(housing.df)


