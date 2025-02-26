

# Figure 3.1 

# Loading the Boston housing data 
housing.df <- mlba::BostonHousing
# Compute the mean MEDV per CHAS category (0 or 1) using the 'aggregate' function
MEDV.per.CHAS <- aggregate(housing.df$MEDV, by = list(housing.df$CHAS), FUN = mean)

# Rename the columns of the resulting data frame
names(MEDV.per.CHAS) <- c("CHAS", "MeanMEDV")

# Convert the 'CHAS' column to a factor
MEDV.per.CHAS$CHAS <- factor(MEDV.per.CHAS$CHAS)

# Compute the mean CAT.MEDV per CHAS category using the 'aggregate' function
CATMEDV.per.CHAS <- aggregate(housing.df$CAT.MEDV, by = list(housing.df$CHAS), FUN = mean)

# Rename the columns of the resulting data frame
names(CATMEDV.per.CHAS) <- c("CHAS", "MeanCATMEDV")

# Convert the 'CHAS' column to a factor
CATMEDV.per.CHAS$CHAS <- factor(CATMEDV.per.CHAS$CHAS)

# Create a scatter plot of MEDV against LSTAT with labeled axes
plot(housing.df$MEDV ~ housing.df$LSTAT, xlab = "LSTAT", ylab = "MEDV")

# Create a bar plot of CHAS vs. mean MEDV
barplot(MEDV.per.CHAS$MeanMEDV, names.arg = MEDV.per.CHAS$CHAS,
        xlab = "CHAS", ylab = "Avg. MEDV")

# Create a bar plot of CHAS vs. % CAT.MEDV
barplot(CATMEDV.per.CHAS$MeanCATMEDV * 100, names.arg = CATMEDV.per.CHAS$CHAS,
        xlab = "CHAS", ylab = "% of CAT.MEDV")

# Load the 'ggplot2' and 'gridExtra' packages for creating and arranging plots
library(ggplot2)
library(gridExtra)

# Create a scatter plot with labeled axes using the 'ggplot2' package
g1 <- ggplot(housing.df) +
  geom_point(aes(x = LSTAT, y = MEDV), colour = "navy", alpha = 0.5)

# Create a bar plot for CHAS vs. mean MEDV using the 'ggplot2' package
g2 <- ggplot(MEDV.per.CHAS) +
  geom_bar(aes(x = CHAS, y = MeanMEDV, fill = CHAS), stat = "identity")

# Create a bar plot for CHAS vs. % CAT.MEDV using the 'ggplot2' package
g3 <- ggplot(CATMEDV.per.CHAS) +
  geom_bar(aes(x = CHAS, y = MeanCATMEDV, fill = CHAS), stat = "identity") +
  ylab("% of CAT.MEDV")

# Arrange the created plots using 'grid.arrange' from the 'gridExtra' package
grid.arrange(g1, g2, g3, ncol = 2, nrow = 2)



#----------------------------------------------------------------------------
#FIGURE 3.2 DISTRIBUTION PLOTS FOR NUMERICAL VARIABLE MEDV. LEFT: HISTOGRAM AND RIGHT: BOXPLOT

## histogram of MEDV
# Create a histogram of the 'MEDV' column with labeled x-axis
hist(housing.df$MEDV, xlab = "MEDV")

# Create a box plot of 'MEDV' for different values of 'CHAS' with labeled axes
boxplot(housing.df$MEDV ~ housing.df$CHAS, xlab = "CHAS", ylab = "MEDV")

# Create a histogram of 'MEDV' using 'ggplot2', specifying the number of bins and labeling the y-axis
ggplot(housing.df) + geom_histogram(aes(x = MEDV), bins = 9) + ylab("Count")

# Create a box plot of 'MEDV' for different values of 'CHAS' using 'ggplot2', with labeled axes
ggplot(housing.df) + geom_boxplot(aes(x = as.factor(CHAS), y = MEDV)) + xlab("CHAS")


#---------------------------------------------------------------------------------
#Figure 3.4 HEATMAP OF A CORRELATION TABLE. DARK RED VALUES DENOTE STRONG POSITIVE CORRELATION AND DARK BLUE STRONG NEGATIVE CORRELATION
## simple heatmap of correlations (without values)
# Create a heatmap of the correlation matrix of 'housing.df'
heatmap(cor(housing.df), Rowv = NA, Colv = NA)

# Create a heatmap of the correlation matrix with values using the 'gplots' package
# Install the 'gplots' package if not installed
install.packages("gplots")
library(gplots)
heatmap.2(cor(housing.df), Rowv = FALSE, Colv = FALSE, dendrogram = "none",
          cellnote = round(cor(housing.df), 2),
          notecol = "black", key = FALSE, trace = "none", margins = c(10, 10))

# Install the 'reshape' package if not installed
install.packages("reshape")
library(reshape) # Load the 'reshape' package to generate input for the plot

# Calculate and round the correlation matrix
cor.mat <- round(cor(housing.df), 2)

# Melt the correlation matrix for plotting
melted.cor.mat <- melt(cor.mat)

# Create a heatmap of the correlation matrix using 'ggplot2'
ggplot(melted.cor.mat, aes(x = X1, y = X2, fill = value)) +
  geom_tile() + xlab("") + ylab("") +
  scale_fill_distiller(palette = "RdBu", limits = c(-1, 1))



#------------------------------------------------------------------------
#FIGURE 3.11 SCATTER PLOT OF LARGE DATASET WITH REDUCED MARKER SIZE, JITTERING, AND MORE TRANSPARENT COLORING
# use function alpha() in library scales to add transparent colors
install.packages("scales")
library(scales)
# Load the 'mlba' package and load the 'UniversalBank' dataset
universal.df <- mlba::UniversalBank

# Create a subset of 'universal.df' where 'Personal.Loan' is 0
no_personal_loan <- subset(universal.df, Personal.Loan == 0)

# Create a subset of 'universal.df' where 'Personal.Loan' is 1
personal_loan <- subset(universal.df, Personal.Loan == 1)

# Create a scatter plot using base R graphics for 'CCAvg' vs. 'Income' with jittering
plot(jitter(no_personal_loan$CCAvg, 1) ~ jitter(no_personal_loan$Income, 1),
     col = alpha("lightblue", 0.5), pch = 20, xlab = "Income", ylab = "CCAvg")

# Add points for the second subset (personal loan) to the existing plot
points(jitter(personal_loan$CCAvg, 1) ~ jitter(personal_loan$Income, 1),
       col = "steelblue", pch = 20, xlab = "Income", ylab = "CCAvg")

# Create a similar scatter plot using 'ggplot2'
ggplot(universal.df, aes(x = Income, y = CCAvg)) +
  geom_jitter(data = no_personal_loan, width = 5, height = 0.2, alpha = 0.5,
              color = "lightblue") +
  geom_jitter(data = personal_loan, width = 5, height = 0.2, color = "steelblue") +
  labs(colour = "Personal\nLoan")


#--------------------------------------------------------------------------------------
#TABLE 4.3  SUMMARY STATISTICS FOR THE BOSTON HOUSING DATA
# Load the 'mlba' package and load the 'BostonHousing' dataset
boston.housing.df <- mlba::BostonHousing

# Display the first 9 rows of the 'boston.housing.df' dataset
head(boston.housing.df, 9)

# Display summary statistics of the 'boston.housing.df' dataset
summary(boston.housing.df)

# Compute and display the mean of the 'CRIM' variable
mean(boston.housing.df$CRIM)

# Compute and display the standard deviation of the 'CRIM' variable
sd(boston.housing.df$CRIM)

# Compute and display the minimum value of the 'CRIM' variable
min(boston.housing.df$CRIM)

# Compute and display the maximum value of the 'CRIM' variable
max(boston.housing.df$CRIM)

# Compute and display the median value of the 'CRIM' variable
median(boston.housing.df$CRIM)

# Compute and display the length of the 'CRIM' variable
length(boston.housing.df$CRIM)

# Find and display the number of missing values in the 'CRIM' variable
sum(is.na(boston.housing.df$CRIM))

# Compute summary statistics for all variables in the 'boston.housing.df' dataset
data.frame(mean = sapply(boston.housing.df, mean),
           sd = sapply(boston.housing.df, sd),
           min = sapply(boston.housing.df, min),
           max = sapply(boston.housing.df, max),
           median = sapply(boston.housing.df, median),
           length = sapply(boston.housing.df, length),
           miss.val = sapply(boston.housing.df,
                             function(x) sum(length(which(is.na(x))))))

#------------------------------------------------------------------------------------
#TABLE 4.4  CORRELATION TABLE FOR BOSTON HOUSING DATA
# Calculate the correlation matrix for the variables in the 'boston.housing.df' dataset
# and round the correlation coefficients to two decimal places
correlation_matrix <- round(cor(boston.housing.df), 2)


#-------------------------------------------------------------------
#TABLE 4.5  NUMBER OF NEIGHBORHOODS THAT BOUND THE CHARLES RIVER VS. THOSE THAT DO NOT
# Load the 'mlba' package and load the 'BostonHousing' dataset
boston.housing.df <- mlba::BostonHousing

# Create a frequency table of the 'CHAS' variable in the 'boston.housing.df' dataset
# This table displays the counts of occurrences for each value of 'CHAS'
table(boston.housing.df$CHAS)

# Use the 'tidyverse' approach to count occurrences of each value in the 'CHAS' variable
# This code is equivalent to the previous 'table()' function but uses the piping operator (%>%)
boston.housing.df %>% count(CHAS)


#------------------------------------------------------------------------------
#TABLE 4.6  AVERAGE MEDV BY CHAS AND RM
# create bins of size 1
boston.housing.df <- boston.housing.df %>%
  mutate(RM.bin = cut(RM, c(1:9), labels=FALSE))

# compute the average of MEDV by (binned) RM and CHAS
# in aggregate() use the argument by= to define the list of aggregating variables,
# and FUN= as an aggregating function.
aggregate(boston.housing.df$MEDV, by=list(RM=boston.housing.df$RM.bin,
                                          CHAS=boston.housing.df$CHAS), FUN=mean)

# tidyverse version
# Group the 'boston.housing.df' dataset by 'RM.bin' and 'CHAS' columns
# The data will be grouped by the combinations of these two variables
boston.housing.df %>%
  group_by(RM.bin, CHAS) %>%
  
  # Summarize the grouped data by calculating the mean of 'MEDV' for each group
  # The result will show the mean 'MEDV' values for each combination of 'RM.bin' and 'CHAS'
  summarise(mean(MEDV))


#-------------------------------------------------------------------------------
#TABLE 4.10  PCA ON THE TWO VARIABLES CALORIES AND RATING
# Load the 'tidyverse' package for data manipulation and visualization
library(tidyverse)

# Load the 'Cereals' dataset from the 'mlba' package and select the 'calories' and 'rating' columns
cereals.df <- mlba::Cereals %>% select(calories, rating)

# Compute principal components (PCs) on two dimensions using the 'prcomp' function
# The data is taken from the 'calories' and 'rating' columns of the 'cereals.df' dataset
pcs <- prcomp(cereals.df %>% select(calories, rating))

# Display summary statistics of the computed principal components
summary(pcs)

# Extract the rotation matrix from the computed principal components
# The rotation matrix shows the correlation between original variables and principal components
pcs$rot

# Extract the scores (projection of data onto principal components) from the computed principal components
scores <- pcs$x

# Display the first few rows of the calculated scores
head(scores, 5)





