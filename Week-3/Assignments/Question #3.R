
# Figure 5.1 PREDICTION ERROR METRICS FROM A MODEL FOR TOYOTA CAR PRICES. TRAINING AND HOLDOUT
# The tidyverse is a collection of R packages designed to work together for data manipulation, visualization, and analysis. 
library(tidyverse)
set.seed(1)

# Load the ToyotaCorolla dataset, drop columns "Id", "Model", "Fuel_Type", "Color", and remove rows with missing values
car.df <- mlba::ToyotaCorolla %>%
  select(-one_of("Id", "Model", "Fuel_Type", "Color")) %>%
  drop_na()

# Randomly split data into a training set (60%) and a holdout set
idx <- caret::createDataPartition(car.df$Price, p=0.6, list=FALSE)
train.df <- car.df[idx,]
holdout.df <- car.df[-idx,]

# Perform linear regression modeling using the training data
reg <- lm(Price ~ ., data=train.df)
pred_t <- predict(reg)  # Predictions for the training set
pred_h <- predict(reg, newdata=holdout.df)  # Predictions for the holdout set

## Evaluate model performance using Root Mean Squared Error (RMSE)
# RMSE for the training set
caret::RMSE(pred_t, train.df$Price)

# RMSE for the holdout set
caret::RMSE(pred_h, holdout.df$Price)

# Use utility function from the mlba package to calculate various regression metrics for training and holdout sets
rbind(
  Training=mlba::regressionSummary(pred_t, train.df$Price),
  Holdout=mlba::regressionSummary(pred_h, holdout.df$Price)
)


#FIGURE 5.2 CUMULATIVE GAINS CHART (LEFT) AND DECILE LIFT CHART (RIGHT) FOR CONTINUOUS OUTCOME VARIABLE (SALES OF TOYOTA CARS)

# Load necessary libraries
library(ggplot2)
library(gridExtra)

# Install and load the gains package
install.packages("gains")
library(gains)

# Extract price and predictions for holdout data
price <- holdout.df$Price
pred_h <- predict(reg, newdata = holdout.df)

# Compute gains
gain <- gains(price, pred_h)

# Cumulative lift chart
# Prepare data for the cumulative gains chart
df_cumulative <- data.frame(
  ncases = c(0, gain$cume.obs),
  cumPrice = c(0, gain$cume.pct.of.total * sum(price))
)

# Create the cumulative gains chart plot (g1)
g1 <- ggplot(df_cumulative, aes(x = ncases, y = cumPrice)) +
  geom_line() +
  geom_line(data = data.frame(ncases = c(0, nrow(holdout.df)), cumPrice = c(0, sum(price))),
            color = "gray", linetype = 2) +  # Adds baseline
  labs(x = "# Cases", y = "Cumulative Price", title = "Cumulative Gains Chart") +
  scale_y_continuous(labels = scales::comma)

# Decile-wise lift chart
# Prepare data for the decile-wise lift chart
df_decile <- data.frame(
  percentile = gain$depth,
  meanResponse = gain$mean.resp / mean(price)
)

# Create the decile-wise lift chart plot (g2)
g2 <- ggplot(df_decile, aes(x = percentile, y = meanResponse)) +
  geom_bar(stat = "identity") +
  labs(x = "Percentile", y = "Decile mean / global mean", title = "Decile-wise Lift Chart")

# Arrange the two plots side by side
grid.arrange(g1, g2, ncol = 2)
# *************************************Explanation*************************************************************
#In the provided R script, a linear regression model is built to predict car prices using the ToyotaCorolla dataset. 
#The data is split into a training set and a holdout set, and the model's performance is evaluated using Root Mean Squared Error (RMSE) for both sets. 
#Additionally, cumulative gains and decile lift charts are created to visualize the model's performance on the holdout data, offering insights into its predictive accuracy across different percentiles. 
#This script showcases the modeling, evaluation, and visualization steps crucial for assessing the effectiveness of a linear regression model in predicting car prices.