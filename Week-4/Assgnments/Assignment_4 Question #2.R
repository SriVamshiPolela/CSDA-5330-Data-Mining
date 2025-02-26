library(readxl)
#Reading the Excel file 
BeverageSales.df <- read_excel("Beverage Sales.xlsx")
head(BeverageSales.df)
plot(BeverageSales.df$Temperature,BeverageSales.df$Sales)
# Function to perform z-score normalization
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

BeverageSales.df$Temperature <- normalize(BeverageSales.df$Temperature)
BeverageSales.df$Sales <- normalize(BeverageSales.df$Sales)

BeverageSales.df
BeverageSales_Reg<-lm(BeverageSales.df$Sales ~ ., data = BeverageSales.df)
summary(BeverageSales_Reg)


# Create a scatterplot of your data
plot(BeverageSales.df$Temperature, BeverageSales.df$Sales, 
     main = "Scatterplot with Regression Line",
     xlab = "Temperature",
     ylab = "Sales")

# Add the regression line to the plot
abline(BeverageSales_Reg, col = "red")

