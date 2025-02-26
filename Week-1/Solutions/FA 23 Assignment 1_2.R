getwd()
path = "Z:/CSDA 5330/FA 2023/Assignment 1/"
setwd(path)

'"******************************************************************************
A. Chapter 3

Bostonhousing attributes definition
Format
A data frame with 506 observations and 14 variables:
  1. CRIM per capita crime rate by town
  2. ZN proportion of residential land zoned for lots over 25,000 sq.ft.
  3. INDUS proportion of non-retail business acres per town.
  4. CHAS Charles River dummy variable (1 if tract bounds river; 0 otherwise)
  5. NOX nitric oxides concentration (parts per 10 million)
  6. RM average number of rooms per dwelling
  7. AGE proportion of owner-occupied units built prior to 1940
  8. DIS weighted distances to five Boston employment centres
  9. RAD index of accessibility to radial highways
  10.TAX full-value property-tax rate per $10,000
  11.PTRATIO pupil-teacher ratio by town
  12.LSTAT % lower status of the population
  13.MEDV Median value of owner-occupied homes in $1000
  14.CAT.MEDV
******************************************************************************"'
#_______________________________________________________________________________
#Figure 3.1. Obly the bar chart of the housing dataset
library(mlba)

# Boston housing data
housing.df <- mlba::BostonHousing
# compute mean MEDV per CHAS = (0, 1)
MEDV.per.CHAS  <- aggregate(housing.df$MEDV, by=list(housing.df$CHAS), FUN=mean) 
#Give names to the new vector
names(MEDV.per.CHAS) <- c("CHAS", "MeanMEDV")
MEDV.per.CHAS$CHAS <- factor(MEDV.per.CHAS$CHAS)
# compute % mean CAT.MEDV
CATMEDV.per.CHAS <- aggregate(housing.df$CAT.MEDV, by=list(housing.df$CHAS), FUN=mean)
  names(CATMEDV.per.CHAS) <- c("CHAS", "MeanCATMEDV")
CATMEDV.per.CHAS$CHAS <- factor(CATMEDV.per.CHAS$CHAS)

## scatter plot with axes names for Boston housing data
plot(housing.df$MEDV ~ housing.df$LSTAT, xlab="LSTAT", ylab="MEDV")

## barchart of CHAS vs. mean MEDV
barplot(MEDV.per.CHAS$MeanMEDV,  names.arg=MEDV.per.CHAS$CHAS,
        xlab="CHAS", ylab="Avg. MEDV")

## barchart of CHAS vs. % CAT.MEDV
barplot(CATMEDV.per.CHAS$MeanCATMEDV * 100,  names.arg=CATMEDV.per.CHAS$CHAS,
        xlab="CHAS", ylab="% of CAT.MEDV")

#***************** GGPLOT2 Version *******************************************#
#*
library(ggplot2)
library(gridExtra)

#g1 <- autoplot(ridership.ts) + xlab("Year") + ylab("Ridership (in 000)")

## scatter plot with axes names
g2 <- ggplot(housing.df) +
  geom_point(aes(x=LSTAT, y=MEDV), colour="navy", alpha=0.5)

g3 <- ggplot(MEDV.per.CHAS) +
  geom_bar(aes(x=CHAS, y=MeanMEDV, fill=CHAS), stat="identity")

g4 <- ggplot(CATMEDV.per.CHAS) +
  geom_bar(aes(x=CHAS, y=MeanCATMEDV, fill=CHAS), stat="identity") +
  ylab("% of CAT.MEDV")
g2
grid.arrange(g3, g4, ncol=2, nrow=1)

#_______________________________________________________________________________
#Figure 3.2. Histogram and Boxplot

### Distribution Plots: Boxplots and Histograms

## histogram of MEDV
hist(housing.df$MEDV, xlab="MEDV", col = "Blue")

## boxplot of MEDV for different values of CHAS
boxplot(housing.df$MEDV ~ housing.df$CHAS, xlab="CHAS", ylab="MEDV", col = "Orange")

#ggplot Version
ggplot(housing.df) + geom_histogram(aes(x=MEDV), bins=9) + ylab("Count")

ggplot(housing.df) + geom_boxplot(aes(x=as.factor(CHAS), y=MEDV)) + xlab("CHAS")


ggplot(housing.df) + geom_histogram(aes(x=MEDV), bins=9) + ylab("Count")
ggsave(file=file.path("..", "figures", "chapter_03", "Viz-Histogram.pdf"),
       last_plot() + theme_bw(), width=2.5, height=2, units="in")

ggplot(housing.df) + geom_boxplot(aes(x = as.factor(CHAS), y=MEDV)) + xlab("CHAS")

#_______________________________________________________________________________
#Figure 3.4. Heatmap or correlation matrix
### Heatmaps: Visualizing Correlations and Missing Values

## simple heatmap of correlations (without values)
##Just color shows the intensity of the coorelation between a pair of columns
heatmap(cor(housing.df), Rowv=NA, Colv=NA) 

## heatmap with values
library(gplots)
heatmap.2(cor(housing.df), Rowv=FALSE, Colv=FALSE, dendrogram="none",
          cellnote=round(cor(housing.df),2),
          notecol="black", key=FALSE, trace="none", margins=c(10,10))

library(reshape) # to generate input for the plot
#we want to reshape our matrix for look like a heatmap. we will use the melt()
#here is an example to see the effect of melt() function
'"****************************************************************************"'
A <- c(1,2,3,4,2,3,4,1) 
B <- c(1,2,3,4,2,3,4,1) 
a <- c(10,20,30,40,50,60,70,80) 
b <- c(100,200,300,400,500,600,700,800) 
data <- data.frame(A,B,a,b) 

print("Original data frame:\n") 
print(data) 
'#
  A B  a  b
1 1 1 10 100
2 2 2 20 200
3 3 3 30 300
4 4 4 40 400
5 2 2 50 500
6 3 3 60 600
7 4 4 70 700
8 1 1 80 800
#'
melt_data <- melt(data, id = c("A","B")) 

print("Reshaped data frame:\n") 
print(melt_data) 
'#
   A B variable value
1  1 1        a    10
2  2 2        a    20
3  3 3        a    30
4  4 4        a    40
5  2 2        a    50
6  3 3        a    60
7  4 4        a    70
8  1 1        a    80
9  1 1        b   100
10 2 2        b   200
11 3 3        b   300
12 4 4        b   400
13 2 2        b   500
14 3 3        b   600
15 4 4        b   700
16 1 1        b   800

#'

'"****************************************************************************"'
# first we create the correlation matrix (table) with rounded values
cor.mat <- round(cor(housing.df), 2)
melted.cor.mat <- melt(cor.mat)
ggplot(melted.cor.mat, aes(x=X1, y=X2, fill=value)) +
  geom_tile() + xlab("") + ylab("") +
  scale_fill_distiller(palette="RdBu", limits=c(-1, 1))

#Better formatting the plot. Using the last plot generated
g <- last_plot() + theme_bw() +
  theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1))
g


#_______________________________________________________________________________
#Figure 3.11 Scatter plot of Large dataset with reduce marker size
#use function alpha() in library scales to add transparent colors
library(scales)
universal.df <- mlba::UniversalBank
#get records where personal loan  os equal to 0
no_personal_loan <- subset(universal.df, Personal.Loan == 0)
personal_loan <- subset(universal.df, Personal.Loan == 1)
plot(jitter(no_personal_loan$CCAvg, 1) ~ jitter(no_personal_loan$Income, 1),
     col=alpha("lightblue", 0.5), pch=20, xlab="Income", ylab="CCAvg")
points(jitter(personal_loan$CCAvg, 1) ~ jitter(personal_loan$Income, 1),
       col="steelblue", pch=20, xlab="Income", ylab="CCAvg")


ggplot(universal.df, aes(x=Income, y=CCAvg)) +
  geom_jitter(data=no_personal_loan, width=5, height=0.2, alpha=0.5,
              color="lightblue") +
  geom_jitter(data=personal_loan, width=5, height=0.2, color="steelblue") +
  labs(colour="Personal\nLoan")


g = last_plot() + theme_bw() +
  scale_color_brewer(palette="Set1",
                     guide=guide_legend(override.aes=list(size=3, alpha=1)))
g

'"******************************************************************************
B. Chapter 4
******************************************************************************"'
## Make sure the mlba package is loaded

if (!require(mlba)) {
  library(devtools)
  install_github("gedeck/mlba/mlba", force=TRUE)
}
#_______________________________________________________________________________
##Figure Table 4.3
#Summary statistics
options(dplyr.summarise.inform = FALSE)
library(tidyverse)
library(knitr)
# Dimension Reduction
## Data Summaries
### Example 1: House Prices in Boston
boston.housing.df <- mlba::BostonHousing
head(boston.housing.df, 9)
kable(summary(boston.housing.df))
'"

|   |     CRIM        |      ZN       |    INDUS     |     CHAS       |     NOX       |      RM      |     AGE       |     DIS       |     RAD       |     TAX      |   PTRATIO    |    LSTAT     |     MEDV     |   CAT.MEDV   |
|:--|:----------------|:--------------|:-------------|:---------------|:--------------|:-------------|:--------------|:--------------|:--------------|:-------------|:-------------|:-------------|:-------------|:-------------|
|   |Min.   : 0.00632 |Min.   :  0.00 |Min.   : 0.46 |Min.   :0.00000 |Min.   :0.3850 |Min.   :3.561 |Min.   :  2.90 |Min.   : 1.130 |Min.   : 1.000 |Min.   :187.0 |Min.   :12.60 |Min.   : 1.73 |Min.   : 5.00 |Min.   :0.000 |
|   |1st Qu.: 0.08205 |1st Qu.:  0.00 |1st Qu.: 5.19 |1st Qu.:0.00000 |1st Qu.:0.4490 |1st Qu.:5.886 |1st Qu.: 45.02 |1st Qu.: 2.100 |1st Qu.: 4.000 |1st Qu.:279.0 |1st Qu.:17.40 |1st Qu.: 6.95 |1st Qu.:17.02 |1st Qu.:0.000 |
|   |Median : 0.25651 |Median :  0.00 |Median : 9.69 |Median :0.00000 |Median :0.5380 |Median :6.208 |Median : 77.50 |Median : 3.207 |Median : 5.000 |Median :330.0 |Median :19.05 |Median :11.36 |Median :21.20 |Median :0.000 |
|   |Mean   : 3.61352 |Mean   : 11.36 |Mean   :11.14 |Mean   :0.06917 |Mean   :0.5547 |Mean   :6.285 |Mean   : 68.57 |Mean   : 3.795 |Mean   : 9.549 |Mean   :408.2 |Mean   :18.46 |Mean   :12.65 |Mean   :22.53 |Mean   :0.166 |
|   |3rd Qu.: 3.67708 |3rd Qu.: 12.50 |3rd Qu.:18.10 |3rd Qu.:0.00000 |3rd Qu.:0.6240 |3rd Qu.:6.623 |3rd Qu.: 94.08 |3rd Qu.: 5.188 |3rd Qu.:24.000 |3rd Qu.:666.0 |3rd Qu.:20.20 |3rd Qu.:16.95 |3rd Qu.:25.00 |3rd Qu.:0.000 |
|   |Max.   :88.97620 |Max.   :100.00 |Max.   :27.74 |Max.   :1.00000 |Max.   :0.8710 |Max.   :8.780 |Max.   :100.00 |Max.   :12.127 |Max.   :24.000 |Max.   :711.0 |Max.   :22.00 |Max.   :37.97 |Max.   :50.00 |Max.   :1.000 |

"'
# compute mean, standard dev., min, max, median, length, and missing values of CRIM
mean(boston.housing.df$CRIM)
sd(boston.housing.df$CRIM)
min(boston.housing.df$CRIM)
max(boston.housing.df$CRIM)
median(boston.housing.df$CRIM)
length(boston.housing.df$CRIM)

# find the number of missing values of variable CRIM
sum(is.na(boston.housing.df$CRIM))

# compute mean, standard dev., min, max, median, length, and missing values for all
# variables
kable(data.frame(mean=sapply(boston.housing.df, mean),
           sd=sapply(boston.housing.df, sd),
           min=sapply(boston.housing.df, min),
           max=sapply(boston.housing.df, max),
           median=sapply(boston.housing.df, median),
           length=sapply(boston.housing.df, length),
           miss.val=sapply(boston.housing.df,
                           function(x) sum(length(which(is.na(x))))))
)

'"
|         |        mean|          sd|       min|      max|    median| length| miss.val|
|:--------|-----------:|-----------:|---------:|--------:|---------:|------:|--------:|
|CRIM     |   3.6135236|   8.6015451|   0.00632|  88.9762|   0.25651|    506|        0|
|ZN       |  11.3636364|  23.3224530|   0.00000| 100.0000|   0.00000|    506|        0|
|INDUS    |  11.1367787|   6.8603529|   0.46000|  27.7400|   9.69000|    506|        0|
|CHAS     |   0.0691700|   0.2539940|   0.00000|   1.0000|   0.00000|    506|        0|
|NOX      |   0.5546951|   0.1158777|   0.38500|   0.8710|   0.53800|    506|        0|
|RM       |   6.2846344|   0.7026171|   3.56100|   8.7800|   6.20850|    506|        0|
|AGE      |  68.5749012|  28.1488614|   2.90000| 100.0000|  77.50000|    506|        0|
|DIS      |   3.7950427|   2.1057101|   1.12960|  12.1265|   3.20745|    506|        0|
|RAD      |   9.5494071|   8.7072594|   1.00000|  24.0000|   5.00000|    506|        0|
|TAX      | 408.2371542| 168.5371161| 187.00000| 711.0000| 330.00000|    506|        0|
|PTRATIO  |  18.4555336|   2.1649455|  12.60000|  22.0000|  19.05000|    506|        0|
|LSTAT    |  12.6530632|   7.1410615|   1.73000|  37.9700|  11.36000|    506|        0|
|MEDV     |  22.5328063|   9.1971041|   5.00000|  50.0000|  21.20000|    506|        0|
|CAT.MEDV |   0.1660079|   0.3724560|   0.00000|   1.0000|   0.00000|    506|        0|

"'
#_______________________________________________________________________________
#Figure Table 4.4
# correlation table for Boston Housing dataset
kable(round(cor(boston.housing.df),2))
'"

|         |  CRIM|    ZN| INDUS|  CHAS|   NOX|    RM|   AGE|   DIS|   RAD|   TAX| PTRATIO| LSTAT|  MEDV| CAT.MEDV|
|:--------|-----:|-----:|-----:|-----:|-----:|-----:|-----:|-----:|-----:|-----:|-------:|-----:|-----:|--------:|
|CRIM     |  1.00| -0.20|  0.41| -0.06|  0.42| -0.22|  0.35| -0.38|  0.63|  0.58|    0.29|  0.46| -0.39|    -0.15|
|ZN       | -0.20|  1.00| -0.53| -0.04| -0.52|  0.31| -0.57|  0.66| -0.31| -0.31|   -0.39| -0.41|  0.36|     0.37|
|INDUS    |  0.41| -0.53|  1.00|  0.06|  0.76| -0.39|  0.64| -0.71|  0.60|  0.72|    0.38|  0.60| -0.48|    -0.37|
|CHAS     | -0.06| -0.04|  0.06|  1.00|  0.09|  0.09|  0.09| -0.10| -0.01| -0.04|   -0.12| -0.05|  0.18|     0.11|
|NOX      |  0.42| -0.52|  0.76|  0.09|  1.00| -0.30|  0.73| -0.77|  0.61|  0.67|    0.19|  0.59| -0.43|    -0.23|
|RM       | -0.22|  0.31| -0.39|  0.09| -0.30|  1.00| -0.24|  0.21| -0.21| -0.29|   -0.36| -0.61|  0.70|     0.64|
|AGE      |  0.35| -0.57|  0.64|  0.09|  0.73| -0.24|  1.00| -0.75|  0.46|  0.51|    0.26|  0.60| -0.38|    -0.19|
|DIS      | -0.38|  0.66| -0.71| -0.10| -0.77|  0.21| -0.75|  1.00| -0.49| -0.53|   -0.23| -0.50|  0.25|     0.12|
|RAD      |  0.63| -0.31|  0.60| -0.01|  0.61| -0.21|  0.46| -0.49|  1.00|  0.91|    0.46|  0.49| -0.38|    -0.20|
|TAX      |  0.58| -0.31|  0.72| -0.04|  0.67| -0.29|  0.51| -0.53|  0.91|  1.00|    0.46|  0.54| -0.47|    -0.27|
|PTRATIO  |  0.29| -0.39|  0.38| -0.12|  0.19| -0.36|  0.26| -0.23|  0.46|  0.46|    1.00|  0.37| -0.51|    -0.44|
|LSTAT    |  0.46| -0.41|  0.60| -0.05|  0.59| -0.61|  0.60| -0.50|  0.49|  0.54|    0.37|  1.00| -0.74|    -0.47|
|MEDV     | -0.39|  0.36| -0.48|  0.18| -0.43|  0.70| -0.38|  0.25| -0.38| -0.47|   -0.51| -0.74|  1.00|     0.79|
|CAT.MEDV | -0.15|  0.37| -0.37|  0.11| -0.23|  0.64| -0.19|  0.12| -0.20| -0.27|   -0.44| -0.47|  0.79|     1.00|

"'
#_______________________________________________________________________________
#Figure Table 4.5
# Pivot the Boston Housing for Charles River vs those do not 
### Aggregation and Pivot Tables

boston.housing.df <- mlba::BostonHousing #Repeat Reading dataset
table(boston.housing.df$CHAS)

# tidyverse version
boston.housing.df %>% count(CHAS)
#and wit kable() 
kable(boston.housing.df %>% count(CHAS))
'"
| CHAS|   n|
|----:|---:|
|    0| 471|
|    1|  35|
"'
#_______________________________________________________________________________
##Table 4.6
#aggregating two columns MEDV and CHAS (number betrroms)
# create bins of size 1

boston.housing.df <- mlba::BostonHousing #Repeat Reading dataset
boston.housing.df <- boston.housing.df %>%
  mutate(RM.bin = cut(RM, c(1:9), labels=FALSE))
#here we can create a histogram for number of Room
# compute the average of MEDV by (binned) RM and CHAS
# in aggregate() use the argument by= to define the list of aggregating variables,
# and FUN= as an aggregating function.
aggregate(boston.housing.df$MEDV, by=list(RM=boston.housing.df$RM.bin,
                                          CHAS=boston.housing.df$CHAS), FUN=mean)

# tidyverse version
boston.housing.df %>%
  group_by(RM.bin, CHAS) %>%
  summarise(mean(MEDV))

##______________________________________________________________________________
# Table 4.10
## Principal Components Analysis
### Example 2: Breakfast Cereals

library(tidyverse) #needed if these codes are ran separately
library(ggfortify) #to be able to use autoplot() 
##we read only twoo columns (categories, and rating) 
#but textbook Table 4.11 reads all except manufacturer (mfr and type) 
cereals.df <- mlba::Cereals %>% select(calories, rating) #selected Columns 
# compute PCs on two dimensions, use the scale. = T to run the PCA on normalized data
pcs <- prcomp(cereals.df %>% select(calories, rating), scale. = T)

autoplot(pcs)

summary(pcs)
'"
Importance of components:
                           PC1    PC2
Standard deviation     22.3165 8.8844
Proportion of Variance  0.8632 0.1368
Cumulative Proportion   0.8632 1.0000

"'
pcs$rot
scores <- pcs$x
head(scores, 5)



