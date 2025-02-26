path = "Z:/CSDA 5330/FA 2023/Assignment 2"
setwd(path)
#load the grocery data into a sparse matrix
library(arules)
library(knitr)
basket <- read.transactions("market.csv", sep = ",")
summary(basket)
'"
transactions as itemMatrix in sparse format with
 9835 rows (elements/itemsets/transactions) and
 169 columns (items) and a density of 0.02609146 

most frequent items:
      whole milk other vegetables       rolls/buns             soda           yogurt          (Other) 
            2513             1903             1809             1715             1372            34055 

element (itemset/transaction) length distribution:
sizes
   1    2    3    4    5    6    7    8    9   10   11   12   13   14   15   16   17   18   19   20   21   22   23   24   26 
2159 1643 1299 1005  855  645  545  438  350  246  182  117   78   77   55   46   29   14   14    9   11    4    6    1    1 
  27   28   29   32 
   1    1    3    1 

   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   3.000   4.409   6.000  32.000 

includes extended item information - examples:
            labels
1 abrasive cleaner
2 artif. sweetener
3   baby cosmetics
"'
# look at the first five transactions
inspect(basket[1:5])
'" 
items                                                                   
[1] {citrus fruit, margarine, ready soups, semi-finished bread}             
[2] {coffee, tropical fruit, yogurt}                                        
[3] {whole milk}                                                            
[4] {cream cheese, meat spreads, pip fruit, yogurt}                         
[5] {condensed milk, long life bakery product, other vegetables, whole milk}
"'
# examine the frequency of items
itemFrequency(basket[, 1:3])

# plot the frequency of items
itemFrequencyPlot(basket, support = 0.1)
itemFrequencyPlot(basket, topN = 20)

# a visualization of the sparse matrix for the first five transactions
image(basket[1:5])

# visualization of a random sample of 100 transactions
image(sample(basket, 100))

## Step 3: Training a model on the data ----
# default settings result in zero rules learned
basketrules <- apriori(basket)
basketrules
#set of 0 rules
# set better support and confidence levels to learn more rules
basketrules <- apriori(basket, parameter = list(support =
                        0.006, confidence = 0.25, minlen = 2))
basketrules
#set of 463 rules

inspect(basketrules)
summary(basketrules)

# look at the first three rules
inspect(basketrules[1:3])
'#
    lhs                rhs               support     confidence coverage   lift     count
[1] {potted plants} => {whole milk}      0.006914082 0.4000000  0.01728521 1.565460 68   
[2] {pasta}         => {whole milk}      0.006100661 0.4054054  0.01504830 1.586614 60   
[3] {herbs}         => {root vegetables} 0.007015760 0.4312500  0.01626843 3.956477 69   
> 
#'

## Step 5: Improving model performance ----

# sorting grocery rules by lift
inspect(sort(basketrules, by = "lift")[1:5])
'#
nspect(sort(basketrules, by = "lift")[1:5])
    lhs                                               rhs                  support     confidence coverage   lift     count
[1] {herbs}                                        => {root vegetables}    0.007015760 0.4312500  0.01626843 3.956477 69   
[2] {berries}                                      => {whipped/sour cream} 0.009049314 0.2721713  0.03324860 3.796886 89   
[3] {other vegetables, tropical fruit, whole milk} => {root vegetables}    0.007015760 0.4107143  0.01708185 3.768074 69   
[4] {beef, other vegetables}                       => {root vegetables}    0.007930859 0.4020619  0.01972547 3.688692 78   
[5] {other vegetables, tropical fruit}             => {pip fruit}          0.009456024 0.2634561  0.03589222 3.482649 93   
#'

# finding subsets of rules containing any berry items
berryrules <- subset(basketrules, items %in% "berries")
inspect(berryrules)
'#
 lhs          rhs                  support     confidence coverage  lift     count
[1] {berries} => {whipped/sour cream} 0.009049314 0.2721713  0.0332486 3.796886  89  
[2] {berries} => {yogurt}             0.010574479 0.3180428  0.0332486 2.279848 104  
[3] {berries} => {other vegetables}   0.010269446 0.3088685  0.0332486 1.596280 101  
[4] {berries} => {whole milk}         0.011794611 0.3547401  0.0332486 1.388328 116  
#'

#creating visualization
plot(basketrules)
#interactive graph
plot(basketrules, method = "graph",  engine = "htmlwidget")

# writing the rules to a CSV file
basketrules.df<-inspect(basketrules)
write.csv(basketrules.df, file = "basketrules.csv")



