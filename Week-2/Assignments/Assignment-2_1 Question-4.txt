install.packages('arules')
library(arules)

# Load and prepare transaction data
basket <- read.transactions("market.csv", sep = ",")

class(basket)# This tells the type of dataset 
inspect(basket) # Displays all the transactions 

inspect(basket[1:5]) # Displays the first five rows in dataset

#Applying Apriori Without parameters using function apriori()
rules_without_parameters <- apriori(basket)
print(rules_without_parameters)

# Applying apriori with specified parameters
rules_with_parameters <- apriori(basket, parameter = list(support = 0.006, confidence = 0.25, minlen=2))

print(rules_with_parameters)
summary(rules_with_parameters)


#Install the package arulesviz
install.packages("arulesViz")
library(arulesViz)


# Scatter plot 
# There are no rules generated using default values of apriori like support  and confidence
# There is no graph for this rules
plot(rules_without_parameters, method ='scatter') 

plot(rules_with_parameters, method = "scatter", jitter = 0)

plot(rules_with_parameters, method = "graph")

library(colorspace)
plot(rules_with_parameters, control = list(col=sequential_hcl(100)), jitter=0)
plot(rules_with_parameters, col=sequential_hcl(100), jitter=0)
plot(rules_with_parameters, method = "two-key plot", jitter=0)
#creating interactive plot 
plot(rules_with_parameters, method = "graph",  engine = "htmlwidget")

#subsets of rules containing any berry items
#Subset function filters the rules where the left-hand side (lhs) of the rules contains the item "berries.
#" The %ain% operator is used to check if the lhs of each rule contains the item "berries." 
berry_Subset<-subset(rules_with_parameters, subset = lhs %ain% "berries")
print(berry_Subset)
inspect(berry_Subset)

# Visualization of the berry rules
plot(berry_Subset, control = list(col=sequential_hcl(100)), jitter=0)
plot(berry_Subset, col=sequential_hcl(100), jitter=0)
plot(berry_Subset, method = "two-key plot", jitter=0)


#creating interactive plot for berry rules
plot(berry_Subset, method = "graph",  engine = "htmlwidget")
inspect(berry_Subset)

#inspect the lift values of association rules using the quality() function from the arules package.
lift_values <- quality(berry_Subset)$lift
print(lift_values)

# Write the berry_Subset rules to a CSV file

write.csv(inspect(berry_Subset), file = "basketrules.csv")


