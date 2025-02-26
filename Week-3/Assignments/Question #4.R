library(arules)
library(arulesViz)
Coursetopics.df<-read.csv("Coursetopics.csv")
Coursetopics.mat <- as.matrix(Coursetopics.df)[,-1] #(remove the transaction number)
#Transform the matrix into a transaction list
Coursetopics.trans <- as(Coursetopics.mat, "transactions")
inspect(Coursetopics.trans[1:5])#display the first 5 transactions


# Get the frequency items
item_frequencies <- itemFrequency(Coursetopics.trans, type="absolute")
print(item_frequencies)
# Generate rules with highest lift supp= 0.01, conf = 0.1
CoursetopicsRules1<- apriori(Coursetopics.trans, parameter = list(support=0.01, conf=0.1))
# Sort the rules by lift in decreasing order
CoursetopicsRules_byLift<- sort(CoursetopicsRules1, by ="lift", decreasing = TRUE)
# Display the top 6 rules 
inspect(head(CoursetopicsRules1))

# Generate rules with highest lift supp= 0.01, conf = 0.1 and 0.5
CoursetopicsRules2<- apriori(Coursetopics.trans, parameter = list(support=0.01, conf=0.1))
# Sort the rules by lift in decreasing order
CoursetopicsRules_HighLift<-sort(CoursetopicsRules2, by="lift" , decreasing = TRUE)
inspect(head(CoursetopicsRules2))

# Rules with confidence=0.5
CoursetopicsRules3<- apriori(Coursetopics.trans, parameter = list(support=0.01, conf=0.5))
CoursetopicsRules_HighLift<-sort(CoursetopicsRules3, by="lift" , decreasing = TRUE)
inspect(head(CoursetopicsRules3))

