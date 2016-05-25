library(ggplot2)
library(dplyr)
library(lubridate)
library(qcc)

#By converting into tables, we see that there is 1 entry without a subject and 29,251 entries without neighborhoods

SUBTab <- table(sr$SUBJECT)
NeighTab <- table(sr$neighborhood)
#Basic Barplots with not much qualitative data easily available to see

barplot(SUBTab)
barplot(NeighTab)

#Create Data Frame for subject and neighborhood, renaming the columns as well, also sorted them in descending frequency order

SUB.DF <- as.data.frame(table(sr$SUBJECT))
colnames(SUB.DF) <- c("SUBJECT", "FREQ")
SUB.DF <- SUB.DF[order(SUB.DF$FREQ, decreasing=TRUE), ]


NEIGH.DF <- as.data.frame(table(sr$neighborhood))
colnames(NEIGH.DF) <- c("NEIGHBORHOOD", "FREQ")
NEIGH.DF <- NEIGH.DF[order(NEIGH.DF$FREQ, decreasing=TRUE), ]


#Looking at neighborhood fluctuations...ugly plot though
ggplot(NEIGH.DF, aes(x = NEIGHBORHOOD, y = FREQ, stat="bin")) + geom_point()

#Quick glance shows that most of our neighborhoods are concentrated between 0 and 40,000 cases
hist(NeighTab)

# Creating a dataframe aggregating Neighborhood/Department combinations
NeighDep <- aggregate(CASE_ENQUIRY_ID ~ neighborhood + SUBJECT, sr, length)
NeighDep2 <- as.data.frame(NeighDep)
colnames(NeighDep2) <- c("Neighborhood", "Subject", "Freq")

# Orders the Neighborhood/Department Combo by decreasing freqency
NeighDep2 <- NeighDep2[order(NeighDep2$Freq, decreasing=TRUE), ]

# Let's us take a look at just the breakdown within Dorchester
Dorch <- NeighDep2[NeighDep2$Neighborhood == "Dorchester", ]
Dorch

# Finds the percentage of all Dorchester cases that each department receives
Dorch$Percent <- (Dorch$Freq)/(sum(Dorch$Freq)) * 100
NeighDepCopy <- NeighDep2
NeighDepCopy$PercentTotal <- NeighDepCopy$Freq/sum(NeighDepCopy$Freq) *100

#Gets the total percentage of all cases coming from each neighborhood
AggByNeigh <- aggregate(PercentTotal ~ Neighborhood, NeighDepCopy, sum)
AggByNeigh <- AggByNeigh[order(AggByNeigh$PercentTotal, decreasing=TRUE), ]

#Gets the total percentage of all cases that each department receives
#Trying to figure out why the notation is different than by neighborhood
AggByDep <- aggregate(PercentTotal ~ Subject, NeighDepCopy, sum)
AggByDep <- AggByDep[order(AggByDep$PercentTotal, decreasing=TRUE), ]

#Dorchester Barchart
ggplot(data=Dorch, aes(x=Subject, y=Freq)) + geom_bar(stat="identity")
ggplot(data=Dorch, aes(x=Subject, y=Percent)) + geom_bar(stat="identity")



