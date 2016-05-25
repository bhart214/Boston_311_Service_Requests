library(dplyr)
library(ggplot2)

sr<-d311
head(sr)
srSubset = select(sr, OPEN_DT, TARGET_DT, CLOSED_DT, OnTime_Status, CASE_TITLE, SUBJECT, REASON, TYPE, QUEUE, Department, neighborhood, LOCATION_ZIPCODE, Source)

#select columns used for Q2
sr2Subset = select(srSubset,OnTime_Status,TYPE,Department, neighborhood)

#plots the data about ontime status
ggplot(sr2Subset,aes(factor(Department),fill=OnTime_Status))+geom_bar(position="dodge")
ggplot(sr2Subset,aes(factor(Department),fill=OnTime_Status))+geom_bar()
ggplot(sr2Subset, aes(factor(OnTime_Status), fill=SUBJECT)) + geom_bar()
ggplot(sr2Subset, aes(factor(OnTime_Status), fill=TYPE)) + geom_bar()
ggplot(sr2Subset, aes(factor(OnTime_Status), fill=neighborhood)) + geom_bar()

# overdue by department
## choose casetitle&ontimestatus to find out the correlation
ObD<-select(sr2Subset,Department,OnTime_Status)
head(ObD)
l<-length(ObD$OnTime_Status)
## change the ontimestatus for ontime =1 &overdue=0
## add 2 cols to the dataframe for calculation
ObD$one<-rep(1,l)
ObD$zero<-rep(1,l)
head(ObD)

#change the "one" col for ontime=1 &overdue=0
for(i in 1:l)
{if(as.character(ObD$OnTime_Status[i])=="OVERDUE"){ObD$one[i]<-0}
}
head(ObD)

# change the col names for better understanding
names(ObD)[3]<-"on_timeN"
names(ObD)[4]<-"requestN"
head(ObD)


## calculate the proportion for each department
ObD<-aggregate(cbind(on_timeN,requestN)~Department,ObD,sum)
ObD<-mutate(ObD,overdueN=requestN-on_timeN)
ObD<-mutate(ObD,Poverdue=overdueN/requestN)

# some change in data
ObD<-arrange(ObD,-Poverdue)
ObD[20:22,1]<-""

# aggregate the result
ObD<-aggregate(cbind(on_timeN,requestN,overdueN,Poverdue)~Department,ObD,sum)
ObD<-arrange(ObD,-Poverdue)
head(ObD)

# number of overdue /proportion of overdue in each department
qplot(ObD$Department,ObD$overdueN,ObD,geom="bar",stat="identity")
qplot(ObD$Department,ObD$Poverdue,ObD,geom="bar",stat="identity")

## overdue by request
#------------------------------------stupid same thing
## choose ontimestatus to find out the correlation
ObR<-select(sr2Subset,TYPE,neighborhood,OnTime_Status)
head(ObR)
l<-length(ObR$OnTime_Status)
## change the ontimestatus for ontime =1 &overdue=0
## add 2 cols to the dataframe for calculation
ObR$one<-rep(1,l)
ObR$zero<-rep(1,l)
head(ObR)

#change the "one" col for ontime=1 &overdue=0
for(i in 1:l)
{if(as.character(ObR$OnTime_Status[i])=="OVERDUE"){ObR$one[i]<-0}
}
head(ObR)

# change the col names for better understanding
names(ObR)[4]<-"on_timeN"
names(ObR)[5]<-"requestN"
head(ObR)
#-------------------------------stupid same thing

## calculate the proportion for each department
ObR<-aggregate(cbind(on_timeN,requestN)~TYPE,ObR,sum)
ObR<-mutate(ObR,overdueN=requestN-on_timeN)
ObR<-mutate(ObR,Poverdue=overdueN/requestN)
ObR<-arrange(ObR,-Poverdue)


