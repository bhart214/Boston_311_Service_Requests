# Read the 311 data csv
sr = read.csv(file.choose())
attach(sr)

# put the table of SUBJECT, REASON, TYPE in a data frame to check 
# if there is missing value like NA or NULL
sub <- as.data.frame(table(SUBJECT))
rea <- as.data.frame(table(REASON))
typ <- as.data.frame(table(TYPE))
sub
rea
typ
# in case that rows with missing value don't show up in the table, see if sum(Freq)==lengh(sr)
sum(sub$Freq)
sum(rea$Freq)
sum(typ$Freq)
# according to the sums, there is no missing value in these 3 columns

# see if there are NAs or "" in SUBJECT column
sr.sub.na <- subset(sr, sr$SUBJECT == NA)
sr.sub.null <- subset(sr, sr$SUBJECT=="")
sr.sub.na
sr.sub.null

# order the data by SUBJECT
sr <- sr[order(sr[,9],decreasing=T),]


# check the combination of SUBJECT, REASON and TYPE
# ----------------------------------------------------------------------------
overdue.subset <- subset(sr, OnTime_Status=="OVERDUE")
ontime.subset <- subset(sr, OnTime_Status=="ONTIME")

?subset
subset

# TYPE and CASE_TITLE are same
as.vector(CASE_TITLE) == as.vector(TYPE)

# get the amount of every reason
sub.rea.len <- aggregate(SUBJECT ~ REASON, sr, length)

# get the amount of every subject
rea.sub.len <- aggregate(REASON ~ SUBJECT, sr, length)

# get all combinations between SUBJECT and REASON
sub.rea.group <- aggregate(TYPE ~ SUBJECT + REASON, sr, length)
# It is reasonable that requests with different reasons were assigned to the same SUBJECT
# Some reasons, to which many requests are related, were assigned to different SUBJECT 

# get all conbinations between SUBJECT and DEAPRTMENT
sub.dep.group <- aggregate(TYPE ~ SUBJECT + Department, sr, length)

# get all kinds of combinations between REASON and TYPE
rea.typ.group <- aggregate(SUBJECT ~ REASON + TYPE, sr, length)
# every reason might have several different types,
# I guess TYPE is more specific reason, while REASON is wider
# However, there are also some types that are allocated into different reasons, but only few of them
# like "Call Log"

# get all combinations in Department, SUBJECT, REASON, TYPE
dsrt.group <- aggregate(CASE_TITLE ~ Department + SUBJECT + REASON + TYPE, sr, length)
dsrt.overdue.group <- aggregate(CASE_TITLE ~ Department + SUBJECT + REASON + TYPE, overdue.subset, length)
dsrt.ontime.group <- aggregate(CASE_TITLE ~ Department + SUBJECT + REASON + TYPE, ontime.subset, length)
# from the number of cases in each Department, maybe we can get misallocations.
# maybe focus on combinations with an amount less than 10

# take the "Public Works Department" in SUBJECT as an example.
# I changed columns from factor to vector so element with Freq==0 won't be there.
sr.pwd <- subset(sr, sr$SUBJECT=="Public Works Department")
pwd.sub <- as.data.frame(table(as.vector(sr.pwd$SUBJECT)))
pwd.rea <- as.data.frame(table(as.vector(sr.pwd$REASON)))
pwd.typ <- as.data.frame(table(as.vector(sr.pwd$TYPE)))
show(pwd.rea)
show(pwd.sub)
show(pwd.typ)

# now extend it to the whole SUBJECT set.
# build a list where each element, named as different SUBJECT
# is a list storing REASON and TYPE it has
sub.list <- list()
sub.list.r <- list()
sub.list.t <- list()
for(sn in as.vector(as.data.frame(table(sr$SUBJECT))$Var1)){
  print(sn)
  class(sn)
  sub.list[[sn]] <- NULL
  sub.list[[sn]]["REASON"] <- as.data.frame(table(as.vector(subset(sr, sr$SUBJECT==sn)$REASON))) # [REASON]
  sub.list[[sn]]["TYPE"] <- as.data.frame(table(as.vector(subset(sr, sr$SUBJECT==sn)$TYPE))) # TYPE
  print(length(sub.list[[sn]]$REASON))
  print(length(sub.list[[sn]]$TYPE))
  #if(length(sub.list[[sn]]$REASON) > 1){sub.list.r[[sn]] <- sub.list[[sn]]}
  #if(length(sub.list[[sn]]$TYPE) > 1){sub.list.t[[sn]]} <- sub.list[[sn]]
}
sub.list
tmp <- sub.list[["Veterans"]]
tmp
sub.list.r[["Veterans"]] <- tmp
sub.list.r
length(tmp)
length(sub.list[["Animal Control"]]$TYPE)


# For REASON
# take the "Sanitation" in REASON as an example.
sr.snt <- subset(sr, sr$REASON=="Sanitation")
table(as.vector(sr.snt$SUBJECT))
snt.sub <- as.data.frame(table(as.vector(sr.snt$SUBJECT)))
snt.rea <- as.data.frame(table(as.vector(sr.snt$REASON)))
snt.typ <- as.data.frame(table(as.vector(sr.snt$TYPE)))
snt.sub
snt.rea
snt.typ
# "Highway Maintenance"
sr.hm <- subset(sr, sr$REASON=="Highway Maintenance")
table(as.vector(sr.hm$SUBJECT))
hm.sub <- as.data.frame(table(as.vector(sr.hm$SUBJECT)))
hm.rea <- as.data.frame(table(as.vector(sr.hm$REASON)))
hm.typ <- as.data.frame(table(as.vector(sr.hm$TYPE)))
hm.sub
hm.rea
hm.typ
# "Street Cleaning"
sr.sc <- subset(sr, sr$REASON=="Street Cleaning")
table(as.vector(sr.sc$SUBJECT))
sc.sub <- as.data.frame(table(as.vector(sr.sc$SUBJECT)))
sc.rea <- as.data.frame(table(as.vector(sr.sc$REASON)))
sc.typ <- as.data.frame(table(as.vector(sr.sc$TYPE)))
sc.sub
sc.rea
sc.typ

# make plot of SUBJECT in every REASON
sr3c <- data.frame(SUBJECT=sr$SUBJECT, REASON=sr$REASON, TYPE=sr$TYPE)
sr3c$SUBJECT <- as.factor(sr$SUBJECT)
sr3c$REASON <- as.factor(sr$REASON)
sr3c$TYPE <- as.factor(sr$TYPE)
library(ggplot2)
histRS <- qplot(REASON, data=sr3c, geom="histogram", fill=SUBJECT)
histRS

  # build a list where each element, named as different REASON
# is a list storing SUBJECT and TYPE it has
rea.list <- list()
rea.list.s <- list()
rea.list.t <- list()
for(sn in as.vector(as.data.frame(table(sr$REASON))$Var1)){
  #sn <- as.vector[sn]
  print(sn)
  class(sn)
  rea.list[[sn]] <- NULL
  rea.list[[sn]]["SUBJECT"] <- as.data.frame(table(as.vector(subset(sr, sr$REASON==sn)$SUBJECT))) # SUBJECT
  rea.list[[sn]]["TYPE"] <- as.data.frame(table(as.vector(subset(sr, sr$REASON==sn)$TYPE))) # TYPE
}
rea.list


# find out which reason was assigned to different subjects
Rlevel <- levels(factor(sr3c$REASON)) # get levels of all kinds of REASON
RSlist <- list() # use this list to store types of SUBJECT for every REASON
for(i in c(1:length(Rlevel))){ # the process of getting RSlist
  subtmp <- subset(sr3c, sr3c$REASON==Rlevel[i])
  Sc <- levels(factor(subtmp$SUBJECT))
  RSlist[[Rlevel[i]]] <- Sc
}
# find out REASON with more than one SUBJECT
RSlistmt1 <- RSlist
i <- 1
while(i <= length(RSlistmt1)){
  if(length(RSlistmt1[[i]])<=1){
    RSlistmt1[[i]] <- NULL
  } else {
    i <- i + 1
  }
}
RSlistmt1


