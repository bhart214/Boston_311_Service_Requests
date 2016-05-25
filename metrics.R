
# The following script loads the data and organizes it with Quality of Service Metrics.

#load packages
library(lubridate)
library(ggplot2)
library(dplyr)
library(reshape)
library(tidyr)
library(ggvis)
library(grid)
library(gridExtra)
library(aplpack)
library(plotrix)

setwd("~/R/rData/311_Service_Requests/311_SR_Data")

# read csv file and make a copy; change all blank cells to NA
sr = read.csv("311_Service_Requests.csv", header=T, na.strings=c("","NA"))
srCopy = sr

str(sr)


# format as tbl_df so dplyr can be utilized
sr = tbl_df(sr)

# create selection of specific variables
srSelect = sr  %>% select(OPEN_DT, TARGET_DT, CLOSED_DT, OnTime_Status, CLOSURE_REASON, CASE_TITLE, SUBJECT, REASON, TYPE, QUEUE, Department, neighborhood, LOCATION_ZIPCODE, LATITUDE, LONGITUDE, land_usage, Source)

# format dates
srSelect$OPEN_DT = mdy_hms(srSelect$OPEN_DT)
srSelect$TARGET_DT = mdy_hms(srSelect$TARGET_DT)
srSelect$CLOSED_DT = mdy_hms(srSelect$CLOSED_DT)

# add columns for time differences between OPEN_DT and TARGET_DT and between TARGET_DT and CLOSED_DT
# times are measured in seconds
srSelect = srSelect %>% mutate(target_minus_closed = as.numeric(difftime(TARGET_DT,CLOSED_DT)))
srSelect = srSelect %>% mutate(target_minus_open = as.numeric(difftime(TARGET_DT,OPEN_DT)))
srSelect = srSelect %>% mutate(closed_minus_open = as.numeric(difftime(CLOSED_DT,OPEN_DT)))

# add columns for day, week, month, year
srSelect = srSelect %>% 
  mutate(day = wday(srSelect$OPEN_DT), week = week(srSelect$OPEN_DT), month = month(srSelect$OPEN_DT), year = year(srSelect$OPEN_DT))
srSelect = srSelect %>% mutate(days_past_target = -(trunc(target_minus_closed/86400) + 1))

# select only data from 2014
srSelect = srSelect %>% filter(year == 2014)

# add QoS Ratio Metric
srSelect = srSelect %>% mutate(qosRatio = (target_minus_open/(median(target_minus_open/closed_minus_open, na.rm = TRUE)*closed_minus_open)))

# select only data that is not employee generated... this will remove data from all but 8 departments
srSelect = srSelect %>% filter(Source != "City Worker App" & Source != "Employee Generated")

# select only data where TARGET_DT does not equal CLOSED_DT
srSelect = srSelect %>% filter(TARGET_DT != OPEN_DT)

# filter out cases that don't have a CLOSED_DT
srSelect1 = srSelect %>% filter(!is.na(CLOSED_DT))

# filter out cases with durations of less than 5 min, 15, min, ...
srSelect1 = srSelect1 %>% filter(closed_minus_open > 300)
srSelect2 = srSelect1 %>% filter(closed_minus_open > 900)

# ALSO REMOVE "INVALID" and "DUPLICATE" REQUESTS (SEE CLOSURE REASON)


View(srSelect)

# Quality of Service Metrics by Type
# metrics1_type contains average days past target, median days past target, average duration (open to close), median duration, consistency (std dev), and number of requests
metrics1_type = srSelect %>% 
  select(SUBJECT, REASON, TYPE, neighborhood, closed_minus_open, days_past_target)  %>% 
  group_by(SUBJECT, REASON, TYPE, neighborhood) %>% 
  summarize(avg_days_past_target = mean(days_past_target, na.rm=TRUE), med_days_past_target = median(days_past_target, na.rm=TRUE), avg_dur = mean(closed_minus_open, na.rm=TRUE), med_dur = median(closed_minus_open, na.rm=TRUE), consistency = 1/sd(closed_minus_open, na.rm = TRUE), numRequests = n())
View(metrics1_type)

# metrics2_type contains proportion of overdue requests by TYPE
metrics2_type = srSelect %>% 
  select(SUBJECT, REASON, TYPE, neighborhood, OnTime_Status) %>% 
  group_by(TYPE, neighborhood, OnTime_Status) %>% 
  tally %>% group_by(TYPE, neighborhood) %>% 
  mutate(prop_overdue = n/sum(n)) %>% 
  filter(OnTime_Status=="OVERDUE") %>% 
  select(prop_overdue)
View(metrics2_type)

# metrics3_type contains min and max counts by type per month by type
metrics3_type = srSelect %>% 
  group_by(TYPE, neighborhood, month) %>% 
  summarize(n=n()) %>% 
  group_by(TYPE, neighborhood) %>% 
  summarize(max = max(n), min = min(n))
View(metrics3_type)

# metrics4_type contains the qosRatio by Type
metrics4_type = srSelect %>% 
  select(SUBJECT, REASON, TYPE, neighborhood, qosRatio)  %>% 
  group_by(TYPE, neighborhood) %>% 
  summarize(qosRatio = median(qosRatio, na.rm=TRUE))
View(metrics4_type)

# combine metrics1_type, metrics2_type, metrics3_type, and metrics4_type into one data frame
metricsForTypes = full_join(full_join(full_join(metrics1_type, metrics2_type, by = c("TYPE", "neighborhood")), metrics3_type, by = c("TYPE", "neighborhood")), metrics4_type, by = c("TYPE", "neighborhood"))
View(metricsForTypes)
metricsForTypes = metricsForTypes %>% filter(numRequests > 1)
View(metricsForTypes)




# Quality of Service Metrics by REASON
# metrics1_reason contains average days past target, median days past target, average duration (open to close), median duration, consistency (std dev), and number of requests
metrics1_reason = srSelect %>% 
  select(SUBJECT, REASON, neighborhood, closed_minus_open, days_past_target)  %>% 
  group_by(SUBJECT, REASON, neighborhood) %>% 
  summarize(avg_days_past_target = mean(days_past_target, na.rm=TRUE), med_days_past_target = median(days_past_target, na.rm=TRUE), avg_dur = mean(closed_minus_open, na.rm=TRUE), med_dur = median(closed_minus_open, na.rm=TRUE), consistency = 1/sd(closed_minus_open, na.rm = TRUE), numRequests = n())
View(metrics1_reason)

# metrics2_reason contains proportion of overdue requests by TYPE
metrics2_reason = srSelect %>% 
  select(SUBJECT, REASON, neighborhood, OnTime_Status) %>% 
  group_by(REASON, neighborhood, OnTime_Status) %>% 
  tally %>% group_by(REASON, neighborhood) %>% 
  mutate(prop_overdue = n/sum(n)) %>% 
  filter(OnTime_Status=="OVERDUE") %>% 
  select(prop_overdue)
View(metrics2_reason)

# metrics3_reason contains min and max counts by type per month by type
metrics3_reason = srSelect %>% 
  group_by(REASON, neighborhood, month) %>% 
  summarize(n=n()) %>% 
  group_by(REASON, neighborhood) %>% 
  summarize(max = max(n), min = min(n))
View(metrics3_reason)

# metrics4_reason contains the qosRatio by Type
metrics4_reason = srSelect %>% 
  select(SUBJECT, REASON, neighborhood, qosRatio)  %>% 
  group_by(REASON, neighborhood) %>% 
  summarize(qosRatio = median(qosRatio, na.rm=TRUE))
View(metrics4_reason)

# combine metrics1_reason, metrics2_reason, metrics3_reason, and metrics4_reason into one data frame
metricsForReasons = full_join(full_join(full_join(metrics1_reason, metrics2_reason, by = c("REASON", "neighborhood")), metrics3_reason, by = c("REASON", "neighborhood")), metrics4_reason, by = c("REASON", "neighborhood"))
View(metricsForReasons)
#metricsForReasons = metricsForReasons %>% filter(numRequests > 1)
#View(metricsForReasons)


# Quality of Service Metrics by SUBJECT
# metrics1_subject contains average days past target, median days past target, average duration (open to close), median duration, consistency (std dev), and number of requests
metrics1_subject = srSelect %>% 
  select(SUBJECT, neighborhood, closed_minus_open, days_past_target)  %>% 
  group_by(SUBJECT, neighborhood) %>% 
  summarize(avg_days_past_target = mean(days_past_target, na.rm=TRUE), med_days_past_target = median(days_past_target, na.rm=TRUE), avg_dur = mean(closed_minus_open, na.rm=TRUE), med_dur = median(closed_minus_open, na.rm=TRUE), consistency = 1/sd(closed_minus_open, na.rm = TRUE), numRequests = n())
View(metrics1_subject)

# metrics2_subject contains proportion of overdue requests by SUBJECT
metrics2_subject = srSelect %>% 
  select(SUBJECT, neighborhood, OnTime_Status) %>% 
  group_by(SUBJECT, neighborhood, OnTime_Status) %>% 
  tally %>% group_by(SUBJECT, neighborhood) %>% 
  mutate(prop_overdue = n/sum(n)) %>% 
  filter(OnTime_Status=="OVERDUE") %>% 
  select(prop_overdue)
View(metrics2_subject)

# metrics3_subject contains min and max counts by type per month by SUBJECT
metrics3_subject = srSelect %>% 
  group_by(SUBJECT, neighborhood, month) %>% 
  summarize(n=n()) %>% 
  group_by(SUBJECT, neighborhood) %>% 
  summarize(max = max(n), min = min(n))
View(metrics3_subject)

# metrics4_subject contains the qosRatio by SUBJECT
metrics4_subject = srSelect %>% 
  select(SUBJECT, neighborhood, qosRatio)  %>% 
  group_by(SUBJECT, neighborhood) %>% 
  summarize(qosRatio = median(qosRatio, na.rm=TRUE))
View(metrics4_subject)

# combine metrics1_subject, metrics2_subject, metrics3_subject, and metrics4_subject into one data frame
metricsForSubjects = full_join(full_join(full_join(metrics1_subject, metrics2_subject, by = c("SUBJECT", "neighborhood")), metrics3_subject, by = c("SUBJECT", "neighborhood")), metrics4_subject, by = c("SUBJECT", "neighborhood"))
View(metricsForSubjects)
metricsForSubjects = metricsForSubjects %>% filter(numRequests > 1)
View(metricsForSubjects)





# and now for a little chernoff fun!
sub_stats = metricsForSubjects %>% 
  group_by(SUBJECT) %>% 
  summarize_each(funs(mean(., na.rm = TRUE))) %>% 
  select(-neighborhood)
View(sub_stats)

# replace NA's with zeros (this is only done for the 'consistency' metric for cases in which numRequests = 1)
sub_stats[is.na(sub_stats)] = 0


# scale each column and transpose
sub_stats_scaled = sub_stats %>% mutate_each(funs(scale), -SUBJECT)
for(i in 2:ncol(sub_stats_scaled)) {
  sub_stats_scaled[,i] <- as.numeric(as.character(unlist(sub_stats_scaled[,i])))
}
View(sub_stats_scaled)


sub_stats_scaled_trans = sub_stats_scaled %>% t() %>% as.data.frame()
colnames(sub_stats_scaled_trans) = c("BPS","BWSC","InspServ","MayorHL","ParksRec","PropMgmt","PWD","Transport") # the first row will be the header
sub_stats_scaled_trans = sub_stats_scaled_trans[-1,]

str(sub_stats_scaled_trans)
View(sub_stats_scaled_trans)

#radial plots
radial.plot(as.matrix(sub_stats_scaled[,2:ncol(sub_stats_scaled)]),
            main = "Metrics By Department",
            labels = c("avg days", "med days", "avg dur", "med dur", "cons", "num", "overdue", "max", "min", "qos"), 
            rp.type = "p",
            lwd = 2,
            grid.bg = gray(.9),
            radial.labels = NA)



faces(sub_stats[,2:ncol(sub_stats)], labels = sub_stats$SUBJECT, face.type = 2)
stars(sub_stats[,2:ncol(sub_stats)], labels = as.character(sub_stats$SUBJECT), key.loc = c(14, 1.5)) # add key
