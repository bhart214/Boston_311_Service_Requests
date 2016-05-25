# Load Data and add variables for time between open date and closed date, open date and target date, and target date and closed date.
# Also add columns for day, week, month, and year.

#load packages
library(lubridate)
library(ggplot2)
library(dplyr)
library(reshape)
library(tidyr)

setwd("~/R/rData/311_Service_Requests/311_SR_Data")

# read csv file and make a copy; change all blank cells to NA
sr = read.csv("311_Service_Requests.csv", header=T, na.strings=c("","NA"))
srCopy = sr

str(sr)


# format as tbl_df so dplyr can be utilized
sr = tbl_df(sr)

# create selection of specific variables
req = sr  %>% select(OPEN_DT, TARGET_DT, CLOSED_DT, OnTime_Status, CLOSURE_REASON, CASE_TITLE, SUBJECT, REASON, TYPE, QUEUE, Department, neighborhood, LOCATION_ZIPCODE, Source)

# format dates
req$OPEN_DT = mdy_hms(req$OPEN_DT)
req$TARGET_DT = mdy_hms(req$TARGET_DT)
req$CLOSED_DT = mdy_hms(req$CLOSED_DT)

# add columns for time differences between OPEN_DT and TARGET_DT and between TARGET_DT and CLOSED_DT
req = req %>% mutate(target_minus_closed = as.numeric(difftime(TARGET_DT,CLOSED_DT)))
req = req %>% mutate(target_minus_open = as.numeric(difftime(TARGET_DT,OPEN_DT)))
req = req %>% mutate(closed_minus_open = as.numeric(difftime(CLOSED_DT,OPEN_DT)))

# add columns for day, week, month, year
req = req %>% 
  mutate(day = wday(req$OPEN_DT), week = week(req$OPEN_DT), month = month(req$OPEN_DT), year = year(req$OPEN_DT))
req = req %>% mutate(days_past_target = -(trunc(target_minus_closed/86400) + 1))

# add QoS Ratio Metric
req = req %>% mutate(qosRatio = (closed_minus_open/target_minus_open))

View(req)
saveRDS(req, "serviceRequests.RDS")