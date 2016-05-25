# Read in data from RDS file created in load.R
req = readRDS("serviceRequests.RDS")

# Clean data: investigate missingness, check variables for legitimacy
# Missing Values
# NA's
colSums(is.na(req))

# number of requests by department and subject; these should add up, but they don't
sort(table(req$Department))
sort(table(req$SUBJECT))

# how many records are missing target dates?
nrow(req  %>% filter(is.na(TARGET_DT) == TRUE))

# which departments are missing target dates?
sort(table(req$SUBJECT[is.na(req$TARGET_DT)]))

# how many records are missing closed dates?
nrow(req  %>% filter(is.na(CLOSED_DT) == TRUE))

# which departments are missing closed dates?
sort(table(req$SUBJECT[is.na(req$CLOSED_DT)]))

# how many records are missing closed dates that are not listed as overdue?
nrow(req  %>% filter(is.na(CLOSED_DT)))
nrow(req  %>% filter(is.na(CLOSED_DT) == TRUE & OnTime_Status == "OVERDUE"))
nrow(req  %>% filter(is.na(CLOSED_DT) == TRUE & OnTime_Status == "ONTIME"))

# check for complete cases
table(complete.cases(req))

# check to see earliest dates and latest dates
summary(req$OPEN_DT)
summary(req$TARGET_DT) # notice that TARGET_DT has a max date of "2062-01-01 12:00:00"
summary(req$CLOSED_DT)

# create new data frame of weirdDates (target dates after Jan 1 2016)
weirdDate = subset(req, TARGET_DT >= "2016-01-01 12:00:00")
colSums(is.na(weirdDate))

# remove weirdDates from req dataframe
req = subset(req, TARGET_DT <= "2016-01-01 12:00:00")


# Compare proportion of requests by SUBJECT for req and weirdDate
sort(table(req$SUBJECT)/nrow(req))
sort(table(weirdDate$SUBJECT)/nrow(weirdDate))

View(weirdDate) # notice that all weirdDates are listed as "ONTIME"

saveRDS(req, "cleanData.RDS")



### Compare different Subsets of the cleaned data ###
reqProp = req %>% 
  select(SUBJECT, neighborhood, OnTime_Status) %>% 
  group_by(SUBJECT, neighborhood, OnTime_Status) %>% 
  summarize(n=n()) %>% mutate(freqAll = n/sum(n))

# Data Frames of Subsets of req:
# 1. complete cases
# 2. incomplete cases
# 3. weird dates (target date > Jan 1 2017)
# 4. complete cases without weird dates
# 5. short duration (less than 30 sec)
# 6. complete cases without short duration
# 7. complete cases without weird dates or short duration

# make new data frame containing only complete records for each request
completeCases = req %>% filter(complete.cases(.))
completeCasesProp = completeCases %>% 
  select(SUBJECT, neighborhood, OnTime_Status) %>% 
  group_by(SUBJECT, neighborhood, OnTime_Status) %>% 
  summarize(n=n()) %>% mutate(freqCompCases = n/sum(n))  %>% 
  select(SUBJECT, neighborhood, OnTime_Status, freqCompCases)

# data frame containing only incomplete cases
incompleteCases = req %>% filter(!complete.cases(.))
incompleteCasesProp = incompleteCases %>% 
  select(SUBJECT, neighborhood, OnTime_Status) %>% 
  group_by(SUBJECT, neighborhood, OnTime_Status) %>% 
  summarize(n=n()) %>% mutate(freqIncompCases = n/sum(n))  %>% 
  select(SUBJECT, neighborhood, OnTime_Status, freqIncompCases)

# make new data frame containing target dates on or after Jan 1, 2017
weirdDates = req %>% filter(TARGET_DT > "2017-01-01 00:00:00")
weirdDatesProp = weirdDates %>% 
  select(SUBJECT, neighborhood, OnTime_Status) %>% 
  group_by(SUBJECT, neighborhood, OnTime_Status) %>% 
  summarize(n=n()) %>% mutate(freqWeirdDates = n/sum(n))  %>% 
  select(SUBJECT, neighborhood, OnTime_Status, freqWeirdDates)

# make new data frame of complete cases without weird dates
compCasesNoWeirdDates = completeCases %>% filter(TARGET_DT < "2017-01-01 00:00:00")
compCasesNoWeirdDatesProp = compCasesNoWeirdDates %>% 
  select(SUBJECT, neighborhood, OnTime_Status) %>% 
  group_by(SUBJECT, neighborhood, OnTime_Status) %>% 
  summarize(n=n()) %>% mutate(freqCompCasesNoWeirdDates = n/sum(n))  %>% 
  select(SUBJECT, neighborhood, OnTime_Status, freqCompCasesNoWeirdDates)

# make new data frame of short duration requests (duration between open and closed is < 30 sec)
shortDuration = req %>% filter(closed_minus_open < 30)
shortDurationProp = shortDuration %>% 
  select(SUBJECT, neighborhood, OnTime_Status) %>% 
  group_by(SUBJECT, neighborhood, OnTime_Status) %>% 
  summarize(n=n()) %>% mutate(freqShortDuration = n/sum(n))  %>% 
  select(SUBJECT, neighborhood, OnTime_Status, freqShortDuration)

# make new data frame of complete cases without weird dates
shortDurationNoWeirdDates = shortDuration %>% filter(TARGET_DT < "2017-01-01 00:00:00")
shortDurationNoWeirdDatesProp = shortDurationNoWeirdDates %>% 
  select(SUBJECT, neighborhood, OnTime_Status) %>% 
  group_by(SUBJECT, neighborhood, OnTime_Status) %>% 
  summarize(n=n()) %>% mutate(freqShortDurationNoWeirdDates = n/sum(n))  %>% 
  select(SUBJECT, neighborhood, OnTime_Status, freqShortDurationNoWeirdDates)

# make new data frame of complete cases without short duration
compCasesNoShortDuration = completeCases %>% filter(closed_minus_open > 30)
compCasesNoShortDurationProp = compCasesNoShortDuration %>% 
  select(SUBJECT, neighborhood, OnTime_Status) %>% 
  group_by(SUBJECT, neighborhood, OnTime_Status) %>% 
  summarize(n=n()) %>% mutate(freqCompCasesNoShortDuration = n/sum(n))  %>% 
  select(SUBJECT, neighborhood, OnTime_Status, freqCompCasesNoShortDuration)

# make new data frame of complete cases without short duration or weird dates
compCasesNoShortDurationNoWeirdDates = compCasesNoShortDuration %>% filter(TARGET_DT < "2017-01-01 00:00:00")
compCasesNoShortDurationNoWeirdDatesProp = compCasesNoShortDurationNoWeirdDates %>% 
  select(SUBJECT, neighborhood, OnTime_Status) %>% 
  group_by(SUBJECT, neighborhood, OnTime_Status) %>% 
  summarize(n=n()) %>% mutate(freqCompCasesNoShortDurationNoWeirdDates = n/sum(n))  %>% 
  select(SUBJECT, neighborhood, OnTime_Status, freqCompCasesNoShortDurationNoWeirdDates)

combinedProps = reqProp %>% 
  left_join(completeCasesProp, by = c("SUBJECT","neighborhood","OnTime_Status"), copy = FALSE) %>% 
  left_join(incompleteCasesProp, by = c("SUBJECT","neighborhood","OnTime_Status"), copy = FALSE) %>% 
  left_join(weirdDatesProp, by = c("SUBJECT","neighborhood","OnTime_Status"), copy = FALSE) %>% 
  left_join(compCasesNoWeirdDatesProp, by = c("SUBJECT","neighborhood","OnTime_Status"), copy = FALSE) %>% 
  left_join(shortDurationProp, by = c("SUBJECT","neighborhood","OnTime_Status"), copy = FALSE) %>% 
  left_join(shortDurationNoWeirdDatesProp, by = c("SUBJECT","neighborhood","OnTime_Status"), copy = FALSE) %>% 
  left_join(compCasesNoShortDurationProp, by = c("SUBJECT","neighborhood","OnTime_Status"), copy = FALSE) %>% 
  left_join(compCasesNoShortDurationNoWeirdDatesProp, by = c("SUBJECT","neighborhood","OnTime_Status"), copy = FALSE)

colSums(is.na(combinedProps))



# This seems like the most logical data frame to use to me if we want to analyze durations between open, target, and closed dates.
# It cuts out lots of the data (any incomplete cases, any where closed_minus_open < 60 sec, and any where closed_minus_open > 30 days)
df = completeCases %>% filter(closed_minus_open < (30*60*60*24), closed_minus_open > 60)

