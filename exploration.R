# Exploration of 311 Service Request Data

# Load Packages
library(ggplot2)
library(dplyr)
library(reshape)
library(tidyr)
library(ggvis)
library(grid)
library(gridExtra)

# load clean data
req = readRDS("cleanData.RDS")

# investigate data with a few graphs
ggplot(req, aes(factor(OnTime_Status))) + geom_bar()
ggplot(req, aes(factor(OnTime_Status), fill=SUBJECT)) + geom_bar()
ggplot(req, aes(factor(neighborhood), fill=SUBJECT)) + geom_bar()
ggplot(req, aes(factor(SUBJECT), fill=OnTime_Status)) + geom_bar()
ggplot(req, aes(factor(SUBJECT), fill=OnTime_Status)) + geom_bar(position="dodge")


# requests by year, month, week, day
plot(table(year(req$OPEN_DT)))
plot(table(month(req$OPEN_DT)))
plot(table(week(req$OPEN_DT)))
plot(table(wday(req$OPEN_DT)))

# heatmap of requests by month and year
hmapTable = as.matrix(table(month(req$OPEN_DT), year(req$OPEN_DT)))
melted = melt(hmapTable)
str(melted)
hmap = ggplot(melted, aes(x = Var.1, y = Var.2)) + geom_tile(aes(fill = value)) + scale_fill_gradient(low="white", high="darkblue") + xlab("") + ylab("")
hmap
ggsave("Plots/heatmap_num_requests1.png")
hmap2 = ggplot(melted, aes(x = Var.2, y = Var.1)) + geom_tile(aes(fill = value)) + scale_fill_gradient(low="white", high="darkblue") + xlab("") + ylab("")
hmap2
ggsave("Plots/heatmap_num_requests2.png")

# basic plots of requests by day, month, year
barplot(table(wday(req$OPEN_DT)))
barplot(table(month(req$OPEN_DT)))
barplot(table(year(req$OPEN_DT)))
barplot(table(year(req$TARGET_DT)))

# requests by neighborhood and subject (dept)
table(req$neighborhood, req$OnTime_Status)
table(req$SUBJECT, req$OnTime_Status)

# create "onTimeNeighborhood" data frame showing OnTime_Status by neighborhood
onTimeNeighborhood = as.data.frame.matrix(table(req$neighborhood, req$OnTime_Status))
onTimeNeighborhood$prop_onTime = onTimeNeighborhood$ONTIME / (onTimeNeighborhood$ONTIME + onTimeNeighborhood$OVERDUE)
onTimeNeighborhood$prop_overdue = onTimeNeighborhood$OVERDUE / (onTimeNeighborhood$ONTIME + onTimeNeighborhood$OVERDUE)
onTimeNeighborhood
onTimeNeighborhood[order(onTimeNeighborhood$prop_onTime),]
rownames(onTimeNeighborhood)
onTimeNeighborhood$neighborhood = rownames(onTimeNeighborhood)
onTimeNeighborhood
rownames(onTimeNeighborhood) = 1:nrow(onTimeNeighborhood)
onTimeNeighborhood
onTimeNeighborhood$TOTAL = onTimeNeighborhood$ONTIME + onTimeNeighborhood$OVERDUE
onTimeNeighborhood
onTimeNeighborhood = onTimeNeighborhood[c("neighborhood", "TOTAL", "ONTIME", "OVERDUE", "prop_onTime", "prop_overdue")]
onTimeNeighborhood %>% arrange(prop_onTime)

# create onTimeSubj data frame showing OnTime_Status by SUBJECT (Dept) - faster way of doing the same thing as above!
onTimeDept = spread(as.data.frame(table(req$SUBJECT, req$OnTime_Status)), Var2, Freq)
onTimeDept = onTimeDept %>% mutate(prop_onTime = ONTIME / (ONTIME + OVERDUE), prop_overdue = OVERDUE / (ONTIME + OVERDUE), TOTAL = ONTIME + OVERDUE)


table(req$neighborhood, req$SUBJECT)
req %>% select(neighborhood) %>% group_by(neighborhood) %>% summarize(n = n()) # note that 31161 requests do not specify a neighborhood

nhbd_sub_count_overdue = req %>% 
  filter(OnTime_Status == "OVERDUE") %>% 
  select(neighborhood, SUBJECT) %>% 
  group_by(neighborhood, SUBJECT) %>% 
  summarize(n = n())
spread(nhbd_sub_count_overdue, SUBJECT, n)

status_by_nbhd_and_dept = req %>% select(neighborhood, SUBJECT, OnTime_Status) %>% 
  group_by(neighborhood, SUBJECT, OnTime_Status)  %>% 
  summarize(n = n())  %>% 
  mutate(freq = n/sum(n))

# data frame of counts by subject for each day/month/year combination
count_by_subject_ts = req %>% select(Source, OPEN_DT, day, week, month, year, OnTime_Status, SUBJECT, neighborhood) %>%
  group_by(day, month, year, SUBJECT) %>%
  summarize(n = n())

# data frame of mean requests by subject by month
mean_by_month = count_by_subject_ts %>% 
  group_by(month, SUBJECT) %>% 
  summarize(mean = mean(n))

mean_by_month_overdue = req %>% 
  select(Source, OPEN_DT, day, week, month, year, OnTime_Status, SUBJECT, neighborhood) %>%
  group_by(month, SUBJECT, OnTime_Status) %>%
  summarize(n = n()) %>% 
  filter(OnTime_Status=="OVERDUE") %>% 
  select(month, SUBJECT, n)

# data frame of proportion of total requests each month
combined_prop_by_month = req %>% select(month) %>% group_by(month) %>% tally %>% mutate(prop = n/sum(n))
combined_onTime_prop_by_month = req %>% select(month, OnTime_Status) %>% group_by(month, OnTime_Status) %>% tally %>% group_by(month) %>% mutate(prop = n/sum(n)) %>% filter(OnTime_Status=="ONTIME")
combined_overdue_prop_by_month = req %>% select(month, OnTime_Status) %>% group_by(month, OnTime_Status) %>% tally %>% group_by(month) %>% mutate(prop = n/sum(n)) %>% filter(OnTime_Status=="OVERDUE")
# data frame of proportion of total requests by subject each month
prop_by_sub_by_month = req %>% select(SUBJECT, month) %>% group_by(SUBJECT, month) %>% tally %>% group_by(SUBJECT) %>% mutate(prop = n/sum(n))

# data frame of proportion of total requests by subject each month on-time
prop_by_sub_by_month_ontime = req %>% 
  select(SUBJECT, month, OnTime_Status) %>% 
  group_by(SUBJECT, month, OnTime_Status) %>% 
  tally %>% 
  group_by(SUBJECT) %>% 
  mutate(prop = n/sum(n))%>% 
  filter(OnTime_Status == "ONTIME") %>% 
  select(SUBJECT, month, n, prop)

# data frame of proportion of total requests by subject each month overdue
prop_by_sub_by_month_overdue = req %>% 
  select(SUBJECT, month, OnTime_Status) %>% 
  group_by(SUBJECT, month, OnTime_Status) %>% 
  tally %>% 
  group_by(SUBJECT) %>% 
  mutate(prop = n/sum(n))%>% 
  filter(OnTime_Status == "OVERDUE") %>% 
  select(SUBJECT, month, n, prop)

prop_by_sub_by_month_status = req %>% 
  select(SUBJECT, month, OnTime_Status) %>% 
  group_by(SUBJECT, month, OnTime_Status) %>% 
  tally %>% 
  group_by(SUBJECT) %>% 
  mutate(prop = n/sum(n))

# Time Series Plots
mean_by_month %>% 
  ggvis(~month, ~mean) %>% 
  layer_points(fill=~SUBJECT) %>%   #remove this to take away the points
  group_by(SUBJECT) %>% 
  layer_lines(stroke=~SUBJECT) %>% 
  add_axis("x", title="Month") %>% 
  add_axis("y", title="Mean Requests By Month")


# time series ggplots
p1 = ggplot(combined_prop_by_month, aes(month, prop)) + geom_line(size=1, color="blue") + scale_x_continuous(breaks=0:13) + theme(plot.title = element_text(size=18)) + labs(title="Proportion All Requests by Month for All Departments Combined", x="Month",y="Proportion of All Requests")
p2 = ggplot(combined_onTime_prop_by_month, aes(month, prop)) + geom_line(size=1, color="blue") + scale_x_continuous(breaks=0:13) + theme(plot.title = element_text(size=18)) + labs(title="Proportion of On Time Requests by Month for All Departments Combined", x="Month",y="Proportion of All Requests")
p3 = ggplot(combined_overdue_prop_by_month, aes(month, prop)) + geom_line(size=1, color="blue") + scale_x_continuous(breaks=0:13) + theme(plot.title = element_text(size=18)) + labs(title="Proportion of Overdue Time Requests by Month for All Departments Combined", x="Month",y="Proportion of All Requests")
grid.arrange(p1, p2, p3, nrow = 3)

p4 = ggplot(mean_by_month, aes(month, mean, color=SUBJECT)) + geom_line(size=.75) + scale_x_continuous(breaks=0:13) + theme(plot.title = element_text(size=18)) + labs(title="Number of Requests by Month", x="Month",y="Number of Requests")
p5 = ggplot(prop_by_sub_by_month, aes(month, prop, color=SUBJECT)) + geom_line(size=.75) + scale_x_continuous(breaks=0:13) + theme(plot.title = element_text(size=18)) + labs(title="Proportion of Requests by Month", x="Month",y="Proportion of Requests")
ggplot(prop_by_sub_by_month_ontime, aes(month, prop, color=SUBJECT)) + geom_line() + ggtitle("Proportion On-Time")
grid.arrange(p4, p5, nrow = 2)

p6 = ggplot(prop_by_sub_by_month_overdue, aes(month, prop, color=SUBJECT)) + geom_line(size=.75) + scale_x_continuous(breaks=0:13) + theme(plot.title = element_text(size=18)) + labs(title="Proportion of Overdue Requests by Month", x="Month",y="Proportion of Requests")
p7 = ggplot(mean_by_month_overdue, aes(month, n, color=SUBJECT)) + geom_line(size=.75) + scale_x_continuous(breaks=0:13) + theme(plot.title = element_text(size=18)) + labs(title="Number of Overdue Requests by Month", x="Month",y="Number of Requests")
grid.arrange(p7, p6, nrow = 2)

prop_by_sub_by_month_status %>% filter(OnTime_Status == "OVERDUE") %>% ggvis(~month, ~prop) %>% layer_points(fill=~SUBJECT) %>% group_by(SUBJECT) %>% layer_lines(stroke=~SUBJECT)
ggplot(mean_by_month, aes(month, mean, color=SUBJECT)) + geom_line(size=1) + facet_grid(. ~ SUBJECT) + guides(color=FALSE) + guides(size=FALSE) + labs(x="Month", y="Average Number of Requests By Month") + theme(axis.ticks = element_blank(), axis.text.x = element_blank())


# Filter TYPE column to look for requests involving Snow.  There were lots of storm-related requests in 2015.
snow = req %>% filter(grepl("Snow", TYPE))
View(snow)

