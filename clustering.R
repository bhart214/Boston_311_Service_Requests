# Unsupervised Learning

#*****CLUSTERING*****

# load clean data
req = readRDS("cleanData.RDS")

# avg days past target and avg duration open to closed
clustDF1 = req %>% 
  select(SUBJECT, REASON, TYPE, closed_minus_open, closed_minus_open, days_past_target)  %>% 
  group_by(SUBJECT, REASON, TYPE) %>% 
  summarize(avg_days_past_target = mean(days_past_target, na.rm=TRUE), avg_dur = mean(closed_minus_open, na.rm=TRUE), num = n())
View(clustDF1)

# proportion of overdue requests by TYPE
clustDF2 = req %>% 
  select(SUBJECT, REASON, TYPE, OnTime_Status) %>% 
  group_by(TYPE, OnTime_Status) %>% 
  tally %>% group_by(TYPE) %>% 
  mutate(prop_overdue = n/sum(n)) %>% 
  filter(OnTime_Status=="OVERDUE") %>% 
  select(prop_overdue)
View(clustDF2)

# min and max counts by type per month
clustDF3 = req %>% 
  group_by(TYPE, month) %>% 
  summarize(n=n()) %>% 
  group_by(TYPE) %>% 
  summarize(max = max(n), min = min(n))
View(clustDF3)

# combine clustDF1, clustDF2, clustDF3 into one data frame
clusterDF = full_join(full_join(clustDF1, clustDF2, by = "TYPE"), clustDF3, by = "TYPE")
clusterDF[is.na(clusterDF)] = 0
View(clusterDF)

# Hierarchical Clustering
library(caret)
preproc = preProcess(clusterDF[,4:9])
clusterDFNorm = predict(preproc, clusterDF[,4:9])

HierDistances = dist(clusterDFNorm, method = "euclidean")
HierClustering = hclust(HierDistances, method = "ward.D")

par(cex=.65,font=3, mar=c(5, 8, 4, 1))
plot(HierClustering, hang=-1, main="Dendrogram Department Clusters", label=clusterDF$TYPE)
dev.off()

library(ape)
plot((HierClustering), cex = 0.75, label=clusterDF$TYPE, label.offset = 1, main="Dendrogram Department Clusters")

rect.hclust(HierClustering, k = 10, border = "red")

HierClusterGroups = cutree(HierClustering, k = 10)
HierClusterGroups

spl = split(clusterDF[,4:9], HierClusterGroups)
ClusterGroupDF = as.data.frame(lapply(spl, colMeans))
ClusterGroupDF


# K-Means clustering
set.seed(123)
kmc = kmeans(clusterDFNorm, centers = 10)
str(kmc)

kmc$cluster
kmc$centers
kmc$withinss
kmc$size

# K-Means Heatmaps with Hierarchical Dendograms
library(gplots)
library(car)
library(RColorBrewer)
heatmap.2(kmc$centers,col=brewer.pal(11,"RdBu"), scale = "row", trace="none")
heatmap.2(kmc$centers, scale = "row")

library(useful)
plot(kmc, data = clusterDF[,4:9])

kmcBest = FitKMeans(clusterDF[,4:9], max.clusters = 20, nstart = 25, seed = 123)
kmcBest
PlotHartigan(kmcBest)

# This shows the best number of clusters is 10
library(cluster)
gap = clusGap(clusterDF[,4:9], FUNcluster = pam, K.max = 20)
library(NbClust)
nc = NbClust(clusterDF[,4:9], min.nc=2, max.nc=20, method="kmeans")

# Withing Group Sum of Squares Plot
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}
wssplot(clusterDF[,4:9])

# Silhouette Plot
library(cluster)
library(HSAUR)
dissE <- daisy(clusterDF[,4:9])
dE2   <- dissE^2
sk2   <- silhouette(kmc$cl, dE2)
plot(sk2)


complete = req[complete.cases(req),]
complete$ontime = ifelse(complete$OnTime_Status == "ONTIME", 1, 0)
