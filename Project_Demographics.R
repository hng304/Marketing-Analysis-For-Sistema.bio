setwd("/Users/Jbags/OneDrive/Documents/Marketing Analytics/Project")
library(readxl)
library(ggplot2)
library(tidyverse)
library(openxlsx)
library(dplyr)
library(janitor)
library(dendextend)
library(factoextra)
library(tidyr)

#Read data
data2022 <- "./Kenya Mexico campaigns/2022_MX_Campaigns-by-Demographics.xlsx"
data2023 <- "./Kenya Mexico campaigns/2023_MX_Campaigns-by-Demographics.xlsx"

#Remove first row for each file
data2022 <-read_excel(data2022)[-1,]
data2023 <-read_excel(data2023)[-1,]

#Merge seperate years and clean
mydata <- bind_rows(data2022, data2023)
mydata<-clean_names(mydata)

#Summary
colSums(is.na(mydata))
str(mydata)
summary(mydata)

#Remove and replace NAs
mydata <- subset(mydata, select = -c(ad_delivery, starts, ends))
mydata[is.na(mydata)] <- 0

#EDA
cname_averages<-mydata %>% select(campaign_name,reach,impressions,clicks_all)%>%
  group_by(campaign_name) %>% 
  summarise(mean_reach=mean(reach),mean_impressions=mean(impressions),mean_clicks=mean(clicks_all))

age_averages<-mydata %>% select(age,reach,impressions,clicks_all)%>%
  group_by(age) %>% 
  summarise(count=n(),mean_reach=mean(reach),mean_impressions=mean(impressions),mean_clicks=mean(clicks_all))

gender_averages<-mydata %>% select(gender,reach,impressions,clicks_all)%>%
  group_by(gender) %>% 
  summarise(count=n(),mean_reach=mean(reach),mean_impressions=mean(impressions),mean_clicks=mean(clicks_all))

#Reshape the data into long format
cname_long <- pivot_longer(cname_averages, cols = -campaign_name, names_to = "Group", values_to = "Value")
age_long <- pivot_longer(age_averages, cols = -age, names_to = "Group", values_to = "Value")
gender_long <- pivot_longer(gender_averages, cols = -gender, names_to = "Group", values_to = "Value")

#Plots
ggplot(cname_long, aes(x = reorder(campaign_name, -Value), y = Value, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Campaign Metrics", x = "Campaigns", y = "Mean Clicks/Impressions/Reach") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(age_long, aes(x = reorder(age, -Value), y = Value, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Metrics by Age", x = "Age", y = "Mean Clicks/Impressions/Reach") +
  theme_minimal()

ggplot(gender_long, aes(x = reorder(gender, -Value), y = Value, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Metrics by Gender", x = "Gender", y = "Mean Clicks/Impressions/Reach") +
  theme_minimal()

#Remove "Unknown" gender
mydata <- mydata %>% filter(gender != "unknown")

#Convert categorical columns
mydata$gender[mydata$gender == "male"] <- 1
mydata$gender[mydata$gender == "female"] <- 0
mydata$gender <- as.numeric(mydata$gender)

mydata$age[mydata$age == "13-17"] <- median(13:17)
mydata$age[mydata$age == "18-24"] <- median(18:24)
mydata$age[mydata$age == "25-34"] <- median(25:34)
mydata$age[mydata$age == "35-44"] <- median(35:44)
mydata$age[mydata$age == "45-54"] <- median(45:54)
mydata$age[mydata$age == "55-64"] <- median(55:64)
mydata$age[mydata$age == "65+"] <- median(65:74)
mydata$age <- as.numeric(mydata$age)

#Data for cluster Analysis
mydata2 = mydata[c("reach","impressions",'clicks_all')]


#Hierachical cluster
distance = dist(mydata2)

#Using silhouette
fviz_nbclust(mydata2,FUN=hcut,method="silhouette")
fviz_nbclust(mydata2,FUN=hcut,metho='wss')

#Create 5 clusters 
mydata2_hcluster <-hclust(distance)
hier_cluster_5 <- cutree(mydata2_hcluster, 5)
mydata$hier_cluster_5 <- hier_cluster_5
plot(mydata2_hcluster, hang=-1, labels = mydata$age)
plot(mydata2_hcluster, hang=-1, labels = mydata$gender)

#Plot dendrogram for 5 clusters
dend_5<-as.dendrogram(mydata2_hcluster)
labels(dend_5)<-mydata$age
dend_5<-color_labels(dend_5,k=5)
dend_5<-color_branches(dend_5,k=5)
plot(dend_5)

#Characterize the clusters
member_5=cutree(mydata2_hcluster,5)
table(member_5)
hist(mydata$hier_cluster_5)

#Compute means by cluster
hcluster_means_5 <- aggregate(cbind(reach,impressions,clicks_all,age,gender) ~ hier_cluster_5, data = mydata, FUN = mean)
print(hcluster_means_5)

#Create 6 clusters
hier_cluster_6 <- cutree(mydata2_hcluster, 6)
mydata$hier_cluster_6 <- hier_cluster_6

#Plot dendrogram for 6 clusters
dend_6 <- as.dendrogram(mydata2_hcluster)
labels(dend_6) <- mydata$age
dend_6 <- color_labels(dend_6, k = 6)
dend_6 <- color_branches(dend_6, k = 6)
plot(dend_6)

#Characterize the clusters for 6 clusters
#Use cutree to separate the tree
member_6 = cutree(mydata2_hcluster, 6)
table(member_6)
hist(mydata$hier_cluster_6)

#Compute means by cluster
hcluster_means <- aggregate(cbind(reach,impressions,clicks_all,age,gender) ~ hier_cluster_6, data = mydata, FUN = mean)
print(hcluster_means)


#Kmeans Cluster

#Optimal number of cluster
fviz_nbclust(mydata2,kmeans,metho='wss')
fviz_nbclust(mydata2,kmeans,metho='silhouette')

#Number of clusters
k <- 4
set.seed(123)
m_kmeans <-kmeans(mydata2,k,nstart=20)
mydata$kmean_cluster = m_kmeans$cluster

hist(mydata$kmean_cluster, main = "k = 4", xlab = "Cluster", ylab = "Frequency", col='lightblue')

#Compute means by cluster
kcluster_means <- aggregate(cbind(reach,impressions,clicks_all,age,gender) ~ kmean_cluster, data = mydata, FUN = mean)
kcluster_means <- round(kcluster_means, 2)
kcluster_means <- kcluster_means[order(-kcluster_means$reach), ]
print(kcluster_means)

k <- 5
set.seed(123)
m_kmeans <-kmeans(mydata2,k,nstart=20)
mydata$kmean_cluster = m_kmeans$cluster

hist(mydata$kmean_cluster, main = "k = 5", xlab = "Cluster", ylab = "Frequency", col='lightblue')

#Compute means by cluster
kcluster_means <- aggregate(cbind(reach,impressions,clicks_all,age,gender) ~ kmean_cluster, data = mydata, FUN = mean)
kcluster_means <- round(kcluster_means, 2)
kcluster_means <- kcluster_means[order(-kcluster_means$reach), ]
print(kcluster_means)

#Look at the center of each of the variables
m_kmeans$centers
class(m_kmeans$centers)
data_center<-as.data.frame(m_kmeans$centers)
data_center$cluster <- c(1:k)
data_center


#Anovas
for(i in 1:3){
  print(colnames(data_center[i]))
  print(summary(aov(data_center[,i]~cluster, data_center)))
  print("---------------------------------------------------")
}``

#Explained variance ratio
m_kmeans$betweenss/m_kmeans$totss

#2-dimensional Model
fviz_cluster(m_kmeans,mydata2)

#Reshape data to long format
data_center_long <- tidyr::gather(data_center, key = "feature", value = "value", -cluster)

#Create plot
ggplot(data_center_long, aes(x = feature, y = value, color = factor(cluster), group = cluster)) +
  geom_line(size = 1.5) +
  geom_point(size = 2.5) +
  labs(title = "Line Plots by Cluster", x = "Feature", y = "Center Value") +
  scale_color_manual(values = c("1" = "red", "2" = "blue", "3" = "black", "4" = "purple")) +
  theme_minimal()
