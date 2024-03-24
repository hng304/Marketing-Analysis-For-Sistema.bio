setwd("C:/Past Education/SantaClara/MKTG2505/team_project")
library("tidyverse")
library("readxl")
library("janitor")
library("dendextend")
library("factoextra")
library("ggplot2")
library("caret")
library("Metrics")
library("zoo")
library("tidyr")

# Set the locale to English
Sys.setlocale("LC_TIME", "C")


# data prepare
Engage_2022 = read_excel('Kenya Mexico campaigns/2022_Mx_campaigns-Engagement.xlsx')[-1,]
Perform_2022 = read_excel('Kenya Mexico campaigns/2022_MX_Campaigns-Performance.xlsx')[-1,]
Engage_2023 = read_excel('Kenya Mexico campaigns/2023_Mx_campaigns-Engagement.xlsx')[-1,]
Perform_2023 = read_excel('Kenya Mexico campaigns/2023_MX_Campaigns-Performance.xlsx')[-1,]

# Identify common column names
common_cols_2022 = intersect(names(Engage_2022), names(Perform_2022))
# Combine the data frames, excluding columns with the same names
data_2022 = cbind(Engage_2022, Perform_2022[, !names(Perform_2022) %in% common_cols_2022])
data_2022["year"] = 2022
# Specify columns to omit
columns_to_omit <- c("Reporting starts", "Reporting ends", "Starts", "Ends")
# Omit the specified columns
data_2022 = data_2022[, -which(names(data_2022) %in% columns_to_omit)]

# Identify common column names
common_cols_2023 = intersect(names(Engage_2023), names(Perform_2023))
# Combine the data frames, excluding columns with the same names
data_2023 = cbind(Engage_2023, Perform_2023[, !names(Perform_2023) %in% common_cols_2023])
data_2023["year"] = 2023
# Omit the specified columns
data_2023 = data_2023[, -which(names(data_2023) %in% columns_to_omit)]

mxn = rbind(data_2022,data_2023)
mxn = clean_names(mxn)


# EDA
# check missing value and type of the data
colSums(is.na(mxn))
str(mxn)
summary(mxn)

table(mxn$campaign_name)
table(mxn$ad_name)

# For campaign_name freq
ggplot(mxn, aes(x = campaign_name)) +
  geom_bar(fill = "blue", color = "black") +
  labs(title = "Frequency of Campaign Names",
       x = "Campaign Name",
       y = "Frequency")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

mean(table(mxn$campaign_name))
# For ad_name freq
ggplot(mxn, aes(x = ad_name)) +
  geom_bar(fill = "red", color = "black") +
  labs(title = "Frequency of Ad Names",
       x = "Ad Name",
       y = "Frequency")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  


# Calculate the count of unique campaigns for each ad_name
campaign_counts <- mxn %>%
  group_by(ad_name) %>%
  summarise(unique_campaigns = n_distinct(campaign_name))

# Create the bar plot
ggplot(campaign_counts, aes(x = ad_name, y = unique_campaigns)) +
  geom_bar(stat = "identity") +
  labs(x = "Ad Name", y = "Count of Unique Campaigns", title = "Count of Unique Campaigns Grouped by Ad Name") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plotting reach by ad_name
ggplot(mxn, aes(x = ad_name, y = reach)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Reach by Ad Name",
       x = "Ad Name",
       y = "Reach")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plotting impression by ad_name
ggplot(mxn, aes(x = ad_name, y = impressions)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Impression by Ad Name",
       x = "Ad Name",
       y = "Impressions")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

# Plotting cpc_all by ad_name
ggplot(mxn, aes(x = ad_name, y = cpc_all)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Cost per link click all by Ad Name",
       x = "Ad Name",
       y = "Cost per link click all")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

# Plotting fellow of like by ad_name
ggplot(mxn, aes(x = ad_name, y = follows_or_likes )) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Follows_or_likes by Ad Name",
       x = "Ad Name",
       y = "Follows or likes")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

# Plotting post_reactions of like by ad_name
ggplot(mxn, aes(x = ad_name, y = post_reactions )) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Post reactions by Ad Name",
       x = "Ad Name",
       y = "Post reactions")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  
# Plotting post_comments by ad_name
ggplot(mxn, aes(x = ad_name, y = post_comments )) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Post comments by Ad Name",
       x = "Ad Name",
       y = "Post comments")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  
# Plotting post_shares by ad_name
ggplot(mxn, aes(x = ad_name, y = post_shares )) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Post shares by Ad Name",
       x = "Ad Name",
       y = "Post shares")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  
# Plotting link_clicks by ad_name
ggplot(mxn, aes(x = ad_name, y = link_clicks )) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Link clicks by Ad Name",
       x = "Ad Name",
       y = "Link clicks")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  


# missing value
colSums(is.na(mxn))

# Replace NA values with the mean
mxn$post_reactions[is.na(mxn$post_reactions)] <- mean(mxn$post_reactions, na.rm = TRUE)
mxn$post_comments[is.na(mxn$post_comments)] <- mean(mxn$post_comments, na.rm = TRUE)
mxn$link_clicks[is.na(mxn$link_clicks)] <- mean(mxn$link_clicks, na.rm = TRUE)
mxn$post_shares[is.na(mxn$post_shares)] <- mean(mxn$post_shares, na.rm = TRUE)
mxn$cpc_cost_per_link_click[is.na(mxn$cpc_cost_per_link_click)] <- mean(mxn$cpc_cost_per_link_click, na.rm = TRUE)
mxn$follows_or_likes[is.na(mxn$follows_or_likes)] <- 0


ad_mxn = mxn%>%
  group_by(ad_name)%>%
  summarise(
    freq_ad = n(),
    avg_post_reactions = mean(post_reactions),
    avg_post_comments = mean(post_comments),
    avg_post_shares = mean(post_shares),
    tot_link_clicks = sum(link_clicks),
    tot_follows_or_likes = mean(follows_or_likes),
    avg_cpc_cost_per_link_click = mean(cpc_cost_per_link_click),
    avg_reach = mean(reach),
    avg_impressions = mean(impressions),
    avg_clicks_all = mean(clicks_all),
    avg_cpc_all = mean(cpc_all),
    avg_amount_spent = mean(amount_spent),
    year_2023 = sum(year == 2023),
    year_2022 = sum(year == 2022),
    inactive = sum(ad_delivery=="inactive")
  )

str(ad_mxn)
# hist of y
hist(ad_mxn$avg_reach)

par(mfrow = c(4, 4))
for (i in 2:16) {
  # Extract the column as a vector and convert it to numeric
  column_values <- as.numeric(ad_mxn[[i]])
  
  # Plot histogram
  hist(column_values, xlab = colnames(ad_mxn)[i], main = paste("Distribution Plots for", colnames(ad_mxn)[i]))
}
par(mfrow = c(4, 4))
for (i in 2:16) {
  # Extract the column as a vector and convert it to numeric
  column_values <- as.numeric(ad_mxn[[i]])
  
  # Plot histogram
  hist(log(column_values), xlab = colnames(ad_mxn)[i], main = paste("Distribution Plots for", colnames(ad_mxn)[i]))
}
par(mfrow = c(1, 1))

# transform the non-zero to 1
ad_mxn$inactive = ifelse(ad_mxn$inactive != 0, 1, ad_mxn$inactive)
# Create a dummy variable 
ad_mxn$inactive  <- as.factor(ad_mxn$inactive )

# transform the numerical col with log and scale
ad_mxn[, !names(ad_mxn) %in% c("ad_name", "inactive")] = log(ad_mxn[, !names(ad_mxn) %in% c("ad_name", "inactive")]+1e-6)



# stepwise
intercept<-lm(avg_reach~1-ad_name,ad_mxn)
all<-lm(avg_reach~.-ad_name ,data =ad_mxn)
stepboth<-step(intercept,direction="both",scope=formula(all))
stepboth$anova

# split the train and test
sample_i<-sample.int(nrow(ad_mxn), replace=FALSE, 
                     size=0.80*nrow(ad_mxn))
trainset<-ad_mxn[sample_i,]
testset<-ad_mxn[-sample_i,]

# nfold validation
set.seed(926)
trainfold<-trainControl(method="cv", number=10, savePredictions = TRUE)

modelfold<-train(avg_reach~ tot_follows_or_likes+ avg_post_comments+ inactive
                 + avg_clicks_all+ freq_ad+ avg_post_shares+ avg_cpc_all
                 + avg_post_reactions + year_2022 + avg_amount_spent
                 + avg_cpc_cost_per_link_click
                 , 
                 data= trainset, method="lm",trControl=trainfold)
summary(modelfold)


# view final model
final_model = modelfold$finalModel
modelfold$pred

# view prediction for each fold
modelfold$resample

# plot the residual 
plot(final_model)
plot(final_model$residuals)

# normal proablility plot
qqnorm(final_model$residuals)
qqline(final_model$residuals)


# obtain predicted values in trainset and the error
pred<-predict(modelfold, trainset)
mae(trainset$avg_reach, pred)
mape(trainset$avg_reach, pred)


# obtain predicted values in testset and the error
pred<-predict(modelfold, testset)
mae(testset$avg_reach, pred)
mape(testset$avg_reach, pred)
# hist of y
hist(ad_mxn$avg_reach)
