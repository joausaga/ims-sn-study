#Loading depedent libraries
library(ggplot2)
library(reshape)
library(relaimpo)
library(knitr)
library(dplyr)

# Set the working directory to the dir where this script is located
setwd('')

#Loading external source
source('./r-scripts/utils.R')

#Loading communities dataset
communities = read.csv("./datasets/civic-participation_communities.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)
communities_more_info = read.csv("./datasets/civic_communities_more_info.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)

#
#-------------------- CLEANING DATA SET ---------------------------------
#
# Replacing NULL-ish values of some fields with NA and then converting them to numeric
communities$ideas_in_review[communities$ideas_in_review=="NULL"] = NA
communities$ideas_in_review = as.numeric(communities$ideas_in_review)
communities$ideas_in_progress[communities$ideas_in_progress=="NULL"] = NA
communities$ideas_in_progress = as.numeric(communities$ideas_in_progress)
communities$ideas_implemented[communities$ideas_implemented=="NULL"] = NA
communities$ideas_implemented = as.numeric(communities$ideas_implemented)
communities$facebook[communities$facebook=="NULL"] = NA
communities$facebook = as.numeric(communities$facebook)
communities$twitter[communities$twitter=="NULL"] = NA
communities$twitter = as.numeric(communities$twitter)
communities$age[communities$age=="NULL"] = NA
communities$age = as.numeric(communities$age)
communities$moderators[communities$moderators=="NULL"] = NA
communities$moderators = as.numeric(communities$moderators)

# Getting rid of communities that were incorrectly included
communities = filter(communities,! id %in% c(190, 203, 306, 327))

# Getting rid of communities not having enabled social sharing buttons 
com_sn_not_na = communities[!is.na(communities$twitter),]

# Calculating Facebook quartiles
summary_f = summary(com_sn_not_na$facebook)
first_q = summary_f[2]
third_q_f = summary_f[5]
iqr = third_q_f-first_q
inner_fence_up = third_q_f + (iqr*1.5)
inner_fence_down = first_q - (iqr*1.5)
outer_fence_up_f = third_q_f + (iqr*3)
outer_fence_down = first_q - (iqr*3)
outliers_f = com_sn_not_na[com_sn_not_na$facebook>outer_fence_up_f,]
paste("Communities eliminated because their numbers of facebook shares are above the outer fence: ", outliers_f$id)

# Getting rid of facebook outliers
comm_no_sn_outliers = com_sn_not_na[com_sn_not_na$facebook<=outer_fence_up_f,]

# Calculating Twitter quartiles
summary_t = summary(com_sn_not_na$twitter)
first_q = summary_t[2]
third_q = summary_t[5]
iqr = third_q-first_q
inner_fence_up = third_q + (iqr*1.5)
inner_fence_down = first_q - (iqr*1.5)
outer_fence_up_t = third_q + (iqr*3)
outer_fence_down = first_q - (iqr*3)
outliers_t = comm_no_sn_outliers[comm_no_sn_outliers$twitter>outer_fence_up_t,]
paste("Communities eliminated because their numbers of twitter shares are above the outer fence: ", outliers_t$id)

# Getting rid of twitter outliers
comm_no_sn_outliers = comm_no_sn_outliers[comm_no_sn_outliers$twitter<=outer_fence_up_t,]

# Calculating Members quartiles
summary_t = summary(com_sn_not_na$members)
first_q = summary_t[2]
third_q = summary_t[5]
iqr = third_q-first_q
inner_fence_up = third_q + (iqr*1.5)
inner_fence_down = first_q - (iqr*1.5)
outer_fence_up_m = third_q + (iqr*3)
outer_fence_down = first_q - (iqr*3)
outliers_m = comm_no_sn_outliers[comm_no_sn_outliers$members>outer_fence_up_m,]
cat("Communities eliminated because their numbers of members are above the outer fence: ", outliers_m$id)

# Getting rid of members outliers
comm_no_sn_outliers = comm_no_sn_outliers[comm_no_sn_outliers$members<=outer_fence_up_m,]

# Calculating Ideas quartiles
summary_t = summary(com_sn_not_na$ideas)
first_q = summary_t[2]
third_q = summary_t[5]
iqr = third_q-first_q
inner_fence_up = third_q + (iqr*1.5)
inner_fence_down = first_q - (iqr*1.5)
outer_fence_up_i = third_q + (iqr*3)
outer_fence_down = first_q - (iqr*3)
outliers_i = comm_no_sn_outliers[comm_no_sn_outliers$ideas>outer_fence_up_i,]
cat("Communities eliminated because their numbers of ideas are above the outer fence: ", outliers_i$id)

# Getting rid of ideas outliers
comm_no_sn_outliers = comm_no_sn_outliers[comm_no_sn_outliers$ideas<=outer_fence_up_i,]

# Calculating votes quartiles
summary_t = summary(com_sn_not_na$votes)
first_q = summary_t[2]
third_q = summary_t[5]
iqr = third_q-first_q
inner_fence_up = third_q + (iqr*1.5)
inner_fence_down = first_q - (iqr*1.5)
outer_fence_up_v = third_q + (iqr*3)
outer_fence_down = first_q - (iqr*3)
outliers_v = comm_no_sn_outliers[comm_no_sn_outliers$votes>outer_fence_up_v,]
cat("Communities eliminated because their numbers of votes are above the outer fence: ", outliers_v$id)

# Getting rid of votes outliers
comm_no_sn_outliers = comm_no_sn_outliers[comm_no_sn_outliers$votes<=outer_fence_up_v,]

# Calculating comments quartiles
summary_t = summary(com_sn_not_na$comments)
first_q = summary_t[2]
third_q = summary_t[5]
iqr = third_q-first_q
inner_fence_up = third_q + (iqr*1.5)
inner_fence_down = first_q - (iqr*1.5)
outer_fence_up_c = third_q + (iqr*3)
outer_fence_down = first_q - (iqr*3)
outliers_c = comm_no_sn_outliers[comm_no_sn_outliers$comments>outer_fence_up_c,]
cat("Communities eliminated because their numbers of votes are above the outer fence: ", outliers_c$id)

# Getting rid of comments outliers
comm_no_sn_outliers = comm_no_sn_outliers[comm_no_sn_outliers$comments<=outer_fence_up_c,]

# Checking that the final dataset has 53 rows
nrow(comm_no_sn_outliers)

# Printing out general statistics
cat("Total Members: ", sum(comm_no_sn_outliers$members))
cat("Total Ideas: ", sum(comm_no_sn_outliers$ideas))
cat("Total Ideas implemented: ", sum(comm_no_sn_outliers[!is.na(comm_no_sn_outliers$ideas_implemented),"ideas_implemented"]))
cat("Total Ideas in progress: ", sum(comm_no_sn_outliers[!is.na(comm_no_sn_outliers$ideas_in_progress),"ideas_in_progress"]))
cat("Total Votes: ", sum(comm_no_sn_outliers$votes))
cat("Total Comments: ", sum(comm_no_sn_outliers$comments))
cat("Total Shares: ", sum(comm_no_sn_outliers[!is.na(comm_no_sn_outliers$facebook),"facebook"]))
cat("Total Tweets: ", sum(comm_no_sn_outliers[!is.na(comm_no_sn_outliers$twitter),"twitter"]))

# Printing out state of communities
cat("Active communities: ", nrow(comm_no_sn_outliers[comm_no_sn_outliers$status=="active",]))
cat("Inactive communities: ", nrow(comm_no_sn_outliers[comm_no_sn_outliers$status=="inactive",]))
cat("Close communities: ", nrow(comm_no_sn_outliers[comm_no_sn_outliers$status=="closed",]))

# Getting rid of row names
rownames(comm_no_sn_outliers) = NULL

# Getting rid of don't needed columns
communities_more_info = select(communities_more_info, id, group)

# Merging datasets
comm_no_sn_outliers = merge(comm_no_sn_outliers,communities_more_info,by.x='id',by.y='id')

# Creating the groups
com_group_a = filter(comm_no_sn_outliers, group=='A')
com_group_b = filter(comm_no_sn_outliers, group=='B')

# Printing out group A general statistics
cat("Total Members: ", sum(com_group_a$members))
cat("Total Ideas: ", sum(com_group_a$ideas))
cat("Total Ideas implemented: ", sum(com_group_a[!is.na(com_group_a$ideas_implemented),"ideas_implemented"]))
cat("Total Ideas in progress: ", sum(com_group_a[!is.na(com_group_a$ideas_in_progress),"ideas_in_progress"]))
cat("Total Votes: ", sum(com_group_a$votes))
cat("Total Comments: ", sum(com_group_a$comments))
cat("Total Shares: ", sum(com_group_a[!is.na(com_group_a$facebook),"facebook"]))
cat("Total Tweets: ", sum(com_group_a[!is.na(com_group_a$twitter),"twitter"]))


#------------- PLOTTING RELATIONSHIP ------------------
#
# Facebook and Twitter Shares ~ Members
# Preparing Data
aux_comm_no_sn_outliers = comm_no_sn_outliers[,c("id","members","facebook","twitter")]
colnames(aux_comm_no_sn_outliers)[3] = "Shares Count"
colnames(aux_comm_no_sn_outliers)[4] = "Tweets Count"
# Melting the data
mem_shares_melt = melt(aux_comm_no_sn_outliers, id.vars = c("id","members"))
ggplot(mem_shares_melt, aes(x=value, y=members)) + 
  geom_point(shape=1) + 
  facet_grid(. ~ variable, scales = "free_x") +
  geom_smooth(color = 'black', method="loess", linetype = "solid", size = 1) + 
  labs(x="", y="Members Count") +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=20), strip.text.x = element_text(size=20)) 

# Each one separately
ggplot(comm_no_sn_outliers, aes(facebook, members)) + geom_point(shape=1) + labs(x="Shares Count", y="Members Count") + 
  geom_smooth(color = 'black', method="loess", linetype = "solid", size = 1) + 
  scale_x_continuous(breaks=c(0,25,50,75,100,125,150)) +
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20))

ggplot(comm_no_sn_outliers, aes(twitter, members)) + geom_point(shape=1) + labs(x="Tweets Count", y="Members Count") + 
  geom_smooth(color = 'black', method="loess", linetype = "solid", size = 1) + 
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20))


# Preparing Data
aux_comm_no_sn_outliers = comm_no_sn_outliers[,c("id","members","ideas","votes","comments","facebook","twitter")]
aux_comm_no_sn_outliers["ideas"] = aux_comm_no_sn_outliers["ideas"]/aux_comm_no_sn_outliers["members"]
aux_comm_no_sn_outliers["votes"] = aux_comm_no_sn_outliers["votes"]/aux_comm_no_sn_outliers["members"]
aux_comm_no_sn_outliers["comments"] = aux_comm_no_sn_outliers["comments"]/aux_comm_no_sn_outliers["members"]
aux_comm_no_sn_outliers["facebook"] = aux_comm_no_sn_outliers["facebook"]/aux_comm_no_sn_outliers["members"]
aux_comm_no_sn_outliers["twitter"] = aux_comm_no_sn_outliers["twitter"]/aux_comm_no_sn_outliers["members"]
colnames(aux_comm_no_sn_outliers)[6] = "Shares/Members"
colnames(aux_comm_no_sn_outliers)[7] = "Tweets/Members"
aux_comm_no_sn_outliers = subset(aux_comm_no_sn_outliers, select=-c(members))
# Melting the data
shares_melt = melt(aux_comm_no_sn_outliers, id.vars = c("id","ideas","votes","comments"))
# Creating data
sh_ideas = ggplot(shares_melt, aes(x=value, y=ideas))
sh_votes = ggplot(shares_melt, aes(x=value, y=votes))
sh_comments = ggplot(shares_melt, aes(x=value, y=comments))

sh_ideas + geom_point(shape=1) + facet_grid(. ~ variable, scales = "free_x") + 
  geom_smooth(color = 'black', method="loess", linetype = "solid", size = 1, se=T) + 
  labs(x="", y="Ideas/Members") +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=20), strip.text.x = element_text(size=20)) 

sh_votes + geom_point(shape=1) + facet_grid(. ~ variable, scales = "free_x") + 
  geom_smooth(color = 'black', method="loess", linetype = "solid", size = 1, se=T) + 
  labs(x="", y="Votes/Members") +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=20), strip.text.x = element_text(size=20)) 

sh_comments + geom_point(shape=1) + facet_grid(. ~ variable, scales = "free_x") + 
  geom_smooth(color = 'black', method="loess", linetype = "solid", size = 1, se=T) + 
  labs(x="", y="Comments/Members") +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=20), strip.text.x = element_text(size=20)) 

# Facebook Shares ~ Ideas
ggplot(comm_no_sn_outliers, aes(facebook/members,ideas/members)) + geom_point(shape=1) + labs(x="shares/members", y="ideas/members") + 
  geom_smooth(color = 'black', method="loess", linetype = "solid", size = 1) + 
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20))
# Facebook Shares ~ Votes
ggplot(comm_no_sn_outliers, aes(facebook/members,votes/members)) + geom_point(shape=1) + labs(x="shares/members", y="votes/members") + 
  geom_smooth(color = 'black', method="loess", linetype = "solid", size = 1) + 
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20))
# Facebook Shares ~ Comments
ggplot(comm_no_sn_outliers, aes(facebook/members,comments/members)) + geom_point(shape=1) + labs(x="shares/members", y="comments/members") + 
  geom_smooth(color = 'black', method="loess", linetype = "solid", size = 1) + 
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20))
# Twitter Shares ~ Ideas
ggplot(comm_no_sn_outliers, aes(twitter/members, ideas/members)) + geom_point(shape=1) + labs(x="tweets/members", y="ideas/members") + 
  geom_smooth(color = 'black', method="loess", linetype = "solid", size = 1) + 
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20))
# Twitter Shares ~ Votes
ggplot(comm_no_sn_outliers, aes(twitter/members, votes/members)) + geom_point(shape=1) + labs(x="tweets/members", y="votes/members") + 
  geom_smooth(color = 'black', method="loess", linetype = "solid", size = 1) + 
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20))
# Twitter Shares ~ Comments
ggplot(comm_no_sn_outliers, aes(twitter/members, comments/members)) + geom_point(shape=1) + labs(x="tweets/members", y="comments/members") + 
  geom_smooth(color = 'black', method="loess", linetype = "solid", size = 1) + 
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20))

nrow(comm_no_sn_outliers[comm_no_sn_outliers$twitter/comm_no_sn_outliers$members==0,])

#
#------------- CALCULATING ABSOLUTE CORRELATIONS FOR THE 53 --------------------
#
cor(comm_no_sn_outliers$members,comm_no_sn_outliers$facebook)
cor(comm_no_sn_outliers$members,comm_no_sn_outliers$twitter)
cor.test(comm_no_sn_outliers$ideas,comm_no_sn_outliers$facebook)
cor.test(comm_no_sn_outliers$ideas,comm_no_sn_outliers$twitter)
cor.test(comm_no_sn_outliers$votes,comm_no_sn_outliers$facebook)
cor.test(comm_no_sn_outliers$votes,comm_no_sn_outliers$twitter)
cor.test(comm_no_sn_outliers$comments,comm_no_sn_outliers$facebook)
cor.test(comm_no_sn_outliers$comments,comm_no_sn_outliers$twitter)

cor.test(comm_no_sn_outliers$members,comm_no_sn_outliers$ideas)
cor.test(comm_no_sn_outliers$members,comm_no_sn_outliers$votes)
cor.test(comm_no_sn_outliers$members,comm_no_sn_outliers$comments)

#
#------------- CALCULATING ABSOLUTE CORRELATIONS FOR THE 53 BUT BY GROUP --------------------
#
cor.test(com_group_a$members,com_group_a$facebook)
cor.test(com_group_a$members,com_group_a$twitter)
cor.test(com_group_b$members,com_group_b$facebook)
cor.test(com_group_b$members,com_group_b$twitter)

#
#------------- CALCULATING RELATIVE CORRELATIONS FOR THE 53 --------------------
#
# Ideas/Members vs. Shares-Tweets/Members
cor.test(comm_no_sn_outliers$ideas/comm_no_sn_outliers$members,comm_no_sn_outliers$facebook/comm_no_sn_outliers$members)
cor.test(comm_no_sn_outliers$ideas/comm_no_sn_outliers$members,comm_no_sn_outliers$twitter/comm_no_sn_outliers$members)
# Votes/Members vs. Shares-Tweets/Members
cor.test(comm_no_sn_outliers$votes/comm_no_sn_outliers$members,comm_no_sn_outliers$facebook/comm_no_sn_outliers$members)
cor.test(comm_no_sn_outliers$votes/comm_no_sn_outliers$members,comm_no_sn_outliers$twitter/comm_no_sn_outliers$members)
# Comments/Members vs. Shares-Tweets/Members
cor.test(comm_no_sn_outliers$comments/comm_no_sn_outliers$members,comm_no_sn_outliers$facebook/comm_no_sn_outliers$members)
cor.test(comm_no_sn_outliers$comments/comm_no_sn_outliers$members,comm_no_sn_outliers$twitter/comm_no_sn_outliers$members)

#
#------------- CALCULATING RELATIVE CORRELATIONS FOR GROUP A --------------------
#
# Ideas/Members vs. Shares-Tweets/Members
cor.test(com_group_a$ideas/com_group_a$members,com_group_a$facebook/com_group_a$members)
cor.test(com_group_a$ideas/com_group_a$members,com_group_a$twitter/com_group_a$members)
# Votes/Members vs. Shares-Tweets/Members
cor.test(com_group_a$votes/com_group_a$members,com_group_a$facebook/com_group_a$members)
cor.test(com_group_a$votes/com_group_a$members,com_group_a$twitter/com_group_a$members)
# Comments/Members vs. Shares-Tweets/Members
cor.test(com_group_a$comments/com_group_a$members,com_group_a$facebook/com_group_a$members)
cor.test(com_group_a$comments/com_group_a$members,com_group_a$twitter/com_group_a$members)

#
#------------- CALCULATING RELATIVE CORRELATIONS FOR GROUP B --------------------
#
# Ideas/Members vs. Shares-Tweets/Members
cor.test(com_group_b$ideas/com_group_b$members,com_group_b$facebook/com_group_b$members)
cor.test(com_group_b$ideas/com_group_b$members,com_group_b$twitter/com_group_b$members)
# Votes/Members vs. Shares-Tweets/Members
cor.test(com_group_b$votes/com_group_b$members,com_group_b$facebook/com_group_b$members)
cor.test(com_group_b$votes/com_group_b$members,com_group_b$twitter/com_group_b$members)
# Comments/Members vs. Shares-Tweets/Members
cor.test(com_group_b$comments/com_group_b$members,com_group_b$facebook/com_group_b$members)
cor.test(com_group_b$comments/com_group_b$members,com_group_b$twitter/com_group_b$members)

# Plot the high and significant correlation
ggplot(com_group_b, aes(facebook/members,ideas/members)) + geom_point(shape=1) + labs(x="shares/members", y="ideas/members") + 
  geom_smooth(color = 'black', method="loess", linetype = "solid", size = 1) + 
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20))

# Leaving aside the initiatives of the group that have facebook page linked to ideascale
com_group_b_1 = filter(com_group_b, !id %in% c(221, 292, 186))
cor.test(com_group_b_1$ideas/com_group_b_1$members,com_group_b_1$facebook/com_group_b_1$members)
ggplot(com_group_b_1, aes(facebook/members,ideas/members)) + geom_point(shape=1) + labs(x="shares/members", y="ideas/members") + 
  geom_smooth(color = 'black', method="loess", linetype = "solid", size = 1) + 
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20))

#
#-------------- LONGITUDINAL ANALYSIS -------------------------
#
totals_obs = getTotalsObs(FALSE)

# Data Preparation
aux_total_obs = totals_obs
colnames(aux_total_obs)[2] = "Shares Count"
colnames(aux_total_obs)[3] = "Tweets Count"
colnames(aux_total_obs)[4] = "Members Count"
total_obs_melted = melt(aux_total_obs, id.vars = "obs", measure.vars = c("Members Count", "Shares Count", "Tweets Count"))  # Melting the data
# Plotting total variables for all observations
ggplot(total_obs_melted, aes(obs, value)) + 
  geom_line(size=1) +
  facet_grid(variable ~ ., scales = "free_y") +
  scale_x_continuous(breaks=c(1:14)) +
  labs(x="Weeks", y="") +
  theme(axis.text=element_text(size=22), axis.title=element_text(size=22), strip.text.y = element_text(size=22)) 

# Calculating Correlation Tests
total_obs = subset(totals_obs, (totals_obs[ ,"obs"] != 1 & totals_obs[ ,"obs"] != 2))
# Facebook
round(cor.test(totals_obs$members,totals_obs$facebook)$estimate,3)
round(cor.test(totals_obs$ideas,totals_obs$facebook)$estimate,3)
round(cor.test(totals_obs$votes,totals_obs$facebook)$estimate,3)
round(cor.test(totals_obs$comments,totals_obs$facebook)$estimate,3)
# Twitter
round(cor.test(totals_obs$members,totals_obs$twitter)$estimate,3)
round(cor.test(totals_obs$ideas,totals_obs$twitter)$estimate,3)
round(cor.test(totals_obs$votes,totals_obs$twitter)$estimate,3)
round(cor.test(totals_obs$comments,totals_obs$twitter)$estimate,3)

#
#-------------- MULTIPLE REGRESSION ANALYSIS FOR ALL COMMUNITIES -------------------------
#

# Getting data
data_totals = getTotalsObs(FALSE)

# Removing outliers (observation 1 and 2)
# data_totals = subset(data_totals, (data_totals[ ,"obs"] != 1 & data_totals[ ,"obs"] != 2))

# Facebook/Members + Age ~ Members
linear_fb_age_members = lm(members~facebook+age, data=data_totals)
summary(linear_fb_age_members)
summary(linear_fb_age_members)$coefficients
rel_impor_table = calRelImporTable(data_totals$facebook,"facebook",data_totals$age,"age",data_totals$members,data_totals)
kable(rel_impor_table,align=c('c','c','c','c','c','c'))

# Twitter/Members + Age ~ Members
linear_tw_age_members = lm(members~twitter+age, data=data_totals)
summary(linear_tw_age_members)
rel_impor_table = calRelImporTable(data_totals$twitter,"twitter",data_totals$age,"age",data_totals$members,data_totals)
kable(rel_impor_table,align=c('c','c','c','c','c','c'))

# Facebook/Members + Age ~ Ideas
linear_fb_age_ideas = lm(ideas~facebook+age, data=data_totals)
summary(linear_fb_age_ideas)
rel_impor_table = calRelImporTable(data_totals$facebook,"facebook",data_totals$age,"age",data_totals$ideas,data_totals)
kable(rel_impor_table,align=c('c','c','c','c','c','c'))

# Facebook/Members + Age ~ Votes
#linear_fb_age_votes = lm(scale(votes)~scale(facebook)+scale(age), data=data_totals)
#summary(linear_fb_age_votes)
rel_impor_table = calRelImporTable(data_totals$facebook,"facebook",data_totals$age,"age",data_totals$votes,data_totals)
kable(rel_impor_table,align=c('c','c','c','c','c','c'))

# Facebook/Members + Age ~ Comments
#linear_fb_age_comments = lm(comments~facebook+age, data=data_totals)
#summary(linear_fb_age_comments)
rel_impor_table = calRelImporTable(data_totals$facebook,"facebook",data_totals$age,"age",data_totals$comments,data_totals)
kable(rel_impor_table,align=c('c','c','c','c','c','c'))

# Twitter/Members + Age ~ Members
#linear_tw_age_members = lm(members~twitter+age, data=data_totals)
#summary(linear_tw_age_members)
rel_impor_table = calRelImporTable(data_totals$twitter,"twitter",data_totals$age,"age",data_totals$members,data_totals)
kable(rel_impor_table,align=c('c','c','c','c','c','c'))

# Twitter/Members + Age ~ Ideas
#linear_tw_age_ideas = lm(ideas~twitter+age, data=data_totals)
#summary(linear_tw_age_ideas)
rel_impor_table = calRelImporTable(data_totals$twitter,"twitter",data_totals$age,"age",data_totals$ideas,data_totals)
kable(rel_impor_table,align=c('c','c','c','c','c','c'))

# Twitter/Members + Age ~ Votes
#linear_tw_age_votes = lm(votes~twitter+age, data=data_totals)
#summary(linear_tw_age_votes)
rel_impor_table = calRelImporTable(data_totals$twitter,"twitter",data_totals$age,"age",data_totals$votes,data_totals)
kable(rel_impor_table,align=c('c','c','c','c','c','c'))

# Twitter/Members + Age ~ Comments
#linear_tw_age_comments = lm(comments~twitter+age, data=data_totals)
#summary(linear_tw_age_comments)
rel_impor_table = calRelImporTable(data_totals$twitter,"twitter",data_totals$age,"age",data_totals$comments,data_totals)
kable(rel_impor_table,align=c('c','c','c','c','c','c'))

#
#-------------- MULTIPLE REGRESSION ANALYSIS PER COMMUNITY -------------------------
#
calRelImporTables()

#
#-------------- MULTIPLE REGRESSION ANALYSIS BUT ONLY INITIATIVES WITH SOCIAL ACTIVITY -------------------------
#
id_social_initiatives = c()
for (id in unique(civic_communities_obs$communityid)) {
  community = filter(civic_communities_obs, communityid==id)
  has_social_activity = FALSE
  if (length(unique(community$facebook)) > 1) {
    cat(paste('Community ', id, ' has facebook activity\n',sep=''))
    has_social_activity = T
  }
  if (length(unique(community$twitter)) > 1) {
    cat(paste('Community ', id, ' has twitter activity\n',sep=''))
    has_social_activity = T
  }
  if (!has_social_activity) {
    cat(paste('Community ', id, ' does not have social activity\n',sep=''))
  } else {
    id_social_initiatives = c(id_social_initiatives, id)
  }
}

social_communities = filter(civic_communities_obs,communityid %in% id_social_initiatives)

# Getting data
data_totals = getTotals(social_communities)

# Data Preparation for plotting
aux_total_obs = data_totals
colnames(aux_total_obs)[2] = "Shares Count"
colnames(aux_total_obs)[3] = "Tweets Count"
colnames(aux_total_obs)[4] = "Members Count"
total_obs_melted = melt(aux_total_obs, id.vars = "obs", measure.vars = c("Members Count", "Shares Count", "Tweets Count"))  # Melting the data
# Plotting total variables for all observations
ggplot(total_obs_melted, aes(obs, value)) + 
  geom_line(size=1) +
  facet_grid(variable ~ ., scales = "free_y") +
  scale_x_continuous(breaks=c(1:14)) +
  labs(x="Weeks", y="") +
  theme(axis.text=element_text(size=22), axis.title=element_text(size=22), strip.text.y = element_text(size=22)) 

# Facebook/Members + Age ~ Members
linear_fb_age_members = lm(members~facebook+age, data=data_totals)
summary(linear_fb_age_members)
summary(linear_fb_age_members)$coefficients
rel_impor_table = calRelImporTable(data_totals$facebook,"facebook",data_totals$age,"age",data_totals$members,data_totals)
kable(rel_impor_table,align=c('c','c','c','c','c','c'))

# Twitter/Members + Age ~ Members
linear_tw_age_members = lm(members~twitter+age, data=data_totals)
summary(linear_tw_age_members)
rel_impor_table = calRelImporTable(data_totals$twitter,"twitter",data_totals$age,"age",data_totals$members,data_totals)
kable(rel_impor_table,align=c('c','c','c','c','c','c'))


#
#-------------- MULTIPLE REGRESSION ANALYSIS BUT ONLY INITIATIVES OF GROUP A -------------------------
#
group_a_communities = filter(civic_communities_obs,communityid %in% com_group_a$id)
# Getting data
data_totals = getTotals(group_a_communities)
# Facebook/Members + Age ~ Members
linear_fb_age_members = lm(members~facebook+age, data=data_totals)
summary(linear_fb_age_members)
summary(linear_fb_age_members)$coefficients
rel_impor_table = calRelImporTable(data_totals$facebook,"facebook",data_totals$age,"age",data_totals$members,data_totals)
kable(rel_impor_table,align=c('c','c','c','c','c','c'))
# Twitter/Members + Age ~ Members
linear_tw_age_members = lm(members~twitter+age, data=data_totals)
summary(linear_tw_age_members)
rel_impor_table = calRelImporTable(data_totals$twitter,"twitter",data_totals$age,"age",data_totals$members,data_totals)
kable(rel_impor_table,align=c('c','c','c','c','c','c'))

#
#-------------- MULTIPLE REGRESSION ANALYSIS BUT ONLY INITIATIVES OF GROUP B -------------------------
#
group_b_communities = filter(civic_communities_obs,communityid %in% com_group_b$id)
# Getting data
data_totals = getTotals(group_b_communities)
# Facebook/Members + Age ~ Members
linear_fb_age_members = lm(members~facebook+age, data=data_totals)
summary(linear_fb_age_members)
summary(linear_fb_age_members)$coefficients
rel_impor_table = calRelImporTable(data_totals$facebook,"facebook",data_totals$age,"age",data_totals$members,data_totals)
kable(rel_impor_table,align=c('c','c','c','c','c','c'))
# Twitter/Members + Age ~ Members
linear_tw_age_members = lm(members~twitter+age, data=data_totals)
summary(linear_tw_age_members)
rel_impor_table = calRelImporTable(data_totals$twitter,"twitter",data_totals$age,"age",data_totals$members,data_totals)
kable(rel_impor_table,align=c('c','c','c','c','c','c'))

#
#-------------- QUALITATIVE ANALYSIS OF TWEETS ------------------------
#
tweets = read.csv("./datasets/tweets_2.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)

# Filtering out tweets of communities wrongly included
tweets = filter(tweets, ! community_id %in% c(190, 203, 306, 327))

# Preparing data
tweets = subset(tweets,select=-c(id))
tweets$source[tweets$source!="Twitter for Websites"] = "2. Other Twitter Clients"
tweets$source[tweets$source=="Twitter for Websites"] = "1. Tweet Button"

# Drawing a box plot to compare the reactions raised by the tweet button agains 
# the reactions raised by the other means
ggplot(filter(tweets,total < 40), aes(x=source, y=total, fill=source)) + 
  geom_boxplot() + 
  scale_y_continuous(breaks=c(0,5,10,15,20,25,30)) +
  theme(axis.text.x = element_text(size=17), 
        axis.title.x = element_text(size=20),
        axis.text.y = element_text(size=17), 
        axis.title.y = element_text(size=20)) + 
  labs(y="Reactions", x='Tweet Source') +
  guides(fill=FALSE)

# Totals
nrow(tweets[tweets$source=="1. Tweet Button",])
nrow(tweets[tweets$source!="1. Tweet Button",])

# Calculating Means
tweets_a = tweets[tweets$total<10,]  # Removing outliers
mean_tw_button = round(mean(tweets_a[tweets_a$source=="1. Tweet Button","total"]),3)
mean_other = round(mean(tweets_a[tweets_a$source!="1. Tweet Button","total"]),3)
tweets["mean"] = ifelse(tweets$source=="Tweet Button",mean_tw_button,mean_other)

# T-Test
t.test(tweets_a[tweets_a$source!="1. Tweet Button","total"], tweets_a[tweets_a$source=="1. Tweet Button","total"])

# Creating annotation dataframe
annot_means = data.frame(x=c(3.8,1.5),y=c(20,20),source=unique(tweets$source),
                         labs=c(paste("Avg. Reactions: ",mean_other,sep=""),paste("Avg. Reactions: ",mean_tw_button,sep="")))

# Plotting and saving into a variable
tweets_by_source = ggplot(tweets, aes(x=total, group=source)) + 
  geom_histogram(binwidth=0.5) +
  geom_vline(data=tweets, aes(xintercept=mean, group=source), linetype = 'dashed', color = 'red', 
             size = 1, labels="Mean", show_guide=T) + 
  scale_x_continuous(breaks=c(0:8)) + 
  facet_grid(. ~ source, scales = "free") +
  labs(x="Total Reactions (RT, RP, FAV)", y="Tweets Count") + 
  theme(axis.text=element_text(size=24), axis.title=element_text(size=24), strip.text.x = element_text(size=24))

# Adding annotations to the plot
tweets_by_source + geom_text(data=annot_means, aes(x,y,label=labs,group=NULL), size=9)

# Crowd vs Moderators (organizers)
tweets_mod = filter(tweets, author_mod==1)
tweets_crowd = filter(tweets, author_mod==0)
cat(paste("Tweets by moderators: ",nrow(tweets_mod), " (",round(nrow(tweets_mod)/nrow(tweets)*100),"%)",sep=""))
cat(paste("Tweets by the participants: ",nrow(tweets_crowd), " (",round(nrow(tweets_crowd)/nrow(tweets)*100),"%)",sep=""))

# Reactions of crowd's tweets agains moderators' tweets
round(mean(tweets_mod[tweets_mod$total<=10,"total"]),3)
round(mean(tweets_crowd[tweets_crowd$total<=10,"total"]),3)

# Effectiveness of tweets buttons in moderators' tweets
round(mean(tweets_mod[tweets_mod$total<=10&tweets_mod$source=="1. Tweet Button","total"]),3)
round(mean(tweets_mod[tweets_mod$total<=10&tweets_mod$source!="1. Tweet Button","total"]),3)

# Effectiveness of tweets buttons in crowd's tweets
round(mean(tweets_crowd[tweets_crowd$total<=10&tweets_crowd$source=="1. Tweet Button","total"]),3)
round(mean(tweets_crowd[tweets_crowd$total<=10&tweets_crowd$source!="1. Tweet Button","total"]),3)

