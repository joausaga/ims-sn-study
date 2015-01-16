#Loading graphic library
library(ggplot2)
library(reshape)
library(relaimpo)
library(knitr)

#Loading external source
source('~/Dropbox/PhD/Thesis/Studies/IdeaScale/IdeaScale_R/communities_social-networks.R')

#Loading communities dataset
setwd("~/Dropbox/PhD/Thesis/Studies/IdeaScale")
communities = read.csv("dumps/civic-participation_communities2.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)

#
#-------------------- CLEANING DATA SET ---------------------------------
#
# Replacing NULL-ish values of some fields with NA and converting them to numeric
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

# Checking that the final dataset has 58 rows
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



#
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
ggplot(comm_no_sn_outliers, aes(facebook,members)) + geom_point(shape=1) + labs(x="Shares Count", y="Members Count") + 
  geom_smooth(color = 'black', method="loess", linetype = "solid", size = 1) + 
  scale_x_continuous(breaks=c(0,25,50,75,100,125,150)) +
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20))

ggplot(comm_no_sn_outliers, aes(twitter,members)) + geom_point(shape=1) + labs(x="Tweets Count", y="Members Count") + 
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
#------------- CALCULATING ABSOLUTE CORRELATIONS FOR THE 58 --------------------
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
#------------- CALCULATING RELATIVE CORRELATIONS FOR THE 58 --------------------
#
cor.test(comm_no_sn_outliers$ideas/comm_no_sn_outliers$members,comm_no_sn_outliers$facebook/comm_no_sn_outliers$members)
cor.test(comm_no_sn_outliers$ideas/comm_no_sn_outliers$members,comm_no_sn_outliers$twitter/comm_no_sn_outliers$members)
cor.test(comm_no_sn_outliers$votes/comm_no_sn_outliers$members,comm_no_sn_outliers$facebook/comm_no_sn_outliers$members)
cor.test(comm_no_sn_outliers$votes/comm_no_sn_outliers$members,comm_no_sn_outliers$twitter/comm_no_sn_outliers$members)
cor.test(comm_no_sn_outliers$comments/comm_no_sn_outliers$members,comm_no_sn_outliers$facebook/comm_no_sn_outliers$members)
cor.test(comm_no_sn_outliers$comments/comm_no_sn_outliers$members,comm_no_sn_outliers$twitter/comm_no_sn_outliers$members)

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
data_totals = subset(data_totals, (data_totals[ ,"obs"] != 1 & data_totals[ ,"obs"] != 2))

# Facebook/Members + Age ~ Members
linear_fb_age_members = lm(members~facebook+age, data=data_totals)
summary(linear_fb_age_members)
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
#-------------- COMPARISON SOCIAL VS ANTISOCIAL COMMUNITIES ------------------------
#
social_communities = read.csv("dumps/civic_social_communities3.csv",header=TRUE,sep=",")
antisocial_communities = read.csv("dumps/civic_antisocial_active_communities3.csv",header=TRUE,sep=",")

sub_antisocial = antisocial_communities[antisocial_communities$id%in%c(138, 147, 203, 246, 411, 454, 476),]

# Mean and SD of Members
print("Members")
mean(social_communities$members)
sd(social_communities$members)
mean(sub_antisocial$members)
sd(sub_antisocial$members)

# Mean and SD of Ideas
mean(social_communities$ideas)
sd(social_communities$ideas)
mean(sub_antisocial$ideas)
sd(sub_antisocial$ideas)

# Mean and SD of Votes
mean(social_communities$votes)
sd(social_communities$votes)
mean(sub_antisocial$votes)
sd(sub_antisocial$votes)

# Mean and SD of Comments
mean(social_communities$comments)
sd(social_communities$comments)
mean(sub_antisocial$comments)
sd(sub_antisocial$comments)

# Mean and SD of Age
mean(social_communities$age)
mean(sub_antisocial$age)

# Mean and SD of Facebook Shares
mean(social_communities$facebook)
mean(sub_antisocial$facebook)

# Mean and SD of Twitter Shares
mean(social_communities$twitter)
mean(sub_antisocial$twitter)


#
#-------------- QUALITATIVE ANALYSIS OF TWEETS ------------------------
#

tweets = read.csv("dumps/tweets_aggregated3.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)

# Preparing data
tweets = subset(tweets,select=-c(id))
tweets$source[tweets$source!="Twitter for Websites"] = "Other Twitter Clients"
tweets$source[tweets$source=="Twitter for Websites"] = "Tweet Button"

# Totals
nrow(tweets[tweets$source=="Tweet Button",])
nrow(tweets[tweets$source!="Tweet Button",])

# Calculating Means
tweets = tweets[tweets$total<30,]  # Removing outliers
mean_tw_button = round(mean(tweets[tweets$source=="Tweet Button","total"]),3)
mean_other = round(mean(tweets[tweets$source!="Tweet Button","total"]),3)
tweets["mean"] = ifelse(tweets$source=="Tweet Button",mean_tw_button,mean_other)

# T-Test
t.test(tweets[tweets$source!="Tweet Button","total"], tweets[tweets$source=="Tweet Button","total"])

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









