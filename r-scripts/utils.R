getLastObsNumber = function(f) {
  obs = read.csv(f,header=TRUE,sep=",")
  return (obs[nrow(obs),"observation"])
}

updateCommunitiesSNObs = function() {
  setwd("~/Dropbox/PhD/Thesis/Studies/IdeaScale")
  
  #Define constants
  FILE_DIR = 'observations/study2-rq1/communities'
  FILE_CSV = paste(c(FILE_DIR,'/civic_communities_aggregated.csv'),collapse="")
  OBSERVATION = getLastObsNumber(FILE_CSV) + 1
  ROUND_DIGIT = 3
  ORIENTATION = 'Civic Participation'
  
  file_obs = paste(c(FILE_DIR,"/communities_obs_",OBSERVATION,".csv"),collapse="")
  
  if (file.exists(file_obs)) {
    communities = read.csv(file_obs,header=TRUE,sep=",")
    
    if (nrow(communities) > 0) {
      #Get only civic participation oriented communities
      civic_communities = communities[communities$orientation==ORIENTATION,]
      
      #Calculate totals for the current observation
      total_fb = sum(civic_communities$facebook)
      total_tw = sum(civic_communities$twitter)
      total_members = sum(civic_communities$members)
      total_ideas = sum(civic_communities$ideas)
      total_comments = sum(civic_communities$comments)
      total_votes = sum(civic_communities$votes)
      
      #Calculate the correlations of the current observation
      #Facebook - Members
      cor_fb_mem = round(cor(civic_communities$facebook,civic_communities$members,method="spearman"),ROUND_DIGIT)
      #Facebook - Ideas
      cor_fb_ide = round(cor(civic_communities$facebook,civic_communities$ideas,method="spearman"),ROUND_DIGIT)
      #Facebook - Comments
      cor_fb_com = round(cor(civic_communities$facebook,civic_communities$comments,method="spearman"),ROUND_DIGIT)
      #Facebook - Votes
      cor_fb_vot = round(cor(civic_communities$facebook,civic_communities$votes,method="spearman"),ROUND_DIGIT)
      #Facebook - Twitter
      cor_fb_tw = round(cor(civic_communities$facebook,civic_communities$twitter,method="spearman"),ROUND_DIGIT)
      #Twitter - Members
      cor_tw_mem = round(cor(civic_communities$twitter,civic_communities$members,method="spearman"),ROUND_DIGIT)
      #Twitter - Ideas
      cor_tw_ide = round(cor(civic_communities$twitter,civic_communities$ideas,method="spearman"),ROUND_DIGIT)
      #Twitter - Comments
      cor_tw_com = round(cor(civic_communities$twitter,civic_communities$comments,method="spearman"),ROUND_DIGIT)
      #Twitter - Votes
      cor_tw_vot = round(cor(civic_communities$twitter,civic_communities$votes,method="spearman"),ROUND_DIGIT)
      
      #Create or Update a dataframe for containing the totals, correlations and current observed values
      #Table Head
      table_head = cbind('observation','communityid','facebook','twitter','members','ideas','comments','votes')
      #Table Body
      table_body = cbind(OBSERVATION,civic_communities[,"id"],civic_communities[,"facebook"],civic_communities[,"twitter"],
                         civic_communities[,"members"],civic_communities[,"ideas"],civic_communities[,"comments"],civic_communities[,"votes"])
      #Table Tail
      table_totals = c(OBSERVATION,'Totals',total_fb,total_tw,total_members,total_ideas,total_comments,total_votes)
      table_cor_fb = c(OBSERVATION,'Cor FB',as.numeric(''),cor_fb_tw,cor_fb_mem,cor_fb_ide,cor_fb_com,cor_fb_vot)
      table_cor_tw = c(OBSERVATION,'Cor TW',cor_fb_tw,as.numeric(''),cor_tw_mem,cor_tw_ide,cor_tw_com,cor_tw_vot)
      ob_table = rbind(table_body,table_totals,table_cor_fb,table_cor_tw)
      
      #Write the table on file
      if (!file.exists(FILE_CSV)) {
        write.table(ob_table,file=FILE_CSV,append=FALSE,sep=",",col.names=table_head, row.names=FALSE)
      } else {
        write.table(ob_table,file=FILE_CSV,append=TRUE,sep=",",col.names=FALSE, row.names=FALSE)
      }
    }
  }
}

getCorrelationMatrix = function() {
  setwd("~/Dropbox/PhD/Thesis/Studies/IdeaScale")
  civic_communities_obs = read.csv("observations/study2-rq1/communities/civic_communities_aggregated.csv",header=TRUE,sep=",")
  civic_communities_names = read.csv("dumps/civic-participation_communities_names2.csv",header=TRUE,sep=",")
  
  #Set constanst
  COR_THRESHOLD_UP = 0.5
  COR_THRESHOLD_DOWN = -0.5
  
  #=======Matrix of significant correlations in communities==========
  remove(matrix_cor_communities_obs)
  for (id in levels(civic_communities_obs$communityid)) {
    if (id != "Cor FB" & id != "Cor TW" & id != "Totals") {
      community = civic_communities_obs[civic_communities_obs$communityid==id,]
      community_name = as.character(civic_communities_names[civic_communities_names$id==id,"name"])
      #Facebook - Members
      cor_fb_mem = round(cor(community$facebook, community$members,method="spearman"),3)
      if (!is.na(cor_fb_mem)) {
        if (cor_fb_mem >= COR_THRESHOLD_UP | cor_fb_mem <= COR_THRESHOLD_DOWN) {
          if (exists("matrix_cor_communities_obs")) {
            matrix_cor_communities_obs = rbind(matrix_cor_communities_obs,c(id,community_name,'facebook~members',cor_fb_mem))
          } else {
            matrix_cor_communities_obs = matrix(c(id,community_name,'facebook~members',cor_fb_mem),ncol=4,byrow=TRUE)
          }
        }
      }
      #Facebook - Ideas
      cor_fb_ide = round(cor(community$facebook, community$ideas,method="spearman"),3)
      if (!is.na(cor_fb_ide)) {
        if (cor_fb_ide >= COR_THRESHOLD_UP | cor_fb_ide <= COR_THRESHOLD_DOWN) {
          if (exists("matrix_cor_communities_obs")) {
            matrix_cor_communities_obs = rbind(matrix_cor_communities_obs,c(id,community_name,'facebook~ideas',cor_fb_ide))
          } else {
            matrix_cor_communities_obs = matrix(c(id,community_name,'facebook~ideas',cor_fb_ide),ncol=4,byrow=TRUE)
          }
        }
      }
      #Facebook - Comments
      cor_fb_com = round(cor(community$facebook, community$comments,method="spearman"),3)
      if (!is.na(cor_fb_com)) {
        if (cor_fb_com >= COR_THRESHOLD_UP | cor_fb_com <= COR_THRESHOLD_DOWN) {
          if (exists("matrix_cor_communities_obs")) {
            matrix_cor_communities_obs = rbind(matrix_cor_communities_obs,c(id,community_name,'facebook~comments',cor_fb_com))
          } else {
            matrix_cor_communities_obs = matrix(c(id,community_name,'facebook~comments',cor_fb_com),ncol=4,byrow=TRUE)
          }
        }
      }
      #Facebook - Votes
      cor_fb_vot = round(cor(community$facebook, community$votes,method="spearman"),3)
      if (!is.na(cor_fb_vot)) {
        if (cor_fb_vot >= COR_THRESHOLD_UP | cor_fb_vot <= COR_THRESHOLD_DOWN) {
          if (exists("matrix_cor_communities_obs")) {
            matrix_cor_communities_obs = rbind(matrix_cor_communities_obs,c(id,community_name,'facebook~votes',cor_fb_vot))
          } else {
            matrix_cor_communities_obs = matrix(c(id,community_name,'facebook~votes',cor_fb_vot),ncol=4,byrow=TRUE)
          }
        }
      }
      #Twitter - Members
      cor_tw_mem = round(cor(community$twitter, community$members, method="spearman"),3)
      if (!is.na(cor_tw_mem)) {
        if (cor_tw_mem >= COR_THRESHOLD_UP | cor_tw_mem <= COR_THRESHOLD_DOWN) {
          if (exists("matrix_cor_communities_obs")) {
            matrix_cor_communities_obs = rbind(matrix_cor_communities_obs,c(id,community_name,'twitter~members',cor_tw_mem))
          } else {
            matrix_cor_communities_obs = matrix(c(id,community_name,'twitter~members',cor_tw_mem),ncol=4,byrow=TRUE)
          }
        }
      }
      #Twitter - Ideas
      cor_tw_ide = round(cor(community$twitter, community$ideas, method="spearman"),3)
      if (!is.na(cor_tw_ide)) {
        if (cor_tw_ide >= COR_THRESHOLD_UP | cor_tw_ide <= COR_THRESHOLD_DOWN) {
          if (exists("matrix_cor_communities_obs")) {
            matrix_cor_communities_obs = rbind(matrix_cor_communities_obs,c(id,community_name,'twitter~ideas',cor_tw_ide))
          } else {
            matrix_cor_communities_obs = matrix(c(id,community_name,'twitter~ideas',cor_tw_ide),ncol=4,byrow=TRUE)
          }
        }
      }
      #Twitter - Comments
      cor_tw_com = round(cor(community$twitter, community$comments, method="spearman"),3)
      if (!is.na(cor_tw_com)) {
        if (cor_tw_com >= COR_THRESHOLD_UP | cor_tw_com <= COR_THRESHOLD_DOWN) {
          if (exists("matrix_cor_communities_obs")) {
            matrix_cor_communities_obs = rbind(matrix_cor_communities_obs,c(id,community_name,'twitter~comments',cor_tw_com))
          } else {
            matrix_cor_communities_obs = matrix(c(id,community_name,'twitter~comments',cor_tw_com),ncol=4,byrow=TRUE)
          }
        }
      }
      #Twitter - Votes
      cor_tw_vot = round(cor(community$twitter, community$votes, method="spearman"),3)
      if (!is.na(cor_tw_vot)) {
        if (cor_tw_vot >= COR_THRESHOLD_UP | cor_tw_vot <= COR_THRESHOLD_DOWN) {
          if (exists("matrix_cor_communities_obs")) {
            matrix_cor_communities_obs = rbind(matrix_cor_communities_obs,c(id,community_name,'twitter~votes',cor_tw_vot))
          } else {
            matrix_cor_communities_obs = matrix(c(id,community_name,'twitter~votes',cor_tw_vot),ncol=4,byrow=TRUE)
          }
        }
      }
    }
  }
  
  if (exists("matrix_cor_communities_obs")) {
    colnames(matrix_cor_communities_obs) = c('Id','Name','Correlation Type','Correlation Coefficient')
    return (matrix_cor_communities_obs)
  } else {
    return (FALSE)
  } 
}

getDeltas = function() {
  setwd("~/Dropbox/PhD/Thesis/Studies/IdeaScale")
  civic_communities_obs = read.csv("observations/study2-rq1/communities/civic_communities_aggregated.csv",header=TRUE,sep=",")
  
  totals = civic_communities_obs[civic_communities_obs$communityid=="Totals",]
  
  for (num_obs in unique(unlist(totals$observation))) {
    if (num_obs!= 0) {
      if (!exists("mat_deltas")) {
        mat_deltas = matrix(c(num_obs,totals[totals$observation==num_obs,"facebook"]-last_fb_tot,
                              totals[totals$observation==num_obs,"twitter"]-last_tw_tot,
                              totals[totals$observation==num_obs,"members"]-last_me_tot,
                              totals[totals$observation==num_obs,"ideas"]-last_id_tot,
                              totals[totals$observation==num_obs,"comments"]-last_co_tot,
                              totals[totals$observation==num_obs,"votes"]-last_vo_tot),ncol=7,byrow=TRUE)
      } else {
        mat_deltas = rbind(mat_deltas, c(num_obs,totals[totals$observation==num_obs,"facebook"]-last_fb_tot,
                                         totals[totals$observation==num_obs,"twitter"]-last_tw_tot,
                                         totals[totals$observation==num_obs,"members"]-last_me_tot,
                                         totals[totals$observation==num_obs,"ideas"]-last_id_tot,
                                         totals[totals$observation==num_obs,"comments"]-last_co_tot,
                                         totals[totals$observation==num_obs,"votes"]-last_vo_tot))
      }
    }
    last_fb_tot = totals[totals$observation==num_obs,"facebook"]
    last_tw_tot = totals[totals$observation==num_obs,"twitter"]
    last_me_tot = totals[totals$observation==num_obs,"members"]
    last_id_tot = totals[totals$observation==num_obs,"ideas"]
    last_co_tot = totals[totals$observation==num_obs,"comments"]
    last_vo_tot = totals[totals$observation==num_obs,"votes"]
  }
  
  colnames(mat_deltas) = c('obs','facebook','twitter','members','ideas','comments','votes')
  
  data_deltas = data.frame(obs=as.numeric(mat_deltas[,1]),facebook=as.numeric(mat_deltas[,2]),
                          twitter=as.numeric(mat_deltas[,3]),members=as.numeric(mat_deltas[,4]),
                          ideas=as.numeric(mat_deltas[,5]),comments=as.numeric(mat_deltas[,6]),
                          votes=as.numeric(mat_deltas[,7]))
  
  return (data_deltas)
}

getTotals = function() {
  setwd("~/Dropbox/PhD/Thesis/Studies/IdeaScale")
  civic_communities_obs = read.csv("observations/study2-rq1/civic_communities_aggregated.csv",header=TRUE,sep=",")
  num_communities = getNumCommunities()
  
  totals_obs = civic_communities_obs[civic_communities_obs$communityid=='Totals',]
  
  remove(mat_totals)
  
  for (obs in unique(unlist(totals_obs$observation))) {
    civic_communities = civic_communities_obs[civic_communities_obs$observation==obs,]
    
    civic_communities = civic_communities[1:num_communities,]
    
    #Calculate totals for the current observation
    total_fb = sum(civic_communities$facebook)
    if (obs == 0) {
      first_wrong_tw_tot = sum(civic_communities$twitter)
    }
    total_tw = ifelse(obs<=7,fixTotalTw(sum(civic_communities$twitter),first_wrong_tw_tot),
                             sum(civic_communities$twitter))
    total_members = sum(civic_communities$members)
    total_ideas = sum(civic_communities$ideas)
    total_comments = sum(civic_communities$comments)
    total_votes = sum(civic_communities$votes)
    
    if (!exists("mat_totals")) {
      mat_totals = matrix(c(obs,total_fb,total_tw,total_members,total_ideas,total_comments,total_votes),
                          ncol=7,byrow=TRUE)
    } else {
      mat_totals = rbind(mat_totals,c(obs,total_fb,total_tw,total_members,total_ideas,total_comments,
                                      total_votes))
    }
  }
  
  colnames(mat_totals) = c('obs','facebook','twitter','members','ideas','comments','votes')
  
  data_totals = data.frame(obs=as.numeric(mat_totals[,1]),facebook=as.numeric(mat_totals[,2]),
                           twitter=as.numeric(mat_totals[,3]),members=as.numeric(mat_totals[,4]),
                           ideas=as.numeric(mat_totals[,5]),comments=as.numeric(mat_totals[,6]),
                           votes=as.numeric(mat_totals[,7]))

#linear regression analysis  
#   ggplot(data_totals, aes(facebook, members)) + 
#     geom_point(shape=1) + labs(x="facebook shares", y="members count") +
#     geom_smooth(method=lm, color = 'blue', se=FALSE, size=1) 
#   
#   ggplot(data_totals, aes(facebook, ideas)) + 
#     geom_point(shape=1) + labs(x="facebook shares", y="ideas count") +
#     geom_smooth(method=lm, color = 'blue', se=FALSE, size=1) 
#   
#   ggplot(data_totals, aes(facebook, comments)) + 
#     geom_point(shape=1) + labs(x="facebook shares", y="comments count") +
#     geom_smooth(method=lm, color = 'blue', se=FALSE, size=1) 
#   
#   ggplot(data_totals, aes(facebook, votes)) + 
#     geom_point(shape=1) + labs(x="facebook shares", y="votes count") +
#     geom_smooth(method=lm, color = 'blue', se=FALSE, size=1) 
#   
#   ggplot(data_totals, aes(twitter, members)) + 
#     geom_point(shape=1) + labs(x="twitter shares", y="members count") +
#     geom_smooth(method=lm, color = 'red', se=FALSE, size=1) 
#   
#   ggplot(data_totals, aes(twitter, ideas)) + 
#     geom_point(shape=1) + labs(x="twitter shares", y="ideas count") +
#     geom_smooth(method=lm, color = 'red', se=FALSE, size=1) 
#   
#   ggplot(data_totals, aes(twitter, comments)) + 
#     geom_point(shape=1) + labs(x="twitter shares", y="comments count") +
#     geom_smooth(method=lm, color = 'red', se=FALSE, size=1) 
#   
#   ggplot(data_totals, aes(twitter, votes)) + 
#     geom_point(shape=1) + labs(x="twitter shares", y="votes count") +
#     geom_smooth(method=lm, color = 'red', se=FALSE, size=1) 
#   
#   linear_fb_members = lm(data_totals$members~data_totals$facebook)
#   summary(linear_fb_members)
#   linear_fb_ideas = lm(data_totals$ideas~data_totals$facebook)
#   linear_fb_votes = lm(data_totals$votes~data_totals$facebook)
#   linear_fb_comments = lm(data_totals$comments~data_totals$facebook)
#   
#   summary(linear_fb_members)
#   summary(linear_fb_ideas)
#   summary(linear_fb_votes)
#   summary(linear_fb_comments)
#   
#   linear_tw_members = lm(data_totals$members~data_totals$twitter)
#   linear_tw_ideas = lm(data_totals$ideas~data_totals$twitter)
#   linear_tw_votes = lm(data_totals$votes~data_totals$twitter)
#   linear_tw_comments = lm(data_totals$comments~data_totals$twitter)
#   
#   summary(linear_tw_members)
#   summary(linear_tw_ideas)
#   summary(linear_tw_votes)
#   summary(linear_tw_comments)
  
  return (data_totals)
}

getTotalsWithoutOutliers = function() {
  setwd("~/Dropbox/PhD/Thesis/Studies/IdeaScale")
  civic_communities_obs = read.csv("observations/study2-rq1/civic_communities_aggregated.csv",header=TRUE,sep=",")
  num_communities = getNumCommunities()
  
  for (obs in unique(unlist(civic_communities_obs$observation))) {
    civic_communities = civic_communities_obs[civic_communities_obs$observation==obs,]
    
    # Remove total rows (the last three)
    civic_communities = civic_communities[1:num_communities,]
    
    # Remove outliers
    civic_communities = civic_communities[-(civic_communities$communityid==57),]
    civic_communities = civic_communities[-(civic_communities$communityid==60),]
    civic_communities = civic_communities[-(civic_communities$communityid==122),]
    civic_communities = civic_communities[-(civic_communities$communityid==404),]
    
    #Calculate totals for the current observation
    total_fb = sum(civic_communities$facebook)
    if (obs == 0) {
      first_wrong_tw_tot = sum(civic_communities$twitter)
    }
    total_tw = ifelse(obs<=7,fixTotalTw(sum(civic_communities$twitter),first_wrong_tw_tot),
                      sum(civic_communities$twitter))
    total_members = sum(civic_communities$members)
    total_ideas = sum(civic_communities$ideas)
    total_comments = sum(civic_communities$comments)
    total_votes = sum(civic_communities$votes)
    
    if (!exists("mat_totals")) {
      mat_totals = matrix(c(obs+1,total_fb,total_tw,total_members,total_ideas,total_comments,total_votes),
                          ncol=7,byrow=TRUE)
    } else {
      mat_totals = rbind(mat_totals,c(obs+1,total_fb,total_tw,total_members,total_ideas,total_comments,
                                      total_votes))
    }
  }
  
  colnames(mat_totals) = c('obs','facebook','twitter','members','ideas','comments','votes')
  
  data_totals = data.frame(obs=as.numeric(mat_totals[,1]),facebook=as.numeric(mat_totals[,2]),
                           twitter=as.numeric(mat_totals[,3]),members=as.numeric(mat_totals[,4]),
                           ideas=as.numeric(mat_totals[,5]),comments=as.numeric(mat_totals[,6]),
                           votes=as.numeric(mat_totals[,7]))

  
  return (data_totals)
}

getTotalsObs = function(share_ratios) {
  setwd("~/Dropbox/PhD/Thesis/Studies/IdeaScale")
  civic_communities_obs = read.csv("dumps/active_civic_communities_final.csv",header=TRUE,sep=",")
  
  for (obs in unique(unlist(civic_communities_obs$observation))) {
    civic_communities = civic_communities_obs[civic_communities_obs$observation==obs,]
    
    #Calculate the totals for the current observation
    total_members = sum(civic_communities$members)
    total_fb = ifelse(share_ratios,sum(civic_communities$facebook)/total_members,sum(civic_communities$facebook))
    total_tw = ifelse(share_ratios,sum(civic_communities$twitter)/total_members,sum(civic_communities$twitter))
    total_ideas = sum(civic_communities$ideas)
    total_comments = sum(civic_communities$comments)
    total_votes = sum(civic_communities$votes)
    total_age = sum(civic_communities$age)
    
    if (!exists("mat_totals")) {
      mat_totals = matrix(c(obs+1,total_fb,total_tw,total_members,total_ideas,total_comments,total_votes,
                            total_age), ncol=8,byrow=TRUE)
    } else {
      mat_totals = rbind(mat_totals,c(obs+1,total_fb,total_tw,total_members,total_ideas,total_comments,
                                      total_votes, total_age))
    }
  }
  
  colnames(mat_totals) = c('obs','facebook','twitter','members','ideas','comments','votes', 'age')
  
  data_totals = data.frame(obs=as.numeric(mat_totals[,1]),facebook=as.numeric(mat_totals[,2]),
                           twitter=as.numeric(mat_totals[,3]),members=as.numeric(mat_totals[,4]),
                           ideas=as.numeric(mat_totals[,5]),comments=as.numeric(mat_totals[,6]),
                           votes=as.numeric(mat_totals[,7]), age=as.numeric(mat_totals[,8]))
  
  return (data_totals)
}

calCommunityRatios = function() {
  setwd("~/Dropbox/PhD/Thesis/Studies/IdeaScale")
  civic_communities = read.csv("dumps/active_civic_communities_final.csv",header=TRUE,sep=",")
  
  # Get last observation
  civic_communities = civic_communities[civic_communities$observation==13, c("communityid","facebook","twitter","members")]
  
  # Calculate Ratios
  civic_communities["ratio_fb"] = civic_communities$facebook/civic_communities$members
  civic_communities["ratio_tw"] = civic_communities$twitter/civic_communities$members
  
  colMeans(civic_communities["ratio_fb"])
  colMeans(civic_communities["ratio_tw"])
  
  # Plot ratio distributions
  qplot(ratio_fb, data=civic_communities, geom="histogram", xlab="facebook shares/members", binwidth=0.3)
  qplot(ratio_tw, data=civic_communities, geom="histogram", xlab="twitter shares/members", binwidth=0.1) 
}

getNumCommunities = function() {
  setwd("~/Dropbox/PhD/Thesis/Studies/IdeaScale")  
  communities = read.csv("observations/study2-rq1/civic_communities_aggregated.csv",header=TRUE,sep=",")
  
  #-3 because we don't have to count the rows containing the totals and the correlation coefficients
  num_civic_communities = length(communities[communities$observation==0,1]) - 3
  
  return (num_civic_communities)
}

betaWeight = function(coef, x, y) {
  return (coef * (sd(x) / sd(y)))
}

zeroOrderCor = function(x, y) {
  return (cor(x,y))
}

semiPartialCor = function(cor_x1_y, cor_x2_y, cor_x1_x2) {
  sr1 = (cor_x1_y - (cor_x2_y * cor_x1_x2)) / sqrt(1-(cor_x1_x2^2))
  return (sr1)
}

calRelImporTable = function(x1, x1_name, x2, x2_name, y, data) {
  #y = community$members
  #x1 = community$ratio_fb
  #x2 = community$age
  
  # Calculating model
  model = lm(y~x1+x2, data=data)
  
  # Calculating Beta Weights (B)
  if (!is.na(model$coeff["x1"])) {
    beta_x1 = betaWeight(model$coeff["x1"], x1, y)
    p_value_beta_x1 = summary(model)$coefficients["x1",4] 
  } else {
    beta_x1 = "NaN"
    p_value_beta_x1 = "NaN"
  }
  if (!is.na(model$coeff["x2"])) {
    beta_x2 = betaWeight(model$coeff["x2"], x2, y)
    p_value_beta_x2 = summary(model)$coefficients["x2",4]
  } else {
    beta_x2 = "NaN"
    p_value_beta_x2 = "NaN"
  }
  
  # Calculating Zero-Order Correlations (R) 
  r_x1_y = zeroOrderCor(x1, y)
  r_x2_y = zeroOrderCor(x2, y)
  r_x1_x2 = zeroOrderCor(x1, x2)
  
  # Calculating Coefficients of Determination (R^2)
  r2_x1_y = r_x1_y ^ 2
  r2_x2_y = r_x2_y ^ 2
  r2_x1_x2 = r_x1_x2 ^ 2
  
  # Calculating Semi-Partial Correlations
  sr_x1_y = semiPartialCor(r_x1_y,r_x2_y,r_x1_x2)
  sr_x2_y = semiPartialCor(r_x2_y,r_x1_y,r_x1_x2)
  
  sr2_x1_y = sr_x1_y ^ 2 
  sr2_x2_y = sr_x2_y ^ 2
  
  if (closeToZero(sr2_x1_y) | closeToZero(sr2_x2_y)) {
    if(closeToZero(sr2_x1_y)) {
      rw_x1=0
      rw_x2=1
    } else { 
      rw_x1=1
      rw_x2=0
    }
  } else {
    # Calculating Relative Weights
    rel_weights = calc.relimp(model,type=c("genizi"), rela=TRUE)
    rw_x1 = rel_weights$genizi["x1"]
    rw_x2 = rel_weights$genizi["x2"] 
  }
  
  if (p_value_beta_x1 < 0.05) {
    beta_x1_p = paste(round(beta_x1,3)," (p < 0.05)",sep="")
  } else {
    beta_x1_p = paste(round(beta_x1,3)," (p = ",signif(p_value_beta_x1,3),")",sep="")
  }
  
  if (p_value_beta_x2 < 0.05) {
    beta_x2_p = paste(round(beta_x2,3)," (p < 0.05)",sep="")
  } else {
    beta_x2_p = paste(round(beta_x2,3)," (p = ",signif(p_value_beta_x2,3),")",sep="")
  }
  
  table = matrix(c(beta_x1_p,round(r_x1_y,3),round(r2_x1_y,3),round(sr2_x1_y,3),round(rw_x1,3)), ncol=5,byrow=TRUE)
  table = rbind(table,c(beta_x2_p,round(r_x2_y,3),round(r2_x2_y,3),round(sr2_x2_y,3),round(rw_x2,3)))
  colnames(table) = c("Beta Weight (sig.)","R","R^2","SR^2","RW")
  rownames(table) = c(x1_name,x2_name)
  
  return (table)
}

calRelImporTableCommunity = function(community) {
  table = NULL
  fb_social = FALSE
  tw_social = FALSE
  
  #---------- Facebook -------------#
  if (sd(community$ratio_fb) != 0 & sd(community$members) != 0) {
    ri_table = calRelImporTable(community$ratio_fb,"ratio_fb",community$age,"age",community$members,community)
    if (ri_table[1,5] > ri_table[2,5]) {
      fb_social = TRUE
      table = matrix(c('-','-','Members','-','-','X'),ncol=6,byrow=TRUE)
    } else {
      table = matrix(c('-','-','Members','-','-','-'),ncol=6,byrow=TRUE)
    }
    table = rbind(table,c("Ind. Variables","Beta Weight (sig.)","R","R^2","SR^2","RW"))
    table = rbind(table,c('Ratio Facebook/Members',ri_table[1,1],ri_table[1,2],ri_table[1,3],ri_table[1,4],ri_table[1,5]))
    table = rbind(table,c('Age',ri_table[2,1],ri_table[2,2],ri_table[2,3],ri_table[2,4],ri_table[2,5]))
  }
  
  if (sd(community$ratio_fb) != 0 & sd(community$ideas) != 0) {
    ri_table = calRelImporTable(community$ratio_fb,"ratio_fb",community$age,"age",community$ideas,community)
    if (ri_table[1,5] > ri_table[2,5]) {
      fb_social = TRUE
      if (is.null(table)) {
        table = matrix(c('-','-','Ideas','-','-','X'),ncol=6,byrow=TRUE)
      } else {
        table = rbind(table,c('-','-','Ideas','-','-','X')) 
      }
    } else {
      if (is.null(table)) {
        table = matrix(c('-','-','Ideas','-','-','-'),ncol=6,byrow=TRUE)
      } else {
        table = rbind(table,c('-','-','Ideas','-','-','-'))
      }
    }
    table = rbind(table,c("Ind. Variables","Beta Weight (sig.)","R","R^2","SR^2","RW"))
    table = rbind(table,c('Ratio Facebook/Members',ri_table[1,1],ri_table[1,2],ri_table[1,3],ri_table[1,4],ri_table[1,5]))
    table = rbind(table,c('Age',ri_table[2,1],ri_table[2,2],ri_table[2,3],ri_table[2,4],ri_table[2,5]))
  }
  
  if (sd(community$ratio_fb) != 0 & sd(community$votes) != 0) {
    ri_table = calRelImporTable(community$ratio_fb,"ratio_fb",community$age,"age",community$votes,community)
    if (ri_table[1,5] > ri_table[2,5]) {
      fb_social = TRUE
      if (is.null(table)) {
        table = matrix(c('-','-','Votes','-','-','X'),ncol=6,byrow=TRUE)
      } else {
        table = rbind(table,c('-','-','Votes','-','-','X')) 
      }
    } else {
      if (is.null(table)) {
        table = matrix(c('-','-','Votes','-','-','-'),ncol=6,byrow=TRUE)  
      } else {
        table = rbind(table,c('-','-','Votes','-','-','-')) 
      }
    }
    table = rbind(table,c("Ind. Variables","Beta Weight (sig.)","R","R^2","SR^2","RW"))
    table = rbind(table,c('Ratio Facebook/Members',ri_table[1,1],ri_table[1,2],ri_table[1,3],ri_table[1,4],ri_table[1,5]))
    table = rbind(table,c('Age',ri_table[2,1],ri_table[2,2],ri_table[2,3],ri_table[2,4],ri_table[2,5]))
  }
  
  if (sd(community$ratio_fb) != 0 & sd(community$comments) != 0) {
    ri_table = calRelImporTable(community$ratio_fb,"ratio_fb",community$age,"age",community$comments,community)
    if (ri_table[1,5] > ri_table[2,5]) {
      fb_social = TRUE
      if (is.null(table)) {
        table = matrix(c('-','-','Comments','-','-','X'),ncol=6,byrow=TRUE)
      } else {
        table = rbind(table,c('-','-','Comments','-','-','X')) 
      }
    } else {
      if (is.null(table)) {
        table = matrix(c('-','-','Comments','-','-','-'),ncol=6,byrow=TRUE)  
      } else {
        table = rbind(table,c('-','-','Comments','-','-','-'))
      }
    }
    table = rbind(table,c("Ind. Variables","Beta Weight (sig.)","R","R^2","SR^2","RW"))
    table = rbind(table,c('Ratio Facebook/Members',ri_table[1,1],ri_table[1,2],ri_table[1,3],ri_table[1,4],ri_table[1,5]))
    table = rbind(table,c('Age',ri_table[2,1],ri_table[2,2],ri_table[2,3],ri_table[2,4],ri_table[2,5]))
  }
  
  # ----------- Twitter -------------- #
  if (sd(community$ratio_tw) != 0 & sd(community$members) != 0) {
    ri_table = calRelImporTable(community$ratio_tw,"ratio_tw",community$age,"age",community$members,community)
    if (ri_table[1,5] > ri_table[2,5]) {
      tw_social = TRUE
      if (is.null(table)) {
        table = matrix(c('-','-','Members','-','-','X'),ncol=6,byrow=TRUE)
      }
      else {
        table = rbind(table,c('-','-','Members','-','-','X'))
      }
    } else {
      if (is.null(table)) {
        table = matrix(c('-','-','Members','-','-','X'),ncol=6,byrow=TRUE)  
      } else {
        table = rbind(table,c('-','-','Members','-','-','-')) 
      }
    }
    table = rbind(table,c("Ind. Variables","Beta Weight (sig.)","R","R^2","SR^2","RW"))
    table = rbind(table,c('Ratio Twitter/Members',ri_table[1,1],ri_table[1,2],ri_table[1,3],ri_table[1,4],ri_table[1,5]))
    table = rbind(table,c('Age',ri_table[2,1],ri_table[2,2],ri_table[2,3],ri_table[2,4],ri_table[2,5]))
  }
  
  if (sd(community$ratio_tw) != 0 & sd(community$ideas) != 0) {
    ri_table = calRelImporTable(community$ratio_tw,"ratio_tw",community$age,"age",community$ideas,community)
    if (ri_table[1,5] > ri_table[2,5]) {
      tw_social = TRUE
      if (is.null(table)) {
        table = matrix(c('-','-','Ideas','-','-','X'),ncol=6,byrow=TRUE)
      } else {
        table = rbind(table,c('-','-','Ideas','-','-','X'))
      }
    } else {
      if (is.null(table)) {
        table = matrix(c('-','-','Ideas','-','-','-'),ncol=6,byrow=TRUE)  
      } else {
        table = rbind(table,c('-','-','Ideas','-','-','-'))
      }
    }
    table = rbind(table,c("Ind. Variables","Beta Weight (sig.)","R","R^2","SR^2","RW"))
    table = rbind(table,c('Ratio Twitter/Members',ri_table[1,1],ri_table[1,2],ri_table[1,3],ri_table[1,4],ri_table[1,5]))
    table = rbind(table,c('Age',ri_table[2,1],ri_table[2,2],ri_table[2,3],ri_table[2,4],ri_table[2,5]))
  }
  
  if (sd(community$ratio_tw) != 0 & sd(community$votes) != 0) {
    ri_table = calRelImporTable(community$ratio_tw,"ratio_tw",community$age,"age",community$votes,community)
    if (ri_table[1,5] > ri_table[2,5]) {
      tw_social = TRUE
      if (is.null(table)) {
        table = matrix(c('-','-','Votes','-','-','X'),ncol=6,byrow=TRUE)
      } else {
        table = rbind(table,c('-','-','Votes','-','-','X'))
      }
    } else {
      if (is.null(table)) {
        table = matrix(c('-','-','Votes','-','-','-'),ncol=6,byrow=TRUE)  
      } else {
        table = rbind(table,c('-','-','Votes','-','-','-')) 
      }
    }
    table = rbind(table,c("Ind. Variables","Beta Weight (sig.)","R","R^2","SR^2","RW"))
    table = rbind(table,c('Ratio Twitter/Members',ri_table[1,1],ri_table[1,2],ri_table[1,3],ri_table[1,4],ri_table[1,5]))
    table = rbind(table,c('Age',ri_table[2,1],ri_table[2,2],ri_table[2,3],ri_table[2,4],ri_table[2,5]))
  }
  
  if (sd(community$ratio_tw) != 0 & sd(community$comments) != 0) {
    ri_table = calRelImporTable(community$ratio_tw,"ratio_tw",community$age,"age",community$comments,community)
    if (ri_table[1,5] > ri_table[2,5]) {
      tw_social = TRUE
      if (is.null(table)) {
        table = matrix(c('-','-','Comments','-','-','X'),ncol=6,byrow=TRUE)  
      } else {
        table = rbind(table,c('-','-','Comments','-','-','X')) 
      }
    } else {
      if (is.null(table)) {
        table = matrix(c('-','-','Comments','-','-','-'),ncol=6,byrow=TRUE)  
      } else {
        table = rbind(table,c('-','-','Comments','-','-','-'))
      }
    }
    table = rbind(table,c("Ind. Variables","Beta Weight (sig.)","R","R^2","SR^2","RW"))
    table = rbind(table,c('Ratio Twitter/Members',ri_table[1,1],ri_table[1,2],ri_table[1,3],ri_table[1,4],ri_table[1,5]))
    table = rbind(table,c('Age',ri_table[2,1],ri_table[2,2],ri_table[2,3],ri_table[2,4],ri_table[2,5]))
  }
  
  if (!is.null(table)) {
    table = rbind(table,c('-','-','-','-','-','-')) 
  }
  
  return (list(tables=table,fb_social=fb_social,tw_social=tw_social))
}

calRelImporTables = function() {
  setwd("~/Dropbox/PhD/Thesis/Studies/IdeaScale")
  civic_communities = read.csv("dumps/active_civic_communities_final.csv",header=TRUE,sep=",")
  FILE_CSV = 'dumps/multiple_regression.csv'
  antisocials = NULL
  socials = NULL
  
  # Remove outliers (observation 0 and 1)
  civic_communities = subset(civic_communities,(civic_communities[ ,"observation"] != 0 & civic_communities[ ,"observation"] != 1))
  
  # Iterate over communities and calculate their relative importance table
  for (community_id in unique(unlist(civic_communities[,"communityid"]))) {
    community = civic_communities[civic_communities$communityid==community_id,]
    
    # Calculate ratios
    community["ratio_fb"] = community[,"facebook"]/community[,"members"]
    community["ratio_tw"] = community[,"twitter"]/community[,"members"]
    
    # Get final ratios
    comm_ratio_fb = community[community["observation"]==13,"ratio_fb"]
    comm_ratio_tw = community[community["observation"]==13,"ratio_tw"]
    
    # Calculate Standard Deviations
    sd_fb_ratio = round(sd(community$ratio_fb),3)
    sd_tw_ratio = round(sd(community$ratio_tw),3)
    sd_members = round(sd(community$members),3)
    sd_ideas = round(sd(community$ideas),3)
    sd_votes = round(sd(community$votes),3)
    sd_comments = round(sd(community$comments),3)
    
    # Create Header
    header = matrix(c("-","Id",community_id,"-","-","-"), ncol=6, byrow=T)
    header = rbind(header, c("Mean FB/M Ratio",round(mean(community$ratio_fb),3),"","","Mean TW/M Ratio",round(mean(community$ratio_tw),3)))
    header = rbind(header, c("SD FB/M Ratio",sd_fb_ratio,"SD TW/M Ratio",sd_tw_ratio,"SD Members",sd_members))
    header = rbind(header, c("SD Ideas",sd_ideas,"SD Votes",sd_votes,"SD Comments",sd_comments))
    
    # Calculate Relative Importance Table
    ret = calRelImporTableCommunity(community)
    
    if (!file.exists(FILE_CSV)) {
      write.table(header,file=FILE_CSV,append=F,sep=",",col.names=F,row.names=F)
    } else {
      write.table(header,file=FILE_CSV,append=T,sep=",",col.names=F,row.names=F) 
    }
    
    if (!is.null(ret$tables)) {
      write.table(ret$tables,file=FILE_CSV,append=T,sep=",",col.names=F,row.names=T)
    }
    
    # Prepare the datastructure to print out a final summary at the end of the file
    if (ret$fb_social | ret$tw_social) {
      if (ret$fb_social & ret$tw_social) {
        row = c(community_id,comm_ratio_fb,comm_ratio_tw)
      } else {
        if (ret$fb_social) {
          row = c(community_id,comm_ratio_fb,0)
        }
        else {
          row = c(community_id,0,comm_ratio_tw)
        }
      }
      if (is.null(socials)) {
        socials = matrix(row, ncol=3, byrow=T)    
      } else {
        socials = rbind(socials, row)    
      }
    } else {
      if (sd_fb_ratio != 0 | sd_tw_ratio != 0 | sd_members != 0 | sd_ideas != 0 | sd_votes != 0 | sd_comments != 0) {
        if (is.null(antisocials)) {
          antisocials = matrix(c(community_id,comm_ratio_fb,comm_ratio_tw), ncol=3, byrow=T)
        } else {
          antisocials = rbind(antisocials, c(community_id,comm_ratio_fb,comm_ratio_tw))
        }
      }
    }
  }
  
  summary = matrix(c("-","-","-","-","-","-"), ncol=6, byrow=T)
  summary = rbind(summary, c("","Summary","","","",""))
  summary = rbind(summary, c("","Socials","","","",""))
  summary = rbind(summary, c("","Ids",paste(socials[,1], collapse=" "),"","",""))
  summary = rbind(summary, c("","FB Ids:",paste(socials[socials[,2]!=0,1], collapse=" "),"","",""))
  summary = rbind(summary, c("","FB Mean:",round(mean(socials[socials[,2]!=0,2]),3),"","",""))
  summary = rbind(summary, c("","FB Range:",paste("[",round(min(socials[socials[,2]!=0,2]),3),", ",round(max(socials[socials[,2]!=0,2]),3),"]",sep=""),"","",""))
  summary = rbind(summary, c("","TW Ids:",paste(socials[socials[,3]!=0,1], collapse=" "),"","",""))
  summary = rbind(summary, c("","TW Mean:",round(mean(socials[socials[,3]!=0,3]),3),"","",""))
  summary = rbind(summary, c("","TW Range:",paste("[",round(min(socials[socials[,3]!=0,3]),3),", ",round(max(socials[socials[,3]!=0,3]),3),"]",sep=""),"","",""))
  summary = rbind(summary, c("-","-","-","-","-","-"))
  summary = rbind(summary, c("","Antisocials","","","",""))
  summary = rbind(summary, c("","Ids:",paste(antisocials[,1], collapse=" "),"","",""))
  summary = rbind(summary, c("","FB Mean:",round(mean(antisocials[antisocials[,2]!=0,2]),3),"","",""))
  summary = rbind(summary, c("","FB Range:",paste("[",round(min(antisocials[,2]),3),", ",round(max(antisocials[,2]),3),"]", sep=""),"","",""))
  summary = rbind(summary, c("","TW Mean:",round(mean(antisocials[antisocials[,3]!=0,3]),3),"","",""))
  summary = rbind(summary, c("","TW Range:",paste("[",round(min(antisocials[,3]),3),", ",round(max(antisocials[,3]),3),"]", sep=""),"","",""))
  
  # Summary
  write.table(summary,file=FILE_CSV,append=T,sep=",",col.names=F,row.names=F)
}

prepareCivicCommunitiesRegistry = function() {
  setwd("~/Dropbox/PhD/Thesis/Studies/IdeaScale")
  civic_communities_obs = read.csv("observations/study2-rq1/civic_communities_aggregated.csv",header=TRUE,sep=",")
  civic_communities_age = read.csv("dumps/age_active_civic_communities.csv",header=TRUE,sep=",")
  FILE_CSV = 'dumps/active_civic_communities_final.csv'
  
  remove(mat_comm)
  
  for (obs in unique(unlist(civic_communities_obs$observation))) {
    communities_obs = civic_communities_obs[civic_communities_obs$observation==obs,]
    
    # Remove Outliers
    communities_obs = subset(communities_obs, communities_obs[ ,"communityid"] != 26)  # Members
    communities_obs = subset(communities_obs, communities_obs[ ,"communityid"] != 28)  # Votes
    communities_obs = subset(communities_obs, communities_obs[ ,"communityid"] != 38)  # Votes
    communities_obs = subset(communities_obs, communities_obs[ ,"communityid"] != 57)  # Facebook
    communities_obs = subset(communities_obs, communities_obs[ ,"communityid"] != 60)  # Twitter
    communities_obs = subset(communities_obs, communities_obs[ ,"communityid"] != 122) # Twitter
    communities_obs = subset(communities_obs, communities_obs[ ,"communityid"] != 367) # Members
    communities_obs = subset(communities_obs, communities_obs[ ,"communityid"] != 404) # Facebook
    communities_obs = subset(communities_obs, communities_obs[ ,"communityid"] != 413) # Twitter
    communities_obs = subset(communities_obs, communities_obs[ ,"communityid"] != 454) # Ideas
    
    # Remove Total Rows
    communities_obs = communities_obs[1:(nrow(communities_obs)-3),]
    
    for (id in unique(unlist(communities_obs$communityid))) {
      community = communities_obs[communities_obs$communityid==id,]
      community["age"] = civic_communities_age[civic_communities_age$id==id,"age"] + (obs * 5)
      if (!exists("mat_comm")) {
        mat_comm = matrix(c(community$observation,id,community$facebook,community$twitter,community$members,
                            community$ideas,community$comments,community$votes,community$age), ncol=9,byrow=TRUE)
      } else {
        mat_comm = rbind(mat_comm,c(community$observation,id,community$facebook,community$twitter,community$members,
                                    community$ideas,community$comments,community$votes,community$age))
      }   
    }
  }
  
  colnames(mat_comm) = c("observation","communityid","facebook","twitter","members","ideas","comments","votes","age")
  
  mat_comm = fixTwValues(mat_comm)
  
  write.table(mat_comm,file=FILE_CSV,append=F,sep=",",col.names=T,row.names=F)
}

# At the beggining and until observation 7, I had an error collecting twitter shares, the real values would
# be around the values of observation 8. Basically, the error was that the crawler only considered the first
# digit of the twitter counter, so instead of reporting 38 it reported 3. Happily, only 13 of the 33 
# communities should be fixed and only two (465 and 452) of them require a special attention.
fixTwValues = function(mat_comm) {
  for (id in unique(unlist(mat_comm[,"communityid"]))) {
    tw = as.numeric(mat_comm[mat_comm[,"communityid"]==id,"twitter"])
    if (id == 465) {
      mat_comm[mat_comm[,"communityid"]==id,"twitter"] = tw[9]    
    } else {
      if (id == 454) {
        mat_comm[mat_comm[,"communityid"]==id&mat_comm[,"observation"]==0,"twitter"] = tw[9] - 4
        mat_comm[mat_comm[,"communityid"]==id&mat_comm[,"observation"]==1,"twitter"] = tw[9] - 4
        mat_comm[mat_comm[,"communityid"]==id&mat_comm[,"observation"]==2,"twitter"] = tw[9] - 4
        mat_comm[mat_comm[,"communityid"]==id&mat_comm[,"observation"]==3,"twitter"] = tw[9] - 4
        mat_comm[mat_comm[,"communityid"]==id&mat_comm[,"observation"]==4,"twitter"] = tw[9] - 2
        mat_comm[mat_comm[,"communityid"]==id&mat_comm[,"observation"]==5,"twitter"] = tw[9] - 2
        mat_comm[mat_comm[,"communityid"]==id&mat_comm[,"observation"]==6,"twitter"] = tw[9] - 2
        mat_comm[mat_comm[,"communityid"]==id&mat_comm[,"observation"]==7,"twitter"] = tw[9] - 2
      } else {
        if (id != 476 & !constantRange(tw)) {
          if (constantRange(tw[9:13])) {
            mat_comm[mat_comm[,"communityid"]==id,"twitter"] = tw[9]
          } else {
            cat("error, twitter shares from observation 8 in advance should be constant. community id:",id)
          }
        } else {
          cat("ok!, community id:",id)
        }
      }
    }
  }
  
  return (mat_comm)
}

constantRange = function(range) {
  n1 = range[1]
  constant = TRUE
  
  for (nx in range[2:length(range)]) {
    if (nx==n1) {
      n1=nx
    } else {
      constant=FALSE
    }
  }
  
  return (constant)
}

closeToZero = function(num) {
  return (round(num,10)==0)
}