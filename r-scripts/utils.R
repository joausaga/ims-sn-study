getTotals = function(communities) {
  for (obs in unique(unlist(communities$observation))) {
    civic_communities = communities[communities$observation==obs,]
    
    #Calculate the totals for the current observation
    total_members = sum(civic_communities$members)
    total_fb = sum(civic_communities$facebook)
    total_tw = sum(civic_communities$twitter)
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

getTotalsObs = function(share_ratios) {
  #setwd("")  # Set to the path of the repository
  civic_communities_obs = read.csv("./datasets/active_civic_communities_final.csv",header=TRUE,sep=",")
  
  # Getting rid of communities that were incorrectly included
  civic_communities_obs = filter(civic_communities_obs,! communityid %in% c(190, 203, 306, 327))
  
  ret = getTotals(civic_communities_obs)
  
  return (ret)
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
  setwd("")  # Set to the path of this repository
  civic_communities = read.csv("datasets/active_civic_communities_final.csv",header=TRUE,sep=",")
  FILE_CSV = 'datasets/multiple_regression.csv'
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
