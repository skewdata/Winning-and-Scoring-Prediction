
library(dplyr)
library(stringr)
library(caTools)
library(lattice)
library(ggplot2)
library(caret)
library(mlbench)
library(mice)
library(Hmisc)
library(corrr)
library(gridExtra)
get_wickets_conceeded <- function(score){
  if(grepl("/",score)){
    wickets = as.numeric(strsplit(as.character(score),split = "/")[[1]][2])  
  }else{
    if(!grepl("DNB",score) && !grepl("NA",score)){
      wickets = 10  
    }else{
      wickets = -1
    }
  }
  return(wickets)
}

normalization <- function(data,x)
{
  for(j in x)
  {
    data[!(is.na(data[,j])),j] = (data[!(is.na(data[,j])),j] - min(data[!(is.na(data[,j])),j]))/(max(data[!(is.na(data[,j])),j])- min(data[!(is.na(data[,j])),j]))
  }
  return(data)
}

team_data = read.csv("India_ODI_Match_Data.csv")[,-1][,-1][,-1]  # removing the first 3 columns as there are only indexes
team_data = team_data[,!duplicated(colnames(team_data))]  # removing duplicated columns
team_data = team_data[,-which(names(team_data) %in% c("MatchID"))] # removing extra column "MatchID" 

#Renaming columns
# Transforming data
team_data$runs_scored = as.numeric(word(team_data$score,sep = "/"))
team_data$wickets_conceeded = sapply(as.character(team_data$score),get_wickets_conceeded)
levels(team_data$result) = 1:length(levels(as.factor(team_data$result))) #won - 4, lost - 1, 2- no-result, tied -3
levels(team_data$toss_result) = 1:length(levels(as.factor(team_data$toss_result)))# 2-lost , 1-win
levels(team_data$strategy) = 1:length(levels(as.factor(team_data$strategy)))# 2-Bat , 1-Field
levels(team_data$match_timing) = 1:length(levels(as.factor(team_data$match_timing)))# 1-D/N, 2-D
levels(team_data$home_ground) = 1:length(levels(as.factor(team_data$home_ground)))# FALSE -1, N/A-2 , 3-TRUE

team_data_final <- team_data[,-which(names(team_data) %in% c("odi_id","date","oppositon","ground","score"))] # removing extra column "MatchID" 
colnames(team_data_final)[10] <- "batting_avg"
colnames(team_data_final)[11] <- "bowling_avg"
team_data_final$home_ground = impute(team_data_final$home_ground,mode)  # dealing missing values

# finding NA's
md.pattern(team_data_final)
hist(team_data_final$runs_scored)
team_data_final$runs_scored <- impute(team_data_final$runs_scored,mean)

# outlier analysis
str(team_data_final)
num_col <- c(1,7,8,9,10,11,12,13,14,15,16,17)
fact_col <- c(2,3,4,5,6)

temp_data_numeric = team_data_final[,num_col]
temp_data_factor = team_data_final[,fact_col]

temp_data_numeric <- normalization(temp_data_numeric,c(1:ncol(temp_data_numeric)))
boxplot(temp_data_numeric)

p1 <- ggplot(temp_data_factor,aes(x=temp_data_factor$result)) + geom_bar()+ geom_text(stat='count', aes(label=..count..), vjust=-1)
p2 <- ggplot(temp_data_factor,aes(x=temp_data_factor$toss_result)) + geom_bar()+geom_text(stat='count', aes(label=..count..), vjust=-1)
p3 <- ggplot(temp_data_factor,aes(x=temp_data_factor$strategy)) + geom_bar()+geom_text(stat='count', aes(label=..count..), vjust=-1)
p4 <- ggplot(temp_data_factor,aes(x=temp_data_factor$match_timing)) + geom_bar()+geom_text(stat='count', aes(label=..count..), vjust=-1)
p5 <- ggplot(temp_data_factor,aes(x=temp_data_factor$home_ground)) + geom_bar()+geom_text(stat='count', aes(label=..count..), vjust=-1)
grid.arrange(p1,p2,p3,p4,p5,nrow=3)

# finding correlations
data.matrix(team_data_final)%>%correlate()%>%focus(result)%>%
  mutate(rowname = factor(rowname, levels = rowname[order(abs(result),decreasing = TRUE)])) %>%
  ggplot(aes(x = rowname, y = result)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=round(result,digits = 2)), vjust=2)+
  ylab("Correlation with Result") +
  xlab("Variable")
  



#get_wickets_conceeded("NA")
