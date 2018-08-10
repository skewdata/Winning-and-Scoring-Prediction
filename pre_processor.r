
library(stringr)
library(caTools)
library(lattice)
library(ggplot2)
library(caret)
library(mlbench)

team_data = read.csv("India_ODI_Match_Data.csv")[,-1][,-1][,-1]  # removing the first 3 columns as there are only indexes
team_data = team_data[,!duplicated(colnames(team_data))]  # removing duplicated columns
team_data = team_data[,-which(names(team_data) %in% c("MatchID"))] # removing extra column "MatchID" 


# Transforming data
team_data$runs_scored = as.numeric(word(team_data$score,sep = "/"))
team_data$wickets_conceeded = sapply(as.character(team_data$score),get_wickets_conceeded)
levels(team_data$result) = 1:length(levels(as.factor(team_data$result))) #won - 4, lost - 1, 2- no-result, tied -3
levels(team_data$toss_result) = 1:length(levels(as.factor(team_data$toss_result)))# 2-lost , 1-win
levels(team_data$strategy) = 1:length(levels(as.factor(team_data$strategy)))# 2-Bat , 1-Field
levels(team_data$match_timing) = 1:length(levels(as.factor(team_data$match_timing)))# 1-D/N, 2-D
levels(team_data$home_ground) = 1:length(levels(as.factor(team_data$home_ground),na.exclude()))# FALSE -1, N/A-2 , 3-TRUE

team_data_final <- team_data[,-which(names(team_data) %in% c("odi_id","date","oppositon","ground","score"))] # removing extra column "MatchID" 

team_data_final <- lapply(team_data_final, function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})
sapply(team_data_final, class)

# Feature Analysis

# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
model <- train(result~., data=team_data_final, method="lvq", preProcess="scale", trControl=control,na.omit("home_ground"))

varImp()



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

#get_wickets_conceeded("NA")
