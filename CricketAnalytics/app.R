

library(shiny)
library(DataCombine)
library(xml2)
library(rvest)
library(stringr)
library(dplyr)
library(caTools)

stats_base_url <- 'http://stats.espncricinfo.com'
search_url <- paste(stats_base_url,'/ci/engine/stats/analysis.html?search=<<key>>;template=analysis',sep = "")
player_url <- 'http://www.espncricinfo.com/india/content/player/'
venue_data <- read.csv("all_venues.csv")
CricketData <- read.csv('CricketModel_FINAL_LOG.csv', header = TRUE)
TeamNameData <- read.csv('TeamNameMapping.csv', header = TRUE)

#print(TeamNameData[which(TeamNameData$CrickInfo == 40),2])
#TeamAMatches <- subset(CricketData,CricketData$TeamName_V == TeamNameData[which(TeamNameData$CrickInfo == 40),2] & CricketData$ground_V == 83)

OppositionData <- read.csv('Opposition_Mapping.csv', header = TRUE)
print(as.numeric(OppositionData[which(OppositionData$CricInfo == 29),2]))
TeamBMatches <- subset(CricketData,CricketData$TeamName_V == as.numeric(OppositionData[which(OppositionData$CricInfo == 29),2]) & CricketData$ground_V == 83)

CricketData$result_V <- as.factor(CricketData$result_V)
groundMap <- read.csv('ground_Mapping.csv',header = TRUE)
#groundMap$ground <- as.character(groundMap$ground)
#test <- read.csv('empty.csv',header = TRUE)
#str(CricketData)

Predict_df <- data.frame(total_batsmen = numeric(), total_bowlers = numeric(), total_allrounders = numeric(),WindSpeed = numeric(), Temperature = numeric(), Humidity = numeric(), WindDirectionDegree = numeric(), score.1 = numeric(), wickets = numeric(), TeamName_V = numeric(), Opposition_V = numeric(), ground_V = numeric(), result_V = factor(), toss_result_V = numeric(), strategy_V = numeric(), home_ground_V = numeric())

CricketModel <- glm(result_V ~ .,data = CricketData, family = binomial)
Result_Outcome  <- "Enter Values and click SUBMIT"
#getwd()
setwd('D:/DataScienceWorkspace/R-Workspace/CricketAnalytics')
#PredictData <- read.csv('Predict.csv',header = TRUE)
#PredictData$result_V <- as.factor(PredictData$result_V)
#get_MatchPrediction(PredictData)

get_MatchPrediction <- function(predictInput){
  print(predictInput)
  prediction <- predict(CricketModel,predictInput, type = 'response')
  paste('Probabilty of winning is ',prediction, sep = '')
}

classify_players <- function(player_vector){
  allrounders <- 0
  batsmen <- 0
  bowlers <- 0
  print(player_vector)
  for(player in player_vector)
  {
    #print(player)
    playerName <- player
    playerId <-get_player_id(player)
    #print(playerId)
    #print(paste(player_url,playerId,'.html',sep = ''))
    playerBattingTable <- as.data.frame(read_html(paste(player_url,playerId,'.html',sep = '')) %>% html_nodes(xpath = '//*[@id="ciHomeContentlhs"]/div[4]/table[1]') %>% html_table())
    playerBattingAvg <- as.numeric(playerBattingTable[which(playerBattingTable$Var.1 == "ODIs"),7])
    if(is.na(playerBattingAvg))
    {
      playerBattingAvg <- 0
    }
    playerBowlingTable <- as.data.frame(read_html(paste(player_url,playerId,'.html',sep = '')) %>% html_nodes(xpath = '//*[@id="ciHomeContentlhs"]/div[4]/table[2]') %>% html_table())
    playerBowls <- as.numeric(playerBowlingTable[which(playerBattingTable$Var.1 == "ODIs"),4])
    if(is.na(playerBowls))
    {
      playerBowls <- 0
    }
    #print(playerBattingAvg)
    if(playerBattingAvg > 30)
    {
      batsmen <- batsmen + 1
    }
    else if(playerBowls > 1200)
    {
      bowlers <- bowlers + 1
    }
    else if(playerBattingAvg > 20 && playerBowls > 800)
    {
      allrounders <- allrounders + 1
    }
    else
    {
      bowlers <- bowlers + 1
    }  
  }
  print(allrounders)
  print(batsmen)
  print(bowlers)
  return(c(batsmen,bowlers,allrounders))
}

get_player_id <- function(player_name){
  query_name <- gsub(" ","+",player_name)
  query_url <- gsub("<<key>>",query_name,search_url)
  #player_id_link <- read_html(query_url) %>% html_node('#gurusearch_player a.statsLinks') %>% html_attr('href') 
  player_id_link <- read_html(query_url) %>% html_nodes('#gurusearch_player a.statsLinks') %>% html_attr('href') 
  player_id <- regmatches(player_id_link,regexec("player/(.*?).html",player_id_link))[[2]][2]
  
}

get_team_id <- function(team_name){
  temp_team_ids <- read.csv("team_ids.csv")
  temp_team_id <- temp_team_ids%>%filter(Team == team_name)%>%select(Id)
  # print(temp_team_id)
}

get_host_name_from_id <- function(host_id){
  temp_team_ids <- read.csv("team_ids.csv")
  temp_team_name <- temp_team_ids%>%filter(Id == host_id)%>%select(Team)
  return(temp_team_name)
}

get_active_team_players <- function(team_id){
  base_url <- 'http://www.espncricinfo.com/ci/content/player/index.html?country=<<team_id>>'
  base_url <- gsub("<<team_id>>",team_id,base_url)
  players_list = read_html(base_url)%>%html_nodes('div#rectPlyr_Playerlistodi table.playersTable tr td a')%>%html_text()
  #print(unique(players_list))
}

get_venue_countries <- function(){
  country_list <- as.list(venue_data %>% select(Country)%>%distinct())
  return(country_list)
}

get_stadiums_in_country <-function(country){
  result = venue_data%>%filter(Country == country)%>%select(Name,Location)
  return(as.array(result$Name))
}

download_stadium_data <- function(stadium_name){
  stadium_name <- trimws(strsplit(stadium_name, "[(]")[[1]][1])
  stadium_base_url <- "http://stats.espncricinfo.com"
  staidum_search_url <- "http://stats.espncricinfo.com/ci/engine/stats/analysis.html?search=<<stadium_name>>;template=analysis"
  stadium_name_encoded <- curlEscape(stadium_name)
  staidum_search_url <- gsub("<<stadium_name>>",stadium_name_encoded,staidum_search_url)
  
  stadiums <- read_html(staidum_search_url)%>%html_nodes("div#gurusearch_ground table tr")
  stadium_data <- data.frame()
  for(row in stadiums){
    name <- row %>% html_nodes('td')%>%.[1]%>%html_text()
    if(name == "") next
    else{
      name = strsplit(name, "[,]")[[1]][1]
      if(name == stadium_name){
        staidum_stats_url = row %>% html_nodes('td')%>%.[3]%>%html_nodes('a.statsLinks')%>%.[2]%>%html_attr("href")
        staidum_stats_url = paste(staidum_stats_url,";spanmax1=08+May+2018;spanmin1=08+May+2013;spanval1=span;template=results;view=innings",sep="")
        staidum_stats_url = paste(stadium_base_url,staidum_stats_url,sep = "")
        data <- read_html(staidum_stats_url)%>%html_nodes("table.engineTable")%>%.[4]%>%html_table()
        stadium_data <- data.frame(data)
        break
      }
    }
  }
  #stadium_data$Wickets <- lapply(stadium_data$Score, function (x) as.numeric(strsplit(x, "[/]")[[1]][2]))
  #stadium_data$Score <- lapply(stadium_data$Score, function (x) as.numeric(strsplit(x, "[/]")[[1]][1]))
  stadium_data$Var.8 <- NULL
  print(stadium_data)
  if(!dir.exists("./venue_data/")){
    dir.create("./venue_data/")
  }
  file_name <- "stadium_data.csv"
  file_path <- paste('./venue_data/',file_name,sep = "")
  print(file_path)
  #sapply(stadium_data,class)
  #unlist(stadium_data$Score)
  #unlist(stadium_data$Wickets)
  write.csv(stadium_data,file = file_path)
}




ui <- fluidPage (

  titlePanel("              Cricket Analytics Dashboard            "),
  sidebarLayout(
    sidebarPanel(
   wellPanel(
          selectInput(inputId = "country", label= "Select Host Country", selected = NULL,get_venue_countries()),
          selectInput(inputId = "venue", label = "Select Stadium ", selected = NULL , c("Not Selected" = "Not Selected")),
         # selectInput(inputId = "matchType", label = "Select match type ", selected = NULL , c("Not Selected" = "Not Selected","Day" = "day", "Day & Night" = "day & night"))
  
         selectInput(inputId = "teamA", label = "Select Team ", selected = NULL , c("Not Selected" = "0","Australia"="2","Bangladesh"="25","England"="1","India"="6","New Zealand"="5","Pakistan"="7","South Africa"="3","Sri Lanka"="8","West Indies"="4","Zimbabwe"="9","Afghanistan"="40","Hong Kong"="19","Ireland"="29","Papua New Guinea"="20","Scotland"="30","UAE"="27")),
         selectInput(inputId = "teamAPlayer", label = "Select Team Players ", selected = NULL , c("Not Selected" = "Not Selected"),multiple = TRUE),
         
         selectInput(inputId = "teamB", label = "Select Opposing Team", selected = NULL , c("Not Selected" = "0","Australia"="2","Bangladesh"="25","England"="1","India"="6","New Zealand"="5","Pakistan"="7","South Africa"="3","Sri Lanka"="8","West Indies"="4","Zimbabwe"="9","Afghanistan"="40","Hong Kong"="19","Ireland"="29","Papua New Guinea"="20","Scotland"="30","UAE"="27")),
         selectInput(inputId = "teamBPlayer", label = "Select Opposing Team Players ", selected = NULL , c("Not Selected" = "Not Selected"),multiple = TRUE),
         
        selectInput("tossResult","Outcome of Toss", choices = c("Not Selected" = "3","Won"="1","Lost"="0"),selected = NULL),
        selectInput("strategy","Outcome after Toss", choices = c("Not Selected" = "3","Bat"="1","Field"="0"),selected = NULL),
        selectInput("homeGround","Home ground", choices = c("Not Selected" = "0","TRUE"="1","FALSE"="2"),selected = NULL),
        numericInput("score","Current Score", value = 0),
        numericInput("wickets","Current Wickets", value = 0),
        numericInput("windSpeed","Enter wind speed", value = 22),
        numericInput("temperature","Enter Temperature", value = 25),
        numericInput("humidity","Enter Humidity",value = 50),
        numericInput("windDirection", "Enter wind direction", value = 105),
        actionButton("submit","Submit")
  )    
    ),
  
  mainPanel(
    tabsetPanel(type="tab",
                tabPanel("Match Result Probability",verbatimTextOutput("Result")),
                tabPanel("Match Plots" , plotOutput("TeamAPlot"),
                         plotOutput("TeamBPlot"),
                         plotOutput("MutualTeamPlot"))
    )
)  
)
)
server <- function(input, output, session) {
  print("Server Start")

 
  
    observeEvent(input$country, {
      updateSelectInput(session, "venue", label = "Select Stadium", choices = get_stadiums_in_country(input$country))
    })
  
    observeEvent(input$teamA, {
      updateSelectInput(session, "teamAPlayer", label = "Select Team Players", choices = get_active_team_players(input$teamA))
    })
    
    observeEvent(input$teamB, {
      updateSelectInput(session, "teamBPlayer", label = "Select Team Players", choices = get_active_team_players(input$teamB))
    })
    
    observeEvent(input$submit, { output$Result <- renderText({
      print("inside func")
      Predict_df <- data.frame(total_batsmen = numeric(), total_bowlers = numeric(), total_allrounders = numeric(),WindSpeed = numeric(), Temperature = numeric(), Humidity = numeric(), WindDirectionDegree = numeric(), score.1 = numeric(), wickets = numeric(), TeamName_V = numeric(), Opposition_V = numeric(), ground_V = numeric(), result_V = factor(), toss_result_V = numeric(), strategy_V = numeric(), home_ground_V = numeric())
      playersClass <- classify_players(input$teamAPlayer)
      #Predict_df <- rbind(Predict_df,c(WindSpeed = input$WindSpeed,Temperature = input$temperature,Humidity = input$humidity,WindDirectionDegree = input$windDirection,score.1 = input$score,wickets = input$wickets,TeamName_V = input$teamA,Opposition_V = input$teamB,ground_V = 3,result_V = 1,toss_result_V = input$tossResult,strategy_V = input$strategy,home_ground_V = input$homeGround))
      #Predict_df[nrow(Predict_df)+1,] <- c(1,1,1,1,1,1,1,1,1,1,1,1,1)
      #print(playersClass[1])
      #print(playersClass[2])
      #print(playersClass[3])
      print(input$venue)
      str(groundMap)
      groundValue <- as.numeric(groundMap[which(groundMap$ground == input$venue),2])
     # groundValue <- as.numeric(groundMap[which(groundMap$ground == "Eden Gardens"),2])
      print(groundValue)
      Predict_df[nrow(Predict_df)+1,] <-c(as.numeric(playersClass[1]),as.numeric(playersClass[2]),as.numeric(playersClass[3]),as.numeric(input$windSpeed),as.numeric(input$temperature),as.numeric(input$humidity),as.numeric(input$windDirection),as.numeric(input$score),as.numeric(input$wickets),as.numeric(input$teamA),as.numeric(input$teamB),as.numeric(groundValue),as.numeric(1),as.numeric(input$tossResult), as.numeric(input$strategy),as.numeric(input$homeGround)) 
      print(Predict_df)
      print(str(Predict_df))
      print(str(CricketData))
      get_MatchPrediction(Predict_df)
      })})
    
    observeEvent(input$submit, { 
      groundsValue <- as.numeric(groundMap[which(groundMap$ground == input$venue),2])
      print("TeamA")
      print(TeamNameData[which(TeamNameData$CrickInfo == input$teamA),2])
      TeamAMatches <- subset(CricketData,CricketData$TeamName_V == as.numeric(TeamNameData[which(TeamNameData$CrickInfo == input$teamA),2]) & CricketData$ground_V == groundsValue)
      TeamAMatches$result_V <- as.numeric(TeamAMatches$result_V)
      print(TeamAMatches)
      
      print("TeamB")
      print(OppositionData[which(OppositionData$CricInfo == input$teamB),2])
      TeamBMatches <- subset(CricketData,CricketData$TeamName_V == as.numeric(OppositionData[which(OppositionData$CricInfo == input$teamB),2]) & CricketData$ground_V == groundsValue)
      TeamBMatches$result_V <- as.numeric(TeamBMatches$result_V)
      print(TeamBMatches)
      
      print("MutualTeam")
      print(TeamNameData[which(TeamNameData$CrickInfo == input$teamA),2])
      TeamMutualMatches <- subset(CricketData,CricketData$TeamName_V == as.numeric(TeamNameData[which(TeamNameData$CrickInfo == input$teamA),2]) & CricketData$Opposition_V == as.numeric(OppositionData[which(OppositionData$CricInfo == input$teamB),2]) & CricketData$ground_V == groundsValue)
      TeamMutualMatches$result_V <- as.numeric(TeamMutualMatches$result_V)
      print(TeamMutualMatches)
      
      
   
      
      output$TeamAPlot <- renderPlot({
      hist(TeamAMatches$result_V,xlab = "Win = 1 Loss = 0", ylab = "Number of Matches", main = "Team Performance", breaks = 3, freq = TRUE)
        #ggplot(TeamAMatches, aes(x=TeamAMatches$result_V)) + geom_bar(aes(fill = TeamAMatches$result_V))+ labs(title = "Ground", x = "Win = 1 Loss = 0", y = "Number of Matches") 
    })
      output$TeamBPlot <- renderPlot({
        hist(TeamBMatches$result_V,xlab = "Win = 1 Loss = 0", ylab = "Number of Matches", main = "Opposing Team Performance", breaks = 3, freq = TRUE)
        #ggplot(TeamAMatches, aes(x=TeamAMatches$result_V)) + geom_bar(aes(fill = TeamAMatches$result_V))+ labs(title = "Ground", x = "Win = 1 Loss = 0", y = "Number of Matches") 
      })
      output$MutualTeamPlot <- renderPlot({
        hist(TeamMutualMatches$result_V,xlab = "Win = 1 Loss = 0", ylab = "Number of Matches", main = "Selected Team VS Opposing Team Performance", breaks = 3, freq = TRUE)
        #ggplot(TeamAMatches, aes(x=TeamAMatches$result_V)) + geom_bar(aes(fill = TeamAMatches$result_V))+ labs(title = "Ground", x = "Win = 1 Loss = 0", y = "Number of Matches") 
      })
      
    })
    
    print("Server End")
}

shinyApp(ui, server)
