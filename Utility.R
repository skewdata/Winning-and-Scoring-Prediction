# contains all utility functions

library(xml2)
library(rvest)
library(stringr)
library(dplyr)
library(RCurl)
library(jsonlite)
library(data.table)
source("GetWeather.R")


stats_base_url <- 'http://stats.espncricinfo.com'
search_url <- paste(stats_base_url,'/ci/engine/stats/analysis.html?search=<<key>>;template=analysis',sep = "")
venue_data <- read.csv("all_venues.csv")
player_roles <- data.frame("player_id" = character(0), "player_role" = character(0))


get_player_id <- function(player_name){
  query_name <- gsub(" ","+",player_name)
  query_url <- gsub("<<key>>",query_name,search_url)
  players <- read_html(query_url)%>%html_nodes('div#gurusearch_player table tr')
  player_id = ""
  for(player_html in players){
    current_player <- player_html%>%html_nodes("td")%>%.[1]%>%html_nodes('span')%>%.[1]%>%html_text()
    if(identical(current_player,player_name)){
      player_id_link <- player_html%>%html_nodes('td')%>%.[3]%>%html_nodes('a')%>%.[1]%>% html_attr('href')
      player_id <- regmatches(player_id_link,regexec("player/(.*?).html",player_id_link))[[1]][2]
      break
    }
  }
  return(player_id)
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
  print(unique(players_list))
}

get_venue_countries <- function(){
  country_list <- as.list(venue_data %>% select(Country)%>%distinct())
  return(country_list)
}

get_stadiums_in_country <-function(country){
  result = venue_data%>%filter(Country == country)%>%select(Name,Location)
  return(as.array(result$Name))
}


scrap_match_data <- function(team_name,match_link,odi_id,venue){
  test_url = match_link
  html <- read_html(test_url)
  model <- data.frame()
  
  # Teams
  # team_a <-html%>%html_nodes('div.cscore_details ul.cscore_competitors li.cscore_item.cscore_item--home div.cscore_team.icon-font-after div.cscore_truncate a span.cscore_name.cscore_name--long')%>%.[1]%>%html_text()
  # team_b <-html%>%html_nodes('div.cscore_details ul.cscore_competitors li.cscore_item.cscore_item--away div.cscore_team.icon-font-after div.cscore_truncate a span.cscore_name.cscore_name--long')%>%.[1]%>%html_text()
  # if(team_a != team_name){
  #   opposition = team_a
  # }else{
  #   opposition = team_b
  # }

  # # Toss Details
  toss_data <- html%>%html_nodes('div.match-detail-container div.match-detail--item')%>%.[2]%>%html_node('div.match-detail--right')%>%html_text()
  team <- strsplit(toss_data," , ")[[1]][1]
  strategy <- strsplit(toss_data," , ")[[1]][2]
  if(team!= team_name){
    toss_result = "Lost"
  }
  else{
    toss_result = "Won"
  }
  if(toss_result == "Lost"){
    if(grepl("field",strategy)){
      strategy = "Bat"
    }else{
      strategy = "Field"
    }
  }else{
    strategy = "Field"
  }
  
  
  # Venue Details (home ground or not)
  # venue_details <- html%>%html_nodes('div.match-detail-container div.stadium-details a span')%>%html_text()
  venue_details <- venue
  # print(venue_details)
  if(grepl(',',venue_details)){
    venue_location <- strsplit(venue_details,", ")[[1]][1] # Querying by stadium name
    stadium_country <- get_stadium_county(venue_location)
    if(length(stadium_country)== 0){
      venue_location <- strsplit(venue_details,", ")[[1]][2] # Querying by stadium city
      stadium_country <- get_stadium_county(venue_location)
      if(length(stadium_country)== 0){
        venue_location <- strsplit(venue_details,", ")[[1]][3]
        if(length(venue_location)!=0 && venue_location != "NA"){
          stadium_country <- get_stadium_county(venue_location)
        }else{
          stadium_country <- "N/A"
        }
      }
    }
  }else{
    venue_location <- venue_details
    stadium_country <- get_stadium_county(venue_location)
  }
  # 
  # #print(venue_location)
  # #stadium_country <- get_stadium_county(venue_location)
  # #print(length(stadium_country))
  if(length(stadium_country)!=0){
    if(stadium_country!= 'N/A'){
      if(stadium_country != team_name) home_ground <- FALSE
      else home_ground <- TRUE
    }else{
      home_ground <- 'N/A'
    }
  }else{
    home_ground <- 'N/A'
  }
  
  # 
  # 
  # #print(home_ground)
  # 
  #Match Timings
  if(grepl("D/N",as.character(html))){
    match_timing = "D/N"
  }else{
    match_timing = "D"
  }
  # 
  # #print(match_timing)
  curr_team_squad <- get_squad(team_name = team_name,match_url = test_url)
  # #opp_team_squad <- get_squad(team_name = opposition,match_url = test_url)
  # 
  # # getting player roles
  total_batsmen <- 0;
  total_bowlers <- 0;
  total_allrounders <- 0;
  for(player in curr_team_squad){
    this_player_id <- get_player_id(player)
    role <- get_player_role(this_player_id)
    if(grepl("batsman",role,ignore.case = TRUE) || grepl("wicket",role,ignore.case = TRUE)){
      total_batsmen = total_batsmen +1
    }else if(grepl("allrounder",role,ignore.case = TRUE)){
      total_allrounders = total_allrounders +1
    }else if(grepl("bowler",role,ignore.case = TRUE)){
      total_bowlers = total_bowlers + 1
    }
  }
  # # #team_name,opposition,toss_result,strategy,match_timing,home_ground,stadium_country , ,total_batsmen,total_bowlers,total_allrounders,bat_bowl_avg$batting_avg,bat_bowl_avg$bowling_avg
  bat_bowl_avg <- get_bat_bowl_avg(team_name,test_url)   # ,match_timing,home_ground,stadium_country,total_batsmen,total_bowlers,total_allrounders,bat_bowl_avg$batting_avg,bat_bowl_avg$bowling_avg
  model <- rbind(model,data.frame(odi_id,toss_result,strategy,match_timing,home_ground,total_batsmen,total_bowlers,total_allrounders,bat_bowl_avg$batting_avg,bat_bowl_avg$bowling_avg))
  return(model)
}
#scrap_match_data("India","http://stats.espncricinfo.com/ci/engine/match/1007649.html")

get_stadium_county <- function(venue_location){
  #print(venue_location)
  if(nrow(venue_data%>%filter(str_detect(Location,venue_location))) == 0){
    country <- venue_data%>%filter(str_detect(Name,venue_location))%>%select(Country)%>%pull()%>%droplevels()
  }else{
    country <- venue_data%>%filter(str_detect(Location,venue_location))%>%select(Country)%>%pull()%>%droplevels()
  }
  return(country)
}

# get_stadium_county()

get_squad <- function(team_name,match_url){
  html <-read_html(match_url)
  article_html <- html%>%html_nodes('article.sub-module.scorecard')
  squad <- c()
  final_squad <- c()
  for(article in article_html) {
     team_scorecard <- article%>%html_nodes('ul.css-accordion li.accordion-item div.accordion-header a h2')%>%html_text()
     if(grepl(team_name,team_scorecard)){
       # retieve batting 
       batsmen_played <- article %>% html_nodes("div.wrap.batsmen div.cell.batsmen")%>%html_text()
       #print(batsmen_played)
       if(length(batsmen_played)<11){
          # check for yet to bat section
         batsmen_dnb <-article%>%html_nodes('div.wrap.dnb a span')%>%html_text()
         #print(batsmen_dnb)
         squad <- c(batsmen_played,batsmen_dnb)
       }else{
         squad <- batsmen_played
       }
       break
     }
  }
  for(player in squad){
    if(grepl(',',player)){
      player <- substr(player,0,regexpr(',',player)-1)
    }
    if(grepl('\\(',player)){ # †
      player <- substr(player,0,regexpr('\\(',player)-1)
    }
    if(grepl('†',player)){
      player <- substr(player,0,regexpr('†',player)-1)
    }
    final_squad <- c(final_squad,trimws(player))
  }
  return(final_squad)
}

#get_squad("India","http://stats.espncricinfo.com/ci/engine/match/1007649.html")

get_bat_bowl_avg <- function(team,match_url){
  html <-read_html(match_url)
  article_html <- html%>%html_nodes('article.sub-module.scorecard')
  batting_avg <- 0.0
  bowling_avg <- 0.0
  bat_bowl_avg <- data.frame()
  for(article in article_html) {
    team_scorecard <- article%>%html_nodes('ul.css-accordion li.accordion-item div.accordion-header a h2')%>%html_text()
    if(grepl(team,team_scorecard)){
      # retieve batting average
       batsmen <- article %>% html_nodes("div.wrap.batsmen")
       count <- 0
       score <- 0
       for(batsman in batsmen){
         batsman_score <- batsman %>% html_nodes("div.cell.runs")%>%.[1]%>%html_text()
         score <- score + as.numeric(batsman_score)
         count <- count +1
       }
       batting_avg <- score/count
    }else{
      #retrieve bowling average
      bowlers <- article %>% html_nodes('div.scorecard-section.bowling table tbody tr')
      wickets <- 0
      runs <- 0
      for(bowler in bowlers){
        runs_conc <- bowler%>%html_nodes('td')%>%.[5]%>%html_text()
        wickets_taken <- bowler%>%html_nodes('td')%>%.[6]%>%html_text()
        runs <- runs + as.numeric(runs_conc)
        wickets <- wickets + as.numeric(wickets_taken)
      }
      bowling_avg <- runs/wickets
    }
  }
  # print(batting_avg)
  # print(bowling_avg)
  return(data.frame(batting_avg,bowling_avg))
}

get_team_match_links <- function(team){
  team_stats_url <-'http://stats.espncricinfo.com/ci/engine/team/<<team_id>>.html?class=2;spanmax1=24+May+2018;spanmin1=24+May+2017;spanval1=span;template=results;type=team;view=innings'
  team_id <- get_team_id(team)
  team_stats_url <-  gsub("<<team_id>>",team_id,team_stats_url)
  links <- read_html(team_stats_url)%>%html_nodes('table.engineTable')%>%.[4]%>%html_nodes('tbody tr a')%>%html_attr("href")
  match_links <- c()
  for(link in links){
    if(grepl('/match/',link)){
      match_links <- c(match_links,link)
    }
  }
  return(match_links)
}

get_player_role <- function(this_player_id){
  this_player_profile_link <- paste(paste("http://www.espncricinfo.com/ci/content/player/",this_player_id,sep=""),".html",sep = "")
  this_player_html <- read_html(this_player_profile_link)
  this_player_role_label <- this_player_html%>%html_nodes('p.ciPlayerinformationtxt')%>%.[5]%>%html_node('b')%>%html_text()
  if(grepl('role',this_player_role_label)){
    this_player_role <- this_player_html%>%html_nodes('p.ciPlayerinformationtxt')%>%.[5]%>%html_node('span')%>%html_text()
  }else{
    this_player_role <- this_player_html%>%html_nodes('p.ciPlayerinformationtxt')%>%.[6]%>%html_node('span')%>%html_text()
  }
  return(this_player_role)
  
}

# get_player_role('236779')

main <- function(team,range_start_str,range_end_str,file_name){
  print(Sys.time())
  team_stats_url <-'http://stats.espncricinfo.com/ci/engine/team/<<team_id>>.html?class=2;spanmax1=<<str_date>>;spanmin1=<<end_date>>;spanval1=span;template=results;type=team;view=innings'
  team_id <- get_team_id(team)
  team_stats_url <-  gsub("<<team_id>>",team_id,team_stats_url)
  team_stats_url <-  gsub("<<str_date>>",range_start_str,team_stats_url)
  team_stats_url <-  gsub("<<end_date>>",range_end_str,team_stats_url)
  team_data <- data.frame()
  team_result_html <- read_html(team_stats_url)
  matches <- team_result_html%>%html_nodes('table.engineTable')%>%.[4]%>%html_nodes('tbody tr')
  index <- 1 
  print(paste("total matches in given range: ",length(matches),sep=""))
  for(match in matches){
    score <- match%>%html_nodes('td')%>%.[1]%>%html_text()
    result <- match%>%html_nodes('td')%>%.[6]%>%html_text()
    date <- match%>%html_nodes('td')%>%.[10]%>%html_node('b')%>%html_text()
    innings <- match%>%html_nodes('td')%>%.[5]%>%html_text()
    ground <- match%>%html_nodes('td')%>%.[9]%>%html_node('a')%>%html_text()
    oppositon <-match%>%html_nodes('td')%>%.[8]%>%html_node('a')%>%html_text()
    odi_id <- match%>%html_nodes('td')%>%.[11]%>%html_node('a')%>%html_text()
    link <- match%>%html_nodes('td')%>%.[11]%>%html_node('a')%>%html_attr("href")
    link <- paste("http://stats.espncricinfo.com",link,sep="")
    match_info <- data.frame(date,oppositon,ground,innings,score,result,odi_id)
    match_data <- scrap_match_data(team,link,odi_id,ground)
    team_data <- rbind(team_data,merge(match_info,match_data))
    print(paste("Done with match# ",index,sep = ""))
    index <- index +1
    #team_data <- cbind(team_data,match_data)
    #print(team_data)
  }
  write.csv(team_data,file = file_name)
  print(paste("successfully created file: ",file_name,sep = ""))
  print(team_data)
  print(Sys.time())
}

merge_data <- function(actual_file,file_name){
  country_model <- NULL
  if(file.exists(actual_file)){
    file_data <- read.csv(file_name)
    country_model <- read.csv(actual_file)[,-1] #omitting unsued index : error number of columns of arguments do not match
    country_model <- rbind(country_model,file_data)
  }else{
    file_data <- read.csv(file_name)
    country_model <- file_data
  }
  
  write.csv(country_model,file=actual_file)
  # if(file.exists(file_name)) {file.remove(file_name)}
}



scrap <- function(country_name){
  actual_file <- paste(country_name,"csv",sep = ".")
  if(file.exists(actual_file)){
    print("destroying existing file. Creating fresh Copy")
    file.remove(actual_file)
  }
  for (i in 1:5){
    curr_date = Sys.Date() ### yyyy-mm-dd
    range_start <- Sys.Date() - 365*(i-1)
    range_start_year <- strsplit(as.character(range_start),"-")[[1]][1]
    range_start_str <- paste(strsplit(as.character(range_start),"-")[[1]][3],paste(month.abb[as.numeric(strsplit(as.character(range_start),"-")[[1]][2])],strsplit(as.character(range_start),"-")[[1]][1],sep = "+"),sep = "+")
    range_end <- Sys.Date() - 356*(i)
    range_end_str <- paste(strsplit(as.character(range_end),"-")[[1]][3],paste(month.abb[as.numeric(strsplit(as.character(range_end),"-")[[1]][2])],strsplit(as.character(range_end),"-")[[1]][1],sep = "+"),sep = "+")
    range_end_year <- strsplit(as.character(range_end),"-")[[1]][1]
    file_name <- paste(paste(range_start_year,range_end_year,sep = "_"),".csv",sep = "")
    #print(paste(range_start_str,range_end_str,sep = "----"))
    #print(file_name)
   
    if(file.exists(file_name)) {
      print(paste(file_name," already exists. Merging and skipping..."))
      merge_data(actual_file,file_name)
      next
    }
    print(paste("creating file: ",file_name,sep = ""))
    main(country_name,range_start_str,range_end_str,file_name)
    merge_data(actual_file,file_name)
    
  }
}

scrap("India")
#main("India")

#merging all the data

# data18_17 <- read.csv("2018_2017.csv")
# data17_16 <- read.csv("2017_2016.csv")
# data16_15 <- read.csv("2016_2015.csv")
# data15_14 <- read.csv("2015_2014.csv")
# data14_13 <- read.csv("2014_2013.csv")
# 
# india_model <- data.frame()
# india_model <- rbind(india_model,data18_17)
# india_model <- rbind(india_model,data17_16)
# india_model <- rbind(india_model,data16_15)
# india_model <- rbind(india_model,data15_14)
# india_model <- rbind(india_model,data14_13)
# 
# write.csv(india_model,file="India.csv")
# 
# getWeatherData("India.csv")




