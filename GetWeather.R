#getwd()
#setwd('D:/DataScienceWorkspace/R-Workspace/ShinyCricket/weatherData')

library('jsonlite')

source("GetStadium.R")

apiKey <- 'b603ed65ef0f4dab81291814182605'
apikey2 <- 'ff0e63e3627f4c839d294806180106'


# location <-'Bangalore, India'
# date <- '2015-09-12'
# preparedUrl <- paste('http://api.worldweatheronline.com/premium/v1/past-weather.ashx?key=',apiKey,'&q=',location,'&format=json&date=',date,sep = '')
# print(preparedUrl)
# stadium_weather <- fromJSON(URLencode(preparedUrl))
# 
# HourlyData <- as.data.frame(stadium_weather$data$weather$hourly)
# 
# print(HourlyData)
# str(HourlyData)
# HourlyData$windspeedKmph <- as.numeric(HourlyData$windspeedKmph)
# 
# HourlyData <- transform(HourlyData, windspeedKmph = as.numeric(windspeedKmph))
# 
# avgSpeed <- mean(as.integer(HourlyData$windspeedKmph))
# print(avgSpeed)
# 2015-09-12
# print(as.character(IndiaMatches_df[1,][6])) as.
# print(IndiaMatches_df[1,][6])
# 
# print(IndiaMatches_df[1,])
# # x <- print(IndiaMatches_df[1,][4])
# x <- strptime('4-Jun-17',"%d-%b-%y")
# print(x)
# betterDate <- as.Date(x,format ="%y-%m-%d")
# 
# print(betterDate)
# HourlyData$windspeedKmph

# str(windDirVector2)
# windDirVector2 <- as.factor(windDirVector2)
# windDirVector2 <- as.vector(HourlyData$winddir16Point)
# tt <- table(windDirVector2)
# names(windDirVector2[which.max(tt)])
# 
# summary(windDirVector2)
# 
# MaxwindDirVector2 <- sort(table(windDirVector2),decreasing = TRUE)[1:1]
# 
# 
# 
# maxd <- as.character(MaxwindDirVector2)

getWeatherData <-function(fileSource){
  getStadiumData()
  Stadium_df <- read.csv('StadiumData.csv', header = TRUE, sep = '|')
  IndiaMatches_df <- read.csv(fileSource, header =  TRUE, sep = '|')
  weatherData_df <- data.frame(MatchID = character(), WindDirectionDegree = character(), windDirection = character(), WindSpeed = character(), Humidity = character(), Temperature = character())
  locationVector <- as.vector(IndiaMatches_df$ground)
  dateVector <- as.vector(IndiaMatches_df$date)
  matchIDVector <- as.vector(IndiaMatches_df$odi_id)
  
  for (i in 1:nrow(IndiaMatches_df)) {
    print(i)
    match_id <- matchIDVector[i]
    matchLocation <- locationVector[i]
    if(matchLocation == 'Fatullah')
    {
      matchLocation <- 'Narayanganj'
    }
    matchDate <- as.Date(strptime(dateVector[i],'%d-%b-%y'),format = '%y-%m-%d')
    print(matchLocation)
    print(matchDate)
    preparedUrl <- paste('http://api.worldweatheronline.com/premium/v1/past-weather.ashx?key=',apikey2,'&q=',matchLocation,'&format=json&date=',matchDate,sep = '')
    stadium_weather <- fromJSON(URLencode(preparedUrl))
    HourlyData <- as.data.frame(stadium_weather$data$weather$hourly)
    HourlyData <- transform(HourlyData, windspeedKmph = as.numeric(windspeedKmph), tempC = as.numeric(tempC), humidity = as.numeric(humidity))
    average_windspeed <- mean(as.integer(HourlyData$windspeedKmph))
    average_TempC <- mean(as.integer(HourlyData$tempC))
    average_humidity <- mean(as.integer(HourlyData$humidity))
    average_windDirDegree <- mean(as.integer(HourlyData$winddirDegree))
    weatherData_df <- rbind(weatherData_df, data.frame(MatchID = match_id, WindSpeed = average_windspeed, Temperature = average_TempC, Humidity = average_humidity, WindDirectionDegree = average_windDirDegree))
  }
  
  IndianMatches_df_new <- cbind(IndiaMatches_df,weatherData_df)
  
  write.csv(IndianMatches_df_new, file='India.csv')
  
}



