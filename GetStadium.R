#getwd()
#setwd("D:/DataScienceWorkspace/R-Workspace/ShinyCricket")

library('rvest')
library('jsonlite')



getStadiumData <- function(){
  stadium_df <- data.frame(Stadium_Name = character(),city = character(),Country = character(), Stadium_Details = character())
  
  countries <- c('australia','bangladesh','canada','india','ireland','kenya','malaysia','morocco','new-zealand','pakistan','Singapore','south-africa','sri-lanka','united-arab-emirates','united-kingdom','west-indies','zimbabwe')
  
  
  for(i in countries)
  {
    print(i)
    countryUrl <- paste('https://www.worldweatheronline.com/cricket/',i,'.aspx',sep = "")
    print(countryUrl)
    countryWebPage <- read_html(countryUrl)
    stadiums <- countryWebPage %>% html_nodes(".listofcountries")%>% html_node("ul") %>% html_nodes("li")
    for(j in stadiums)
    {
      stadiumDetails <- j %>% html_node("a") %>% html_attr("title")
      print(stadiumDetails)
      StadiumDetailsVector <- strsplit(stadiumDetails,',')[[1]]
      StadiumName <- gsub(' holiday weather','',StadiumDetailsVector[1])
      StadiumCity <- gsub(' holiday weather','',StadiumDetailsVector[2])
      stadium_df <- rbind(stadium_df, data.frame(Stadium_name = StadiumName, City = StadiumCity  , Country = i,Stadium_Details = stadiumDetails ))
    }
  }
  
  write.csv(stadium_df, file = "StadiumData.csv")
}

