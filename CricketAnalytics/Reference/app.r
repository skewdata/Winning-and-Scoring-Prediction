library(shiny)


ui=fluidPage(
  
  titlePanel(h1("~~~~~~~~~~~~~~~~~~~~~~~CRICKET ANALYSIS~~~~~~~~~~~~~~~~~~~~~~", align="center")),
  sidebarLayout(
    sidebarPanel(
   #   radioButtons("type","select match type", c("T20","ONE DAY","TEST")),
      selectInput("Team1","Please select 1st team", choices = c("India","Australia","Pakistan","Afganistan","South Africa","New Zealand","England","Sri lanka"),selected = NULL),
      uiOutput("vx"),
      selectInput("Team2","Please select 2nd team", choices = c("Australia","India","Pakistan","Afganistan","South Africa","New Zealand","England","Sri lanka"),selected = NULL),
      
      uiOutput("vy"),
      selectInput("toss","Please select toss result", c("Not selected","Won","Lost")),
      selectInput("strtgy","Please select strategy result", c("Not selected","Field","Bat")),
      selectInput("grndtyp","Please select ground type", c("Not selected","TRUE","FALSE")),
      selectInput("homegrnd","Match is getting played in homegroung??", c("Not selected","TRUE","FALSE")),
      textInput("ground","Please Enter Ground"),
      numericInput("temp","Please Enter Temperature", value = 25),
      numericInput("wspeed","Please Enter Wind speed ",value = 10),
      numericInput("humidt","Please Enter Humidiy ", value = 50),
      numericInput("winddir","Please Enter wind direction in degree ", value = 150),
      
      submitButton("Submit"),
      tags$style("body{background-color:linen;color:brown}"),
      tags$style("radio{font-style:oblique;border-style:solid}")
    ),
    mainPanel(
      tabsetPanel(type="tab",
                  tabPanel("Match",h1(textOutput("result"))),
                  tabPanel("structure", verbatimTextOutput("strc")),
                  tabPanel("data",tableOutput("data1")),
                  tabPanel("plot" , plotOutput("out"))
      )
      
      
    )
  )
  
  
  
)
server=shinyServer(function(input,output)
  
{
  var=reactive({
    switch(input$Team1,
      "India"=c("Virat kohli(c & b)","MS Dhoni(Wk & b)","Rohit Sharma(b)","Dhawan(b)","Suresh raina(b)","KL Rahul(b)","Bhubaneswar kumar(bowl)","Bumrah(Bowl)","Yugvendra(bowl)","Jadega(ALL)","Ashwin(bowl)"),
      "Pakistan"=c("shahid(b)","hafeez(bowl)","mallick(b)"),
      "Australia"=c("smith","warner","watson")
    )
    
    
  })
  
  output$vx=renderUI(
    {
      selectInput("variablex","Select players for your team",choices = var(),multiple = T)
    }
  )
 
  file1=read.csv("C:/Users/Lenovo/Desktop/study/stats/shiny/cric.csv")
  output$result=renderText(input$ground
    
  )
  
  

  
}
  
  
  
  )
shinyApp(ui,server)