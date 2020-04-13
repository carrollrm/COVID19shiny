library(shiny)

shinyUI(fluidPage(
  titlePanel("COVID-19 in North Carolina"),
  sidebarLayout(
    sidebarPanel(
      helpText("Note: Please allow time at launch for the data to load."),
      sliderInput("day", label="Days beginning with March 22, 2020", 
                  min=1, 
                  max=as.numeric(as.Date(Sys.time(),format="%m-%d-%Y")-as.Date("03-23-2020",format="%m-%d-%Y")), 
                  value=1),      
      radioButtons("adj",label="Data Adjustment:",c("None","Percent of Population")),
      radioButtons("loc",label="Location:",c("State","Region")),
      submitButton("Submit")
    ),
    mainPanel(
      textOutput("text"),
      plotOutput("map"),
      plotOutput("barplot"))
	)
      )
    )