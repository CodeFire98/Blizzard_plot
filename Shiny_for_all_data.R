library(shiny)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

#source("queplot.R")
source("Parameters.R")

ui <- fluidPage(
  titlePanel("Plots"),
  sidebarPanel(
    h3("Data Sources:"),
    radioButtons("choice", "Select a Data Source:", 
                 c("IIG_Bharati (2012-2016)"="iigb",
                   "IIG_Maitri (2012-2015)"="iigm",
                   "IMD_Maitri (1985-2016)"="imdm",
                   "Sankalp_Sase (2006-2015)"="ssase",
                   "Dozer_Sase (2007-2015)"="dozer")),
    h3("Enter Dates:"),
    dateInput("date1", "Start Date"),
    dateInput("date2", "End Date"),
    h3("Parameters:"),
    checkboxInput('valtempr', 'Temperature', FALSE),
    checkboxInput('valrh', 'Humidity', FALSE),
    checkboxInput('valws', 'Wind Speed', FALSE),
    checkboxInput('valap', 'Air Pressure', FALSE),
    actionButton("submit","Submit")
  ),
  mainPanel(
    h2("Time Series Plot\n"),
    plotOutput("view"),
    verbatimTextOutput("d")
  )
)
#server=function(input,output){}

server <- function(input, output) {
  observeEvent(input$submit, {
    output$d = renderText({
      paste(input$choice, typeof(input$choice), input$valtempr, input$valrh, input$date1)
    })
    output$view = renderPlot({
      inputter(input$date1, input$date2, input$valtempr, input$valrh, input$valws, input$valap, input$choice)
    })
  })
}

shinyApp(ui, server)
