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
    conditionalPanel(
      condition = "input.choice == 'iigb'",
      dateRangeInput("date", "Input Date", start = "2012-01-28", end = "2012-01-28", min = "2012-01-28", max = "2016-12-31")
    ),
    conditionalPanel(
      condition = "input.choice == 'iigm'",
      dateRangeInput("date", "Input Date", start = "2012-01-01", end = "2012-01-01", min = "2012-01-01", max = "2015-12-31")
    ),
    conditionalPanel(
      condition = "input.choice == 'imdm'",
      dateRangeInput("date", "Input Date", start = "1985-01-01", end = "1985-01-01", min = "1985-01-01", max = "2016-12-19")
    ),
    conditionalPanel(
      condition = "input.choice == 'ssase'",
      dateRangeInput("date", "Input Date", start = "2006-02-23", end = "2006-02-23", min = "2006-02-23", max = "2016-12-31")
    ),
    conditionalPanel(
      condition = "input.choice == 'dozer'",
      dateRangeInput("date", "Input Date", start = "2007-03-01", end = "2007-03-01", min = "2007-03-01", max = "2015-11-15")
    ),
    h3("Parameters:"),
    checkboxInput('valtempr', 'Temperature', FALSE),
    checkboxInput('valrh', 'Humidity', FALSE),
    checkboxInput('valws', 'Wind Speed', FALSE),
    checkboxInput('valap', 'Air Pressure', FALSE),
    actionButton("submit","Submit")
  ),
  mainPanel(
    h2("Time Series Plot\n"),
    plotOutput("view")
  )
)
#server=function(input,output){}

server <- function(input, output) {
  observeEvent(input$submit, {
    output$view = renderPlot({
      inputter(input$date, input$valtempr, input$valrh, input$valws, input$valap, input$choice)
    })
  })
}

shinyApp(ui, server)
