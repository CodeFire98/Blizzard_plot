library(shiny)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

#source("queplot.R")
source("Parameters-all data sources.R")

ui <- fluidPage(
  titlePanel("Plots"),
  sidebarPanel(
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
    plotOutput("view")
  )
)
server=function(input,output){}

server <- function(input, output) {
  observeEvent(input$submit, {
      output$view = renderPlot({
      inputter(input$date1, input$date2, input$valtempr, input$valrh, input$valws, input$valap, input$iigb, input$iigm, input$imdm, input$ssase, input$dozer)
    })
  })
}

shinyApp(ui, server)

