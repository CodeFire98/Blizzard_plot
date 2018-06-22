library(shiny)
library(ggplot2)
library(reshape)
library(scales)

source("queplot.R")

ui <- fluidPage(
  titlePanel("Plots"),
  sidebarPanel(
    h3("Constraints (Between 2012 and 2016)"),
    dateInput("date1", "Start Date", value = "2012-01-28"),
    dateInput("date2", "End Date", value = "2012-01-28"),
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

server <- function(input, output) {
  mytable = iig_bharati
  observeEvent(input$submit, {
    output$d = renderText({
      paste(input$date1, " to ", input$date2)
    })
    output$view = renderPlot({
      #queplot(input$date1, input$date2, input$valtempr, input$valrh, input$valws, input$valap)
      queplotuser(input$date1, input$date2, input$valtempr, input$valrh, input$valws, input$valap)
    })
  })
}

shinyApp(ui, server)
runApp(host="172.27.11.219",port=7200)
