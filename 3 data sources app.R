library(shiny)
library(ggplot2)
library(reshape)
library(scales)
library(reshape2)

mydozer=Final_File
mybharati=iig_bharati
mymaitri=iig_maitri

#source("queplot.R")
source("allpar.R")

ui <- fluidPage(
  titlePanel("Plots"),
  sidebarPanel(
    h3("Constraints (Between 2007 and 2015) \n"),
    h3("Data Sources"),
    checkboxInput('dozer', 'Dozer_Data: 2007-2015', FALSE),
    checkboxInput('bharati', 'Bharati_Data: 2012-2016', FALSE),
    checkboxInput('maitri', 'Maitri_Data: 2012-2015', FALSE),
    h3("Dates:"),
    dateInput("date1", "Start Date", value = "2007-03-01"),
    dateInput("date2", "End Date", value = "2007-03-01"),
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
server=function(input, output){}

server <- function(input, output) {
  observeEvent(input$submit, {
    output$view = renderPlot({
      if(input$dozer)
      {
        #queplot(input$date1, input$date2, input$valtempr, input$valrh, input$valws, input$valap)
        queplotdozer(input$date1, input$date2, input$valtempr, input$valrh, input$valws, input$valap)
      }
      if(input$bharati)
      {
        #queplot(input$date1, input$date2, input$valtempr, input$valrh, input$valws, input$valap)
        queplotbharati(input$date1, input$date2, input$valtempr, input$valrh, input$valws, input$valap)
      }
      if(input$maitri)
      {
        #queplot(input$date1, input$date2, input$valtempr, input$valrh, input$valws, input$valap)
        queplotmaitri(input$date1, input$date2, input$valtempr, input$valrh, input$valws, input$valap)
      }
    })
  })
}

shinyApp(ui, server)
