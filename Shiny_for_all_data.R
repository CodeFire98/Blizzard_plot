library(shiny)

source("Source File for 7 sources trial.R")
source("db_comm.R")

ui <- fluidPage(
  headerPanel("Plots"),
  sidebarPanel(
    h3("Stations:"),
    radioButtons("choice", "Select a Station:", 
                 c("Bharati"="b",
                   "Maitri"="m")),
    h3("Data Sources:"),
    conditionalPanel(
      condition = "input.choice == 'b'",
      radioButtons("choice1", "Select the Data Source:", 
                   c("IIG"="iigb",
                     "IMD"="imdb")),
      h3("Enter Dates:"),
      conditionalPanel(
        condition = "input.choice1 == 'iigb'",
        dateRangeInput("date1", "Input Date", start = "2012-01-28", end = "2016-12-31", min = "2012-01-28", max = "2016-12-31")
      ),
      conditionalPanel(
        condition = "input.choice1 == 'imdb'",
        dateRangeInput("date2", "Input Date", start = "2012-02-06", end = "2016-11-13", min = "2012-02-06", max = "2016-11-13")
      ),
      h3("Parameters:"),
      checkboxInput('valtemprb', 'Temperature', FALSE),
      checkboxInput('valrhb', 'Humidity', FALSE),
      checkboxInput('valwsb', 'Wind Speed', FALSE),
      checkboxInput('valapb', 'Air Pressure', FALSE)
    ),
    conditionalPanel(
      condition = "input.choice == 'm'",
      radioButtons("choice2", "Select the Data Source:", 
                   c("IIG"="iigm",
                     "IMD"="imdm",
                     "Sankalp SASE"="ssasem",
                     "Dozer SASE"="dozerm",
                     "Surface Data"="ant_tb3m")),
      h3("Enter Dates:"),
      conditionalPanel(
        condition = "input.choice2 == 'iigm'",
        dateRangeInput("date3", "Input Date", start = "2012-01-01", end = "2015-12-31", min = "2012-01-01", max = "2015-12-31")
      ),
      conditionalPanel(
        condition = "input.choice2 == 'imdm'",
        dateRangeInput("date4", "Input Date", start = "1985-01-01", end = "2016-12-19", min = "1985-01-01", max = "2016-12-19")
      ),
      conditionalPanel(
        condition = "input.choice2 == 'ssasem'",
        dateRangeInput("date5", "Input Date", start = "2006-02-23", end = "2016-12-31", min = "2006-02-23", max = "2016-12-31")
      ),
      conditionalPanel(
        condition = "input.choice2 == 'dozerm'",
        dateRangeInput("date6", "Input Date", start = "2007-03-01", end = "2015-11-15", min = "2007-03-01", max = "2015-11-15")
      ),
      conditionalPanel(
        condition = "input.choice2 == 'ant_tb3m'",
        dateRangeInput("date7", "Input Date", start = "1985-02-26", end = "2010-12-31", min = "1985-02-26", max = "2010-12-31"),
        h3("Parameters:"),
        checkboxInput('valtemprant', 'Temperature', FALSE),
        checkboxInput('valwsant', 'Wind Speed', FALSE),
        checkboxInput('valmslpant', 'Mean Sea Level Pressure', FALSE)
      ),
      conditionalPanel( 
        condition = "input.choice2 != 'ant_tb3m'",
        h3("Parameters:"),
        checkboxInput('valtempr', 'Temperature', FALSE),
        checkboxInput('valrh', 'Humidity', FALSE),
        checkboxInput('valws', 'Wind Speed', FALSE),
        checkboxInput('valap', 'Air Pressure', FALSE)
      )
    ),
    actionButton("submit","Submit")
  ),
  mainPanel(
    h2("Time Series Plot\n"),
    plotOutput("view")
  )
)

server <- function(input, output) {
  observeEvent(input$submit, {
    output$view = renderPlot({
      inputter(input$choice, input$choice1, input$choice2, input$valtemprb, input$valrhb, input$valwsb, input$valapb, input$valtemprant, input$valwsant, input$valmslpant, input$valtempr, input$valrh, input$valws, input$valap, input$date1, input$date2, input$date3, input$date4, input$date5, input$date6, input$date7)
    })
  })
  
}

shinyApp(ui, server)
