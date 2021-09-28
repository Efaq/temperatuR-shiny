#'INPUTS
#'
#'actionButton
#'submitButton
#'checkboxInput
#'checkboxGroupInput
#'dateInput
#'dateRangeInput
#'fileInput
#'numericInput
#'passwordInput
#'radioButtons
#'selectInput
#'sliderInput
#'textInput
#'
#'OUTPUTS
#'
#'dataTableOutput
#'htmlOutput
#'imageOutput
#'plotOutput
#'tableOutput
#'textOutput
#'uiOutput
#'verbatimTextOutput
#'
#'RENDERS
#'
#'renderDataTable
#'renderImage
#'renderPlot
#'renderPrint
#'renderTable
#'renderText
#'renderUI
#'
#'CALL
#'
#'runGitHub("Efaq/temperatuR-shiny", ref = "main")
#'
devtools::install_github("https://github.com/Efaq/temperatuR")
library(temperatuR)
library(shiny)
agent = temperaturNuAgent()
agent$getInfoStations()

vari = 2

ui <- fluidPage(
  
  sliderInput(
    inputId = "num",
    label = "Choose a number",
    value = 25,
    min = 1,
    max = 100
  ),
  selectInput(
    inputId = "selection",
    label = "Make a selection",
    choices = c("1", "2", "3")
  ),
  plotOutput(
    outputId = "plot"
    
  )

)

server <- function(input, output) {
  print("rerunning this")
  output$plot = renderPlot(
    {
      print(vari)
      vari <<- vari + 1
      title = "random normal variables"
      hist(rnorm(input$num), main = title)
    }
  )
  
}

shinyApp(ui = ui, server = server)