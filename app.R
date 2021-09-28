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

#Boilerplate
devtools::install_github("https://github.com/Efaq/temperatuR")
library(temperatuR)
library(shiny)
agent = temperaturNuAgent()
df_stations = agent$getInfoStations()
sorted_kommun_list = sort(unique(df_stations$kommun))



ui <- fluidPage(
  selectInput(
    inputId = "kommun_selection",
    label = "Select the kommun where the measurement station is placed",
    choices = sorted_kommun_list
  ),
  uiOutput("id_selection"),
  plotOutput(outputId = "plot")
  
)

server <- function(input, output) {
  output$id_selection = renderUI({
    selectInput(
      inputId = "id_selection",
      label = "Select the id of the station to fetch measurements",
      choices = sort(row.names(df_stations[df_stations$kommun == input$kommun_selection,]))
    )
  })
  output$plot = renderPlot({
    title = "How was Xmas and New Year?"
    temp_data = agent$getTForStation(input$id_selection,
                                     "2020-12-23-00-00",
                                     "2021-01-02-00-00")
    temp_data = temp_data[!is.na(temp_data$temperatur), ]
    if (nrow(temp_data) > 0) {
      plot(temp_data$temperatur)
    } else {
      plot(0, 0)
    }
    
  })
  
}

shinyApp(ui = ui, server = server)