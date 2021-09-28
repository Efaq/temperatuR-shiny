devtools::install_github("https://github.com/Efaq/temperatuR")
library(temperatuR)
library(shiny)

agent = temperaturNuAgent()
df_stations = agent$getInfoStations()
sorted_kommun_list = sort(unique(df_stations$kommun))

moving_average = function(input, pivot){
  l = length(input)
  ma = rep(NA, l)
  for (ind in (pivot:l)){
    ma[[ind]] = mean(input[(ind-pivot+1):ind])
  }
  return(ma)
}

ui <- fluidPage(
  plotOutput(outputId = "plot"),
  selectInput(
    inputId = "kommun_selection",
    label = "Select a kommun to search for measurements",
    choices = sorted_kommun_list
  ),
  uiOutput("id_selection_renderer"),
  uiOutput("mov_avrg_pivot_renderer")
)

server <- function(input, output) {
  output$id_selection_renderer = renderUI(
    {
      selectInput(
        inputId = "id_selection",
        label = "Select the id of the station to fetch measurements",
        choices = sort(row.names(df_stations[df_stations$kommun == input$kommun_selection,]))
      )
    }
  )
  output$mov_avrg_pivot_renderer = renderUI(
    {
      sliderInput(
        inputId = "mov_avrg_pivot",
        label = "Moving average parameter",
        min = 1,
        max = 48,
        value = 12
      )
    }
  )
  output$plot = renderPlot({
    title = "How cold was Xmas and New Year?"
    temp_data = agent$getTForStation(input$id_selection,
                                     "2020-12-23-23-50",
                                     "2021-01-02-00-00")
    temp_data = temp_data[!is.na(temp_data$temperatur), ]
    if (nrow(temp_data) > 0) {
      par(mar=c(8, 4, 2, 2) + 0.1)
      plot(temp_data$temperatur, pch=20, axes=FALSE, xlab="", ylab = "Temperature (°C)", main = title)
      lines(temp_data$temperatur)
      conditions = endsWith(temp_data$datetime, "00:00") | endsWith(temp_data$datetime, "12:00")
      axis(1, las=2,
           at = which(conditions),
           labels=temp_data$datetime[conditions])
      axis(2)
      lines(moving_average(temp_data$temperatur, pivot=input$mov_avrg_pivot),
            col = "red")
      
    } else {
      plot(0, 0)
    }
    
  })
  
}

shinyApp(ui = ui, server = server)