library(shiny)
library(shinydashboard)
library(jsonlite)
library(ggvis)
library(dplyr)
library(fontawesome)
library(curl)

ui <- dashboardPage(
  dashboardHeader(title = "花園新城的Weather Report"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      valueBoxOutput("temperatureBox", width = 4),
      valueBoxOutput("humidityBox", width = 4),
      infoBoxOutput("PM25Box", width = 4)
    ),
    fluidRow(
      box(
        title = "Temperature Over Last Seven Days",
        width = 12,
        ggvisOutput("plot1")
      )
    )
  )
)

server <- function(input, output) {
  weather <- fromJSON("http://1a46f20b.ngrok.io/weather/range?start_date=2018-12-20&end_date=2019-01-02")
  
  latest <- tail(weather$data, n = 1)
  
  output$temperatureBox <- renderInfoBox({
    valueBox(
      paste0(latest$temperature, "*C"), "Temperature", icon = icon("sun", lib = "font-awesome"),
      color = "yellow"
    )
  })
  
  output$humidityBox <- renderValueBox({
    valueBox(
      paste0(latest$humidity, "%"), "Relative Humidity", icon = icon("cloud-rain", lib = "font-awesome"),
      color = "blue"
    )
  })
  
  output$PM25Box <- renderValueBox({
    valueBox(
      "15",
      "PM 2.5",
      color = "red",
      icon = icon("biohazard")
    )
  })
  
  weather$data %>% 
    mutate(
      datetime2 = as.POSIXct(strptime(datetime, format = "%Y-%m-%dT%H:%M:%OS"))
    ) %>% 
    ggvis(~datetime2, ~temperature) %>%
    layer_points( opacity := 0.1, fill := "lightgreen") %>% 
    layer_smooths(se = TRUE, span = 0.4, stroke = "lightblue") %>% 
    set_options(width = "auto", height = "auto") %>% 
    bind_shiny("plot1")
  }

shinyApp(ui, server)
