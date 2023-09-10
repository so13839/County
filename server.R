library(shiny)
library(flexdashboard)
library(readr)
library(shinyWidgets)
#library(echarts4r)

# Sample data frame with location-value mapping
data1 <- read_csv("data1.csv")
data2 <- read_csv("data2.csv")
data3 <- read_csv("data3.csv")


server <- function(input, output) {
  selected_county <- reactive({
    subset(data1, County == input$County)$Value
  })
  
  output$gauge1 <- renderGauge({
    gauge(min = 0, 
          max = 3, 
          sectors = gaugeSectors(success = c(2.1, 3.0), 
                                 warning = c(1.1, 2.0),
                                 danger = c(0.0, 1.0)),
          value = selected_county())
  })
  
  selected_subcounty <- reactive({
    subset(data2, Subcounty == input$Subcounty)$Value
  })
  
  output$gauge2 <- renderGauge({
    gauge(min = 0, 
          max = 3, 
          sectors = gaugeSectors(success = c(2.1, 3.0), 
                                 warning = c(1.1, 2.0),
                                 danger = c(0.0, 1.0)),
          value = selected_subcounty())
  })
  
  selected_ward <- reactive({
    subset(data3, Ward == input$Ward)$Value
  })
  
  output$gauge3 <- renderGauge({
    gauge(min = 0, 
          max = 3, 
          sectors = gaugeSectors(success = c(2.1, 3.0), 
                                 warning = c(1.1, 2.0),
                                 danger = c(0.0, 1.0)),
          value = selected_ward())
  })
}

shinyApp(ui = ui, server = server)
