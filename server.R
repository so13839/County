# Install and load the required packages
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(flexdashboard)
library(tidyverse)

# Create a sample data frame with your data
data <- read_csv("data.csv")

server <- function(input, output) {
  selected_ward <- reactive({
    data[data$Ward == input$ward_select, ]
  })
  
  output$gauge_chart <- renderGauge({
    value <- selected_ward()$Value
    gauge(min = 0, max = 3, value = value,
          sectors = gaugeSectors(success = c(2.1, 3.0), 
                                 warning = c(1.1, 2.0),
                                 danger = c(0.0, 1.0)))
  })
  
  output$county_subcounty <- renderText({
    paste("County:", selected_ward()$County, ",", "\nSubcounty:", selected_ward()$Subcounty)
    
  })
}

#shinyApp(ui, server)
