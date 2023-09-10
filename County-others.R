library(shiny)
library(flexdashboard)
library(readr)
library(shinyWidgets)
#library(echarts4r)

# Sample data frame with location-value mapping
data1 <- read_csv("data1.csv")
data2 <- read_csv("data2.csv")
data3 <- read_csv("data3.csv")

ui <- fluidPage(
  h3("ResilienceAg! Risk Assesment Platform for County, Sub County, and Ward"),
  h4("ResilienceAg! is an innovative initiative facilitated by AB Consultants, aimed at revolutionizing agricultural finance and agronomy services. By leveraging climate, vegetative, soil, and field data, it empowers financial institutions to make informed lending decisions to farmers. The ResilienceAg! model integrates GPS location, climate insights, historical yields, and on-field support to create a comprehensive risk profile for farmers, ultimately enabling the design of sustainable financial products. This entails developing a credit scoring engine that also offers farmers a client-facing interface to assess their credit eligibility. The project, incubated within AB Consultants, serves as a pivotal driver in promoting insurance adoption across Africa's agricultural landscape."),
  h3("How to use the Risks Assessment Platform"),
  h4("Risk assessment can be conducted at different levels, including a County, Sub County, or Ward level, with the findings represented on a gauge."),
  h4("The gauge's color will shift in accordance with the scores, spanning from a Bad score (1: red) to Fair (2: Orange) and Good (3: Green)."), 
  selectInput("County", label = "Asses a risk of a County", choices = data1$County),
  gaugeOutput("gauge1"),
  selectInput("Subcounty", label = "Asses a risk of a Sub County", choices = data2$Subcounty),
  gaugeOutput("gauge2"),
  tags$style(HTML(".selectize-input { color: blue; }")),
  selectInput("Ward", label = "Asses a risk of a Ward", choices = data3$Ward),
  gaugeOutput("gauge3"),
  sidebarLayout(
    sidebarPanel(
      textInput("farmers_input", "Farmer:", value = ""),
      textInput("crop_input", "Crop:", value = " "),
    ),
    mainPanel(
      h3("Optional: Farmer's name and Crop")),
       
    )
  )


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
