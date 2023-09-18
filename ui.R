# Install and load the required packages
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(flexdashboard)
library(tidyverse)

# Create a sample data frame with your data
data <- read_csv("data.csv")

# Define the Shiny app UI and server
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
    h3("ResilienceAg! Risk Assesment Platform for Ward, Subcounty, and County"),
    h4("ResilienceAg! is an innovative initiative facilitated by AB Entheos, aimed at revolutionizing agricultural finance and agronomy services. By leveraging climate, vegetative, soil, and field data, it empowers financial institutions to make informed lending decisions to farmers. The ResilienceAg! model integrates GPS location, climate insights, historical yields, and on-field support to create a comprehensive risk profile for farmers, ultimately enabling the design of sustainable financial products. This entails developing a credit scoring engine that also offers farmers a client-facing interface to assess their credit eligibility. The project, incubated within AB Consultants, serves as a pivotal driver in promoting insurance adoption across Africa's agricultural landscape."),
    h3("How to use the Risks Assessment Platform"),
    h4("Risk assessment is carried out utilizing artificial intelligence (AI), which analyzes five years' worth of cumulative historical data. The results are visually represented on a gauge for easy interpretation."),
    h4("The gauge's color will shift in accordance with the scores, spanning from a Bad score (red) to Average: (Orange) and Good (Green)."), 
    
    fluidRow(
      box(
        title = "Gauge for visualising score",
        width = 6,
        selectizeInput("ward_select", "Select Ward", choices = data$Ward),
        gaugeOutput("gauge_chart")
      ),
      box(
        title = "County and Subcounty",
        width = 6,
        textOutput("county_subcounty")
      )
    )
  )
)