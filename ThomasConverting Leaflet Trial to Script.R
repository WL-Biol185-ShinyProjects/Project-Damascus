library(leaflet)
library(dplyr)
library(ggplot2)
library(tidyr)
library(shiny)
library(readxl)
##Mapping <- read_excel("~/Project-Damascus/data/Mapping.xlsx")
library(readr)
##money_right <- read_csv("~/Project-Damascus/data/money-right.csv")
#calling all packages and datasets necessary for app
##combined <- read.csv("~/Project-Damascus/data/combined.csv")
hospitals <- data.frame(lat = combined$lat,
                        lon = combined$lon,
                        place = combined$name,
                        condition = combined$DRG.Definition,
                        state = combined$Provider.State,
                        payment = combined$Average.Total.Payments,
                        coverage = combined$Average.Covered.Charges,
                        medicare_pay = combined$Average.Medicare.Payments,
                        stringsAsFactors = FALSE)

ui <- fluidPage(
  fluidRow(
    column(11, offset=1, tags$h1(
      tags$strong(style="font-family: Impact", "Hospital Locator")
    )),
    column(8,
           leafletOutput(outputId = "map")),
           
    column(4,
      inputPanel(
        tags$h4("Please select your", tags$em(tags$strong("Condition")), "and", tags$em(tags$strong("State")), "using the dropdown menus below"),
  # Copy the line below to make a select box 
  selectInput("condition", label = h3("Condition"), 
              choices = unique(hospitals$condition), 
              selected = 1),
  
  # Copy the line below to make a select box 
  selectInput("state", label = h3("State"), 
              choices = unique(hospitals$state), 
              selected = 1),
  
  checkboxGroupInput("checkGroup", label = h3("Checkbox group"),
                     choices = list("Average Covered Charges" = 1, "Average Total Payments" = 2, "Average Medicare Payments" = 3),
                     selected = 1),
  
  hr()
)
) 
),
fluidRow(
  plotOutput(outputId = "bar"))
)
#this is just creating space for the graph when we make it.
server <- function(input,output) {
  output$bar <- renderPlot({
    combined %>%
      filter(DRG.Definition %in% input$condition) %>%
      filter(Provider.State %in% input$state) %>%
      ggplot(aes(name, Average.Total.Payments, fill = name)) + geom_bar(stat = "identity") + theme(axis.text.x = element_blank()) 
  })
  output$map <- renderLeaflet({
    hospitals %>%
      filter(condition %in% input$condition) %>%
      filter(state %in% input$state) %>%
      leaflet() %>% 
      setView(lng = -79.442778, lat = 37.783889, zoom = 1) %>%
      addTiles() %>%
      addCircleMarkers(popup = ~place, clusterOptions = markerClusterOptions())
  })
}
  shinyApp(ui=ui, server=server)