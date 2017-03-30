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
combined1 <- read.csv(file = "~/Project-Damascus/data/combined1.csv")
combined_sorted <- arrange(combined1, Average.Total.Payments)
combined_ordered <- mutate(combined_sorted,
                           Average.Total.Payments = factor(Average.Total.Payments, levels = Average.Total.Payments, ordered = TRUE))
hospitals <- data.frame(lat = combined_ordered$lat,
                        lon = combined_ordered$lon,
                        place = combined_ordered$name.x,
                        condition = combined_ordered$DRG.Definition,
                        state = combined_ordered$Provider.State,
                        payment = combined_ordered$Average.Total.Payments,
                        coverage = combined_ordered$Average.Covered.Charges,
                        medicare_pay = combined_ordered$Average.Medicare.Payments,
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
fixedRow(
  plotOutput(outputId = "bar"))
)
#this is just creating space for the graph when we make it.
server <- function(input,output) {
  output$bar <- renderPlot({
    combined_ordered %>%
      filter(DRG.Definition %in% input$condition) %>%
      filter(Provider.State %in% input$state) %>%
      ggplot(aes(name.x, Average.Total.Payments, fill = name.x)) + geom_bar(stat = "identity") + theme(axis.text.x = element_blank()) 
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