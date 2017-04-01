library(leaflet)
library(dplyr)
library(ggplot2)
library(tidyr)
library(shiny)
library(readxl)
library(shinythemes)
##Mapping <- read_excel("~/Project-Damascus/data/Mapping.xlsx")
library(readr)
##money_right <- read_csv("~/Project-Damascus/data/money-right.csv")
#calling all packages and datasets necessary for app
combined1 <- read.csv(file = "~/Project-Damascus/data/combined1.csv")

hospitals <- data.frame(lat = combined1$lat,
                        lon = combined1$lon,
                        place = combined1$name.x,
                        condition = combined1$DRG.Definition,
                        state = combined1$Provider.State,
                        payment = combined1$Average.Total.Payments,
                        coverage = combined1$Average.Covered.Charges,
                        medicare_pay = combined1$Average.Medicare.Payments,
                        stringsAsFactors = FALSE)

hospitals$labels = paste("Name: ", hospitals$place, "<br>",
                         "Average Payment: $", hospitals$payment, "<br>",
                         "Average Coverage: $", hospitals$coverage, "<br>",
                         "Medicare Coverage: $", hospitals$medicare_pay)

ui <- fluidPage(
  tabsetPanel(
    tabPanel("Map",
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
  )
    ),
  tabPanel("Payment",
    fixedRow(
   column(11, offset = 1, plotOutput(outputId = "bar", height = "800px"),
    tags$p("     "),
    tags$p("     "),
    tags$hr("    "),
    tags$p("     "),
    tags$p("     ")))),
   tabPanel("Coverage",
    fixedRow(
     column(11, offset = 1, plotOutput(outputId = "bar2", height = "800px"),
    tags$p("     "),
    tags$p("     "),
    tags$hr("    "),
    tags$p("     "),
    tags$p("     ")))),
  tabPanel("Medicare",
    fixedRow(
      column(11, offset =1, plotOutput(outputId = "bar3", height = "800px")))
    )
  ))
#this is just creating space for the graph when we make it.
server <- function(input,output) {
  output$bar <- renderPlot({
   combined1 %>%
      filter(DRG.Definition %in% input$condition) %>%
      filter(Provider.State %in% input$state) %>%
      ggplot(aes(name.x, Average.Total.Payments, fill = Provider.Zip.Code)) + 
      geom_bar(stat = "identity") + 
      theme(axis.text.x = element_text(angle = 60, hjust = 1)) + theme(legend.position = "none") + scale_color_brewer(palette = "Greens")
    
  })
  output$bar2 <- renderPlot({
    combined1 %>%
      filter(DRG.Definition %in% input$condition) %>%
      filter(Provider.State %in% input$state) %>%
      ggplot(aes(name.x, Average.Covered.Charges, fill = name.x)) + 
      geom_bar(stat = "identity") + 
      theme(axis.text.x = element_text(angle = 60, hjust = 1)) + theme(legend.position = "none")
  })
  output$bar3 <- renderPlot({
    combined1 %>%
      filter(DRG.Definition %in% input$condition) %>%
      filter(Provider.State %in% input$state) %>%
      ggplot(aes(name.x, Average.Medicare.Payments, fill = name.x)) + 
      geom_bar(stat = "identity") + 
      theme(axis.text.x = element_text(angle = 60, hjust = 1)) + theme(legend.position = "none")
    
  })
  output$map <- renderLeaflet({
    hospitals %>%
      filter(condition %in% input$condition) %>%
      filter(state %in% input$state) %>%
      leaflet() %>% 
      setView(lng = -79.442778, lat = 37.783889, zoom = 1) %>%
      addTiles() %>%
      addCircleMarkers(popup = ~labels, clusterOptions = markerClusterOptions())
  })
}
shinyApp(ui=ui, server=server)