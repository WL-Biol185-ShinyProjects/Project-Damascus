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
                        region = combined1$Hospital.Referral.Region.Description,
                        stringsAsFactors = FALSE)

hospitals$labels = paste("Name: ", hospitals$place, "<br>",
                         "Average Payment: $", hospitals$payment, "<br>",
                         "Average Coverage: $", hospitals$coverage, "<br>",
                         "Medicare Coverage: $", hospitals$medicare_pay)

ui <- fluidPage(
  theme = shinytheme("spacelab"),
  navlistPanel(widths = c(3,9),
               tabPanel("Title Page",
                        tags$h2("Welcome to the", tags$h1(tags$strong("Hospital Locator"), align = "center"), align = "center"),
                        tags$p("   "),
                        HTML('<center><img src="Hospital.png" width = "400" height = "400"></center>'),
                        tags$p(tags$h2("Our Mission:", align = "center"),
                               tags$h4("We created this app so that you can become an informed consumer when paying for your healthcare. By selecting your condition and your state, you can see all the hospitals in your area that treat that condition.", align = "center")),
                        tags$p(""),
                        tags$p(tags$h4("Additionally, we have provided information of the average amount a patient pays for their condition at any given hospital, alongwith similar information about insurance coverage and medicare coverage.", align = "center")),
                        tags$p(""),
                        tags$p(tags$h4("If you would like to directly compare two or more hospitals, please click on the", tags$em("Cost Comparison"), "link in the navigation bar.", align = "center"))
               ),
               tabPanel("Map",
                        fluidRow(
                          column(11, offset=1, tags$h1(
                            tags$strong("Hospital Locator")
                          )),
                          column(8,
                                 leafletOutput(outputId = "map", height = "800px")),
                          
                          column(4,
                                 wellPanel(
                                   tags$h4("Please select your", tags$em(tags$strong("Condition")), "and", tags$em(tags$strong("State")), "using the dropdown menus below"),
                                   # Copy the line below to make a select box 
                                   selectInput("condition", label = h3("Condition"),
                                               choices = unique(hospitals$condition),
                                               selected = NULL),
                                   uiOutput("state")
                                 )
                                 
                          ) 
                        )
               ),
               tabPanel("Comparison",
                        fixedRow(
                          column(11, offset = 1,
                                 uiOutput("hospital")),
                          tabsetPanel(
                            tabPanel("Payment",
                                     fixedRow(
                                       column(11, offset = 1, plotOutput(outputId = "bar", height = "800px")))
                            ),
                            tabPanel("Coverage",
                                     fixedRow(
                                       column(11, offset = 1, plotOutput(outputId = "bar2", height = "800px")))
                            ),
                            tabPanel("Medicare",
                                     fixedRow(
                                       column(11, offset =1, plotOutput(outputId = "bar3", height = "800px")))
                            )
                          )
                        ))
  )
)
#this is just creating space for the graph when we make it.
server <- function(input,output) {
  output$bar <- renderPlot({
    combined1 %>%
      filter(DRG.Definition %in% input$condition) %>%
      filter(Provider.State %in% input$state) %>%
      filter(name.x %in% input$hospital) %>%
      ggplot(aes(name.x, Average.Total.Payments, fill = Average.Total.Payments)) + 
      geom_bar(stat = "identity", alpha = 0.8) + 
      theme(axis.text.x = element_text(angle = 60, hjust = 1)) + theme(legend.position = "none")
    
    
  })
  output$bar2 <- renderPlot({
    combined1 %>%
      filter(DRG.Definition %in% input$condition) %>%
      filter(Provider.State %in% input$state) %>%
      filter(name.x %in% input$hospital) %>%
      ggplot(aes(name.x, Average.Covered.Charges, fill = Average.Covered.Charges)) + 
      geom_bar(stat = "identity", alpha = 0.8) + 
      theme(axis.text.x = element_text(angle = 60, hjust = 1)) + theme(legend.position = "none")
  })
  output$bar3 <- renderPlot({
    combined1 %>%
      filter(DRG.Definition %in% input$condition) %>%
      filter(Provider.State %in% input$state) %>%
      filter(name.x %in% input$hospital) %>%
      ggplot(aes(name.x, Average.Medicare.Payments, fill = Average.Medicare.Payments)) + 
      geom_bar(stat = "identity", alpha = 0.8) + 
      theme(axis.text.x = element_text(angle = 60, hjust = 1)) + theme(legend.position = "none")
    
  })
  output$map <- renderLeaflet({
    hospitals %>%
      filter(condition %in% input$condition) %>%
      filter(state %in% input$state) %>%
      leaflet() %>% 
      setView(lng = -99.9018, lat = 41.4925, zoom = 3) %>%
      addTiles(options = tileOptions(noWrap = TRUE)) %>%
      addCircleMarkers(popup = ~labels, clusterOptions = markerClusterOptions())
  })
  output$state <- renderUI({
    selectizeInput("state", label = h3("State"),
                   choices = unique(hospitals$state), "State",
                   selected = NULL, multiple = TRUE)
  })
  output$condition <- renderUI({
    selectInput("condition", label = h3("Condition"),
                choices = unique(hospitals$condition),
                selected = 1)
  })
  output$hospital <- renderUI({
    selectizeInput("hospital", label = h3("Hospital"),
                   choices = unique(hospitals$place[which(hospitals$state == input$state)]), "State",
                   selected = NULL, multiple = TRUE)
  })
}
shinyApp(ui=ui, server=server)