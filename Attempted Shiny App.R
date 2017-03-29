library(shiny)
library(ggplot2)
library(dplyr)
ui <- fluidPage(
  
  # Copy the line below to make a select box 
  selectInput("condition", label = h3("Condition"), 
              choices = unique(money_right$`DRG Definition`), 
              selected = 1),
  
  # Copy the line below to make a select box 
  selectInput("state", label = h3("State"), 
              choices = unique(money_right$`Provider State`), 
              selected = 1),
  
  checkboxGroupInput("checkGroup", label = h3("Checkbox group"),
                     choices = list("Average Covered Charges" = 1, "Average Total Payments" = 2, "Average Medicare Payments" = 3),
                     selected = 1),
  
  hr(),
  fluidRow(column(3, verbatimTextOutput("value"))),
  
  plotOutput(outputId = "bar")
)

server <- function(input,output) {
  output$bar <- renderPlot({
    money_right %>%
      filter(`DRG Definition` %in% input$condition) %>%
      filter(`Provider State` %in% input$state) %>%
      ggplot(aes(`Provider Name`, `Average Total Payments`, fill = `Provider Name`)) + geom_bar(stat = "identity") + theme(axis.text.x = element_blank()) 
    })
}
shinyApp(ui = ui, server = server)



