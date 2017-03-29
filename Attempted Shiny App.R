library(shiny)

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
    filter
    ggplot(aes(`Provider Name`, input$)) + geom_bar(stat = "identity") + theme(axis.text.x = element_blank()) + scale_x_discrete(limit = money_right$`Provider Name`[1:5])
})
    }

shinyApp(ui = ui, server = server)



