library(shiny)

ui <- fluidPage(
  sidebarLayout(
   mainPanel(
     plotOutput(outputId = "hist")
   ),
     sidebarPanel(
      sliderInput(inputId = "num",
                label= "Choose a Number",
                value=25, min =1, max =100)
    )
    
  )
)
server <- function(input,output) {
  output$hist <- renderPlot({
    hist(rnorm(input$num))
  })
  
}
shinyApp(ui=ui, server=server)
