library(shiny)

ui <- fluidPage(
  tags$h1(style = "font-family: Impact", "Hospital Locator"),
  #h1-h6 create decreasingly large headers for the web app. style = font-family: Impact makes different font.
  tags$h2("Find Your Hospital By Selecting Your Condition and Your State"),
  
  "Find Your Hospital By Selecting Your Condition and Your State",
  #You can also add text by putting it in quotes
  
  tags$p("Using the p tag makes paragraphs"),
  tags$p("Which you can see here"),
  
  tags$em("The em tag makes text italicized"),
  tags$strong("The strong tag makes text bold"),
  
  tags$h1(
    tags$strong("Hospital"),
    "Locator"),
  #you can mix tags
  
  tags$br("text"),
  #makes a linebreak
  tags$hr("text"),
  #adds a grey line underneith this line of text
  fluidRow(
    column(2,
  sliderInput(inputId = "num",
              label= "Choose a Number",
              value=25, min =1, max =100)
  )),
  
  tags$a(href= "www.rstudio.com", "R Studio"),
  #a makes a web-link
  
  fluidRow(
    column(10, offset=2,
  plotOutput(outputId = "hist"))
))
server <- function(input,output) {
  output$hist <- renderPlot({
    hist(rnorm(input$num))
    })
    
}
shinyApp(ui=ui, server=server)
#group elements of the ui together using inputPanel, wellPanel(), and they will function as asingle unit
#tabsetPanel(tabPanel(), tabPanel()) will create tabs for web app
#navlistPanel() acts like tabsetPanel() but puts them in a list on the page
#navbarPage() instead of fluidPage() acts like navlistPanel but makes the whole page that way.