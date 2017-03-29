library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(),
  dashboardBody(),
  dashboardSidebar()
)
server <- function(input,output) {}
shinyApp(ui=ui, server=server)