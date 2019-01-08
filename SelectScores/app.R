#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(
  selectInput("variable", "Variable:",
               unique(variableNameTable$Main.Tag)),
   tableOutput("data")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$data <- renderTable({
    
    
    Selection = as.character(variableNameTable$Main.Tag) == input$variable
    variableNameTable[Selection, c("Identifiers"), drop = FALSE]
  }, rownames = TRUE)
}

# Run the application 
shinyApp(ui = ui, server = server)

