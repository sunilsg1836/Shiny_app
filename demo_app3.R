library(shiny)

ui <- fluidPage(
  #inputs
  numericInput("No1", label = "A", value = 0),
  numericInput("No2", label = "B", value = 0),
  numericInput("No3", label = "C", value = 0),
  
  #outputs
  textOutput("multabc")
  
)

server <- function(input, output, session) {
  output$multabc <- renderText({
    mult <- input$No1 * input$No2 * input$No3 
    paste("value is :", mult)
  })
  
}

shinyApp(ui, server) 