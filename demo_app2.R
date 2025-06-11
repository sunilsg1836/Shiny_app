library(shiny)

ui <- fluidPage(
  #inputs
  numericInput("No1", label = "A", value = 0),
  numericInput("No2", label = "B", value = 0),
  numericInput("No3", label = "C", value = 0),
  
  #outputs
  textOutput("sumabc")
  
)

server <- function(input, output, session) {
  output$sumabc <- renderText({
    sum <- input$No1 + input$No2 + input$No3 
    paste("sum is :", sum)
  })
  
}

shinyApp(ui, server)