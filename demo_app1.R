library(shiny)

ui <- fluidPage(
  #inputs
  textInput("id1", label = "Enter name"),
  textInput("id2", label = "Place name"),
  
  #outputs
  textOutput("outid1")
)

server <- function(input, output, session) {
  #render is based on what type of output
  output$outid1 <- renderText({
    paste0(input$id1, "is from ", input$id2)
  })
  
}

shinyApp(ui, server) 