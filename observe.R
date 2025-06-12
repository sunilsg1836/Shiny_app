##Observe 
library(shiny)

ui <- fluidPage(
  numericInput("num", "Enter a number:", 5),
  verbatimTextOutput("result")
)

server <- function(input, output, session) {
  observe({
    print(paste("Number input changed to:", input$num))
  })
  
  output$result <- renderText({
    paste("You entered:", input$num)
  })
}

shinyApp(ui,server)

##observeEvent

library(shiny)

ui <- fluidPage(
  numericInput("num", "Enter a number:", 5),
  actionButton("go", "Show Message"),
  verbatimTextOutput("message")
)

server <- function(input, output, session) {
  msg <- reactiveVal("")
  
  observeEvent(input$go, {
    msg(paste("You entered:", input$num))
  })
  
  output$message <- renderText({
    msg()
  })
}

shinyApp(ui,server)

