library(shiny)

library(shiny)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        font-family: 'Times New Roman', Times, serif;
      }
    "))
  ),
  tags$h1("Welcome To Shiny World", style = "text-align: center;"),
  fluidRow(
    column(6, h1("What is Shiny?")),
    column(2, h1("Text 2")),
    column(2, h1("Text 3")),
    column(2, h1("Text 4 "))
  ),
  fluidRow(
    column(6, h3("Shiny is an R package that makes it easy to build interactive web applications (apps) straight from R. This lesson will get you started building Shiny apps right away.")),
    column(2, h3("Shiny is an R package that makes it easy to build interactive web applications (apps) straight from R. This lesson will get you started building Shiny apps right away.")),
    column(2, h3("Shiny is an R package that makes it easy to build interactive web applications (apps) straight from R. This lesson will get you started building Shiny apps right away.")),
    column(2, h3("Shiny is an R package that makes it easy to build interactive web applications (apps) straight from R. This lesson will get you started building Shiny apps right away. "))
  )
) 

server <- function(input, output, session) {
  
}

shinyApp(ui, server)