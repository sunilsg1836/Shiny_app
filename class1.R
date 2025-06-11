library(shiny)

ui <- fluidPage(
  h1("Hello World"),
  h2("Hello World"),
  h3("Hello World"),
  h4("Hello World"),
  p("This is a paragraph,
    shiny is a package to build appliacations")
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)




