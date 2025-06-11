library(shiny)

ui <- fluidPage(
  titlePanel("Sidebar layout"),
  sidebarLayout(
    sidebarPanel(
      h1("T1")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Menu",
                 column(6, h3("Shiny is an R package that makes it easy to build interactive web applications (apps) straight from R. This lesson will get you started building Shiny apps right away.")),
                 column(2, h3("Shiny is an R package that makes it easy to build interactive web applications (apps) straight from R. This lesson will get you started building Shiny apps right away.")),
                 column(2, h3("Shiny is an R package that makes it easy to build interactive web applications (apps) straight from R. This lesson will get you started building Shiny apps right away.")),
                 column(2, h3("Shiny is an R package that makes it easy to build interactive web applications (apps) straight from R. This lesson will get you started building Shiny apps right away."))
        ),
        tabPanel("Data",
                 fluidRow(
                   box(title = "Sample Plot", width = 12, status = "success",
                       plotOutput("myplot"))
                 )),
        tabPanel("Contact",
                 column(6, h3("Shiny is an R package that makes it easy to build interactive web applications (apps) straight from R. This lesson will get you started building Shiny apps right away.")),
                 column(2, h3("Shiny is an R package that makes it easy to build interactive web applications (apps) straight from R. This lesson will get you started building Shiny apps right away.")),
                 column(2, h3("Shiny is an R package that makes it easy to build interactive web applications (apps) straight from R. This lesson will get you started building Shiny apps right away.")),
                 column(2, h3("Shiny is an R package that makes it easy to build interactive web applications (apps) straight from R. This lesson will get you started building Shiny apps right away."))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  output$myplot <- renderPlot({
    hist(mtcars$mpg, main = "Histogram of MPG", col = "steelblue", border = "white")
  
})}

shinyApp(ui, server)