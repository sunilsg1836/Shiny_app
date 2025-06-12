library(shiny)
library(tidyverse)
library(ggplot2)

ui <- fluidPage(
  selectInput("species", "Select Species", choices = c("setosa", "versicolor", "virginica")), 
  plotOutput("speciesplot")
)

server <- function(input, output, session) {
  
  speciesdata <- reactive({
    iris %>% 
      filter(Species==input$species)
  })
  output$speciesplot<- renderPlot({
    speciesdata() %>% 
      ggplot(aes(x = Sepal.Length, y= Sepal.Width))+
      geom_point()
    })
  output$speciesplot<- renderTable({
    speciesdata()
  })
}

shinyApp(ui, server)
 
