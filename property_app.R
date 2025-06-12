library(shiny)
library(tidyverse)
library(janitor)

ui <- fluidPage(
  
  tableOutput("tb1")
  
)

server <- function(input, output, session) {
  
  data1 <- reactive({
    "10_Property_stolen_and_recovered.csv" %>% 
      read_csv() %>% 
      clean_names()
  })
  data2 <- reactive({
    data1() %>% 
      rename("state_ut" = "area_name") 
    
  })
  data3 <- reactive({
    data2() %>% 
      rename(group_name != "Total Property")
    
  })
  output$tb1 <-renderTable({
    data2()
  })
   
}

shinyApp(ui, server)