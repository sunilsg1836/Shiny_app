library(shiny)
library(tidyverse)
library(janitor)
library(shiny)
library(DT)

ui <- fluidPage(
  dataTableOutput("yr_tr")
)

server <- function(input, output, session) {
  
  data1<- reactive({
    "city_day.csv" %>% 
      read_csv() %>% 
      clean_names()
  })
  
  output$yr_tr <-renderDT({
   datatable(data1())
  })
  
  }

shinyApp(ui, server)

 
