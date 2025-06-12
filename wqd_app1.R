##Integrated R Shiny App Code with Piper Plot

# Integrated R Shiny App with GPKG Upload, WQI Calculation, and Piper Plot

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(leaflet)
library(dplyr)
library(magrittr)
library(ggplot2)
library(sf)
library(DT)
library(smwrGraphs)
library(smwrBase)
library(readr)
library(tidyr)
library(rmarkdown)
library(shinycssloaders)

calculate_wqi <- function(gpkg_path) {
  layer_name <- st_layers(gpkg_path)$name[1]
  water_sf <- st_read(gpkg_path, layer = layer_name, quiet = TRUE)
  if (sf::st_crs(water_sf)$epsg != 4326) {
    water_sf <- st_transform(water_sf, 4326)
  }
  if (!all(st_is_valid(water_sf))) {
    water_sf <- st_make_valid(water_sf)
  }
  if ("NA" %in% names(water_sf)) {
    names(water_sf)[names(water_sf) == "NA"] <- "Sodium"
  }
  
  standards <- list(
    TDS = list(St = 1000, Wi = 0.121),
    EC = list(St = 2500, Wi = 0.121),
    NITRATE = list(St = 50, Wi = 0.152),
    SULPHATE = list(St = 250, Wi = 0.121),
    CHLORIDE = list(St = 250, Wi = 0.093),
    BICARBONATE = list(St = 500, Wi = 0.152),
    FLUORIDE = list(St = 1.2, Wi = 0.030),
    CA = list(St = 100, Wi = 0.060),
    MG = list(St = 50, Wi = 0.060),
    Sodium = list(St = 200, Wi = 0.060),
    K = list(St = 20, Wi = 0.030)
  )
  
  water_sf$WQI <- NA_real_
  param_names <- names(standards)
  for (param in param_names) {
    if (!param %in% names(water_sf)) {
      water_sf[[param]] <- 0
    } else {
      water_sf[[param]][is.na(water_sf[[param]])] <- 0
    }
  }
  
  for (param in param_names) {
    qi_col <- paste0("qi_", param)
    sli_col <- paste0("SLi_", param)
    water_sf[[qi_col]] <- water_sf[[param]] / standards[[param]]$St
    water_sf[[sli_col]] <- water_sf[[qi_col]] * standards[[param]]$Wi
  }
  
  sli_cols <- paste0("SLi_", param_names)
  sli_values <- st_drop_geometry(water_sf)[, sli_cols]
  sli_values[] <- lapply(sli_values, as.numeric)
  water_sf$WQI <- rowSums(sli_values, na.rm = TRUE)
  
  water_sf$Quality <- cut(
    water_sf$WQI,
    breaks = c(-Inf, 0.5, 1, 2, 3, Inf),
    labels = c("Excellent", "Good", "Poor", "Very Poor", "Unsuitable"),
    right = FALSE
  )
  
  return(water_sf)
}

ui <- dashboardPage(
  dashboardHeader(title = "Ground Water Assessment Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Upload", tabName = "upload", icon = icon("upload")),
      menuItem("Chemistry", tabName = "chemistry", icon = icon("flask"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "upload",
              fileInput("gpkg_upload", "Upload GPKG", accept = ".gpkg"),
              actionButton("load_data", "Load Data"),
              verbatimTextOutput("file_info")
      ),
      tabItem(tabName = "chemistry",
              withSpinner(plotOutput("piper_plot", height = "700px"), type = 6)
      )
    )
  )
)

server <- function(input, output, session) {
  data_storage <- reactiveValues(csv_data = NULL, sf_data = NULL)
  
  observeEvent(input$load_data, {
    req(input$gpkg_upload)
    water_sf <- calculate_wqi(input$gpkg_upload$datapath)
    data_storage$csv_data <- st_drop_geometry(water_sf)
    data_storage$sf_data <- water_sf
  })
  
  output$file_info <- renderPrint({
    req(data_storage$csv_data)
    cat("Rows:", nrow(data_storage$csv_data), "Columns:", ncol(data_storage$csv_data))
  })
  
  output$piper_plot <- renderPlot({
    req(data_storage$sf_data)
    df <- st_drop_geometry(data_storage$sf_data)
    PD <- df %>%
      mutate(
        Ca.meq = conc2meq(CA, "calcium"),
        Mg.meq = conc2meq(MG, "magnesium"),
        Na.meq = conc2meq(Sodium, "sodium"),
        Cl.meq = conc2meq(CHLORIDE, "chloride"),
        SO4.meq = conc2meq(SULPHATE, "sulfate"),
        HCO3.meq = conc2meq(BICARBONATE, "bicarb"),
        SS = row_number()
      )
    
    piperPlot(PD$Ca.meq, PD$Mg.meq, PD$Na.meq,
              PD$Cl.meq, PD$HCO3.meq, PD$SO4.meq,
              Plot = list(name = PD$SS, color = setColor(PD$SS)),
              zCat.title = "Sodium",
              xAn.title = "Chloride",
              yAn.title = "Bicarbonate")
  })
}

shinyApp(ui, server)


