
# Load Libraries
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
library(smwrData)
library(readr)
library(tidyr)
library(rmarkdown)
library(shinycssloaders)

# Custom color palette
iisc_palette <- c(
  primary = "#005A9C",
  secondary = "#00A86B",
  accent = "#4FC3F7",
  background = "#F0F4F8",
  text = "#2C3E50"
)

# WQI Calculation Function
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
  
  csv_path <- file.path(getwd(), "water_quality_output.csv")
  write.csv(st_drop_geometry(water_sf), csv_path, row.names = FALSE)
  return(water_sf)
}

ui <- dashboardPage(
  dashboardHeader(title = "Ground Water Quality Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Upload GPKG", tabName = "upload", icon = icon("upload")),
      menuItem("Map", tabName = "map", icon = icon("map")),
      menuItem("Piper Plot", tabName = "piper", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "upload",
              fileInput("gpkg_upload", "Upload GPKG file", accept = ".gpkg"),
              actionButton("load_data", "Load and Calculate WQI"),
              verbatimTextOutput("file_info")
      ),
      tabItem(tabName = "map",
              leafletOutput("map", height = 600)
      ),
      tabItem(tabName = "piper",
              plotOutput("piper_plot", height = "700px")
      )
    )
  )
)

server <- function(input, output, session) {
  data_storage <- reactiveValues(sf_data = NULL, csv_data = NULL)
  
  observeEvent(input$load_data, {
    req(input$gpkg_upload)
    tryCatch({
      water_sf <- calculate_wqi(input$gpkg_upload$datapath)
      data_storage$sf_data <- water_sf
      data_storage$csv_data <- st_drop_geometry(water_sf)
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  output$file_info <- renderPrint({
    req(data_storage$csv_data)
    summary(data_storage$csv_data)
  })
  
  output$map <- renderLeaflet({
    req(data_storage$sf_data)
    pal <- colorFactor(terrain.colors(5), domain = data_storage$sf_data$Quality)
    leaflet(data_storage$sf_data) %>%
      addTiles() %>%
      addCircleMarkers(radius = 4, color = ~pal(Quality), label = ~as.character(Quality)) %>%
      addLegend("bottomright", pal = pal, values = ~Quality, title = "Water Quality")
  })
  
  output$piper_plot <- renderPlot({
    data(MiscGW)
    PD <- transform(MiscGW,
                    Ca.meq = conc2meq(Calcium, "calcium"),
                    Mg.meq = conc2meq(Magnesium, "magnesium"),
                    Na.meq = conc2meq(Sodium, "sodium"),
                    Cl.meq = conc2meq(Chloride, "chloride"),
                    SO4.meq = conc2meq(Sulfate, "sulfate"),
                    HCO3.meq = conc2meq(Bicarbonate, "bicarb"))
    PD$SS <- row.names(PD)
    piperPlot(PD$Ca.meq, PD$Mg.meq, PD$Na.meq,
              PD$Cl.meq, PD$HCO3.meq, PD$SO4.meq,
              Plot = list(name = PD$SS, color = setColor(PD$SS)),
              zCat.title = "Sodium",
              xAn.title = "Chloride",
              yAn.title = "Bicarbonate")
  })
}

shinyApp(ui, server)

