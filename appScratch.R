library(shiny)
library(leaflet)
library(sf)
library(terra)
library(dplyr)
library(lubridate)
library(glue)

# --- Helper functions ---

source("./util/currYrFireHelperFunc.R")

get_eddi_date <- function(date, scale = "02wk", cache_dir = tempdir()) {
  year_str <- format(date, "%Y")
  yyyymmdd <- format(date, "%Y%m%d")
  
  valid_scales <- c("01wk", "02wk", "03wk", "01mn", "02mn", "03mn", "06mn", "09mn", "12mn", "24mn")
  if (!scale %in% valid_scales) {
    stop(glue::glue("Invalid scale '{scale}'. Must be one of: {paste(valid_scales, collapse = ', ')}"))
  }
  
  fname <- glue::glue("EDDI_ETrs_{scale}_{yyyymmdd}.asc")
  url <- glue::glue("https://downloads.psl.noaa.gov/Projects/EDDI/CONUS_archive/data/{year_str}/{fname}")
  local_path <- file.path(cache_dir, fname)
  
  # Try local file first
  if (file.exists(local_path)) {
    r <- tryCatch({
      terra::rast(local_path)
    }, error = function(e) NULL)
    if (!is.null(r)) {
      terra::crs(r) <- "EPSG:4326"
      return(r)
    }
  }
  
  # If not cached, try downloading
  tryCatch({
    download.file(url, local_path, mode = "wb", quiet = TRUE)
    r <- terra::rast(local_path)
    terra::crs(r) <- "EPSG:4326"
    return(r)
  }, error = function(e) {
    message(glue::glue("❌ Failed to load EDDI for {date} ({scale}): {e$message}"))
    return(NULL)
  })
}

# --- Load Reanalysis Data ---
hgt_500 <- get_current_year_reanalysis(year = format(Sys.Date(), "%Y"), level_index = 6)  # 500mb
hgt_700 <- get_current_year_reanalysis(year = format(Sys.Date(), "%Y"), level_index = 4)  # 700mb
hgt_850 <- get_current_year_reanalysis(year = format(Sys.Date(), "%Y"), level_index = 3)  # 850mb

# --- Load Fire Data ---
wfigs_sf <- get_wfigs_data() |>
  mutate(DiscoveryDate = as.Date(as_datetime(FireDiscoveryDateTime / 1000)))

reanalysis_dates <- as.Date(time(hgt_500))  # same for 700mb
latest_valid_date <- max(reanalysis_dates, na.rm = TRUE)
earliest_valid_date <- min(reanalysis_dates, na.rm = TRUE)

# --- UI ---
ui <- fluidPage(
  titlePanel(
    div(
      h2("Synoptic Fire Weather Viewer"),
      tags$h5("Visualizing upper-air patterns, evaporative demand, and active fire locations across CONUS")
    )
  ),
  sidebarLayout(
    sidebarPanel(
      h4("Select Date:"),
      fluidRow(
        column(2, actionButton("prev_date", "←")),
        column(8, dateInput("map_date", "Date:",
                            value = latest_valid_date,
                            min = earliest_valid_date,
                            max = latest_valid_date)),
        column(2, actionButton("next_date", "→"))
      ),
      selectInput("level", "Pressure Level:", choices = c("500 mb" = "500", "700 mb" = "700","850 mb" = "850")),
      selectInput("eddi_scale", "EDDI Timescale:", 
                  choices = c("1-week" = "01wk", "2-week" = "02wk", "1-month" = "01mn", 
                              "2-month" = "02mn", "3-month" = "03mn", "6-month" = "06mn"),
                  selected = "02wk"),
      # Add right after titlePanel(...)
      wellPanel(
        HTML("
    <h4>About This Tool</h4>
    <p>
      This interactive application provides a daily snapshot of current year fire weather and drought conditions across the contiguous United States (CONUS).
      It integrates three key data sources:
    </p>
    <ul>
      <li><strong>Geopotential Height Fields</strong> (500 mb and 700 mb): 
          Daily reanalysis data from the <a href='https://psl.noaa.gov/data/gridded/data.ncep.reanalysis2.html' target='_blank'>NOAA NCEP/DOE Reanalysis II Project</a>, used to visualize synoptic-scale atmospheric patterns.</li>
      <li><strong>Evaporative Demand Drought Index (EDDI)</strong>: 
          Sub-seasonal drought signals calculated by NOAA's Physical Sciences Laboratory, available at multiple timescales (weekly to multi-month) via 
          <a href='https://psl.noaa.gov/eddi' target='_blank'>EDDI archive</a>.</li>
      <li><strong>Wildfire Incident Locations</strong>: 
          Near-real-time fire discovery data from the <a href='https://data-nifc.opendata.arcgis.com/datasets/nifc::2025-wildland-fire-incident-locations-to-date/about' target='_blank'>WFIGS dataset</a> (Wildland Fire Interagency Geospatial Services).</li>
    </ul>
    <p>
      Select a date (limited to the current calendar year) to visualize corresponding fire activity, atmospheric heights, and drought indicators. Use the sidebar to toggle pressure levels and EDDI timescales.
    </p>
    <p style='font-size: 90%; color: gray;'>
      <strong>EDDI Legend Notes:</strong><br>
      <em>ED</em> = Evaporative Drought (dry anomalies)<br>
      <em>EW</em> = Evaporative Wetness (wet anomalies)<br>
      Percentile breakpoints represent: 100% = driest; 0% = wettest
    </p>
      <hr>
    <p style='font-size: 90%; color: gray;'>
      Developed by <strong>Mike Crimmins</strong> | University of Arizona Climate Science Applications Program<br>
      Contact: <a href='mailto:crimmins@arizona.edu'>crimmins@arizona.edu</a>
    </p>
    <p style='font-size: 90%; color: gray;'>This tool is <strong>experimental</strong> only and may contain delays or artifacts.</p>

  ")
      )
    ),
    mainPanel(
      leafletOutput("map", height = "700px")
    )
  ),
  
)

# --- Server ---
server <- function(input, output, session) {
  
  observeEvent(input$prev_date, {
    new_date <- input$map_date - 1
    if (new_date >= earliest_valid_date) {
      updateDateInput(session, "map_date", value = new_date)
    }
  })
  
  observeEvent(input$next_date, {
    new_date <- input$map_date + 1
    if (new_date <= latest_valid_date) {
      updateDateInput(session, "map_date", value = new_date)
    }
  })
  
  selected_raster <- reactive({
    date_idx <- which(reanalysis_dates == input$map_date)
    if (length(date_idx) == 1) {
      if (input$level == "500") {
        hgt_500[[date_idx]]
      } else if (input$level == "700") {
        hgt_700[[date_idx]]
      } else if (input$level == "850") {
        hgt_850[[date_idx]]
      }
    } else {
      NULL
    }
  })
  
  eddi_raster <- reactive({
    get_eddi_date(input$map_date, scale = input$eddi_scale)
  })
  
  filtered_fires <- reactive({
    wfigs_sf |>
      filter(DiscoveryDate == input$map_date)
  })
  
  output$map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$CartoDB.Positron) |>
      #fitBounds(-125, 25, -66, 49)
      setView(lng = -98.5, lat = 39.8, zoom = 5) 
  })
  
  observe({
    req(selected_raster())
    req(filtered_fires())
    req(eddi_raster())
    
    # Reanalysis raster + contours
    r <- selected_raster()
    v <- values(r)
    #contour_levels <- pretty(v, 10)
    contour_levels <- seq(floor(min(v, na.rm = TRUE)),
                          ceiling(max(v, na.rm = TRUE)),
                          by = 30)  # Change 30 to something finer like 20 or 10 if needed
    contours <- as.contour(r, levels = contour_levels)
    contour_pal <- colorNumeric("viridis", domain = contour_levels)
    
    # EDDI raster
    eddi <- eddi_raster()
    #eddi_pal <- colorNumeric("RdYlBu", domain = c(-5, 5), reverse = TRUE)
    
    eddi_bins <- c(-Inf, -2, -1.6, -1.3, -1.0, -0.8, 0.8, 1.0, 1.3, 1.6, 2, Inf)
    eddi_colors <- c(
      "#08306B",  # EW4
      "#2171B5",  # EW3
      "#4292C6",  # EW2
      "#6BAED6",  # EW1
      "#9ECAE1",  # EW0
      "#F7F7F7",  # Normal
      "#FDD0A2",  # ED0
      "#FDAE6B",  # ED1
      "#F16913",  # ED2
      "#D94801",  # ED3
      "#8C2D04"   # ED4
    )
    eddi_labels <- c(
      "EW4 (0-2%)", "EW3 (2-5%)", "EW2 (5-10%)", "EW1 (10-20%)", "EW0 (20-30%)",
      "Normal (30-70%)",
      "ED0 (70-80%)", "ED1 (80-90%)", "ED2 (90-95%)", "ED3 (95-98%)", "ED4 (98-100%)"
    )
    
    eddi_pal <- colorBin(
      palette = eddi_colors,
      domain = c(-5, 5),  # Full range of EDDI values
      bins = eddi_bins,
      na.color = "transparent"
    )
    
    
    
    leafletProxy("map") |>
      clearShapes() |>
      clearMarkers() |>
      clearImages() |>
      clearControls() |>
      addPolylines(
        data = contours,
        color = ~contour_pal(level),
        weight = 1.5,
        label = ~paste("Height:", round(level))
      ) |>
      addLegend(
        position = "topright",
        pal = contour_pal,
        values = contour_levels,
        title = glue::glue("GPH {input$level} mb (m)"),
        labFormat = labelFormat(transform = function(x) round(x, 0)),
        opacity = 1
      ) |>
      addCircleMarkers(
        data = filtered_fires(),
        lng = ~InitialLongitude, lat = ~InitialLatitude,
        radius = ~log1p(IncidentSize) * 2,
        fillColor = "red", stroke = FALSE, fillOpacity = 0.7,
        popup = ~paste0("<b>Name:</b> ", IncidentName, "<br>",
                        "<b>Size:</b> ", round(IncidentSize, 1), " acres<br>",
                        "<b>Date:</b> ", DiscoveryDate)
      ) |>
      addRasterImage(
        eddi,
        colors = eddi_pal,
        opacity = 0.5,
        project = TRUE,
        layerId = "eddi_layer"
      ) |>
      addLegend(
        position = "bottomright",
        colors = eddi_colors,
        labels = eddi_labels,
        title = glue("EDDI ({input$eddi_scale})<br><small>Percentile categories</small>"),
        opacity = 1
      )
    
    
  })
}

# --- Launch App ---
shinyApp(ui, server)
