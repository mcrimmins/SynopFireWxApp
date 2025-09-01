# app.R
# -------------------------------------------------------------------
# Synoptic Fire Weather Viewer
# Developed by Mike Crimmins | UArizona Climate Science Applications Program
# Visualizes daily fire activity, upper-air reanalysis (GPH), and EDDI drought indices across CONUS.
# -------------------------------------------------------------------

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
    stop(glue("Invalid scale '{scale}'. Must be one of: {paste(valid_scales, collapse = ', ')}"))
  }
  fname <- glue("EDDI_ETrs_{scale}_{yyyymmdd}.asc")
  url <- glue("https://downloads.psl.noaa.gov/Projects/EDDI/CONUS_archive/data/{year_str}/{fname}")
  local_path <- file.path(cache_dir, fname)
  
  if (file.exists(local_path)) {
    r <- tryCatch(terra::rast(local_path), error = function(e) NULL)
    if (!is.null(r)) {
      terra::crs(r) <- "EPSG:4326"
      return(r)
    }
  }
  
  tryCatch({
    download.file(url, local_path, mode = "wb", quiet = TRUE)
    r <- terra::rast(local_path)
    terra::crs(r) <- "EPSG:4326"
    return(r)
  }, error = function(e) {
    message(glue("❌ Failed to load EDDI for {date} ({scale}): {e$message}"))
    return(NULL)
  })
}

# --- Load Data ---
hgt_500 <- get_current_year_reanalysis(format(Sys.Date(), "%Y"), level_index = 6)
hgt_700 <- get_current_year_reanalysis(format(Sys.Date(), "%Y"), level_index = 4)
hgt_850 <- get_current_year_reanalysis(format(Sys.Date(), "%Y"), level_index = 3)

# load fire data
# wfigs_sf <- get_wfigs_data() |>
#   mutate(DiscoveryDate = as.Date(as_datetime(FireDiscoveryDateTime / 1000)))

# --- Load and cache fire data per session, refresh daily ------
fire_cache <- file.path(tempdir(), "wfigs_cached.rds")

if (!file.exists(fire_cache) || as.Date(file.info(fire_cache)$mtime) < Sys.Date()) {
  message("🔄 Fetching fresh WFIGS fire data...")
  fire_data <- tryCatch({
    get_wfigs_data()
  }, error = function(e) {
    message(glue::glue("⚠️ Failed to fetch WFIGS data: {e$message}"))
    NULL
  })
  
  if (!is.null(fire_data)) {
    saveRDS(fire_data, fire_cache)
  }
}

# Load from cache (even if freshly updated)
wfigs_sf <- tryCatch({
  readRDS(fire_cache) |>
    mutate(DiscoveryDate = as.Date(as_datetime(FireDiscoveryDateTime / 1000)))
}, error = function(e) {
  message(glue::glue("⚠️ Failed to read cached fire data: {e$message}"))
  NULL
})
#####

# get date ranges
reanalysis_dates <- as.Date(time(hgt_500))
fire_dates <- sort(unique(wfigs_sf$DiscoveryDate))
all_dates <- sort(unique(c(reanalysis_dates, fire_dates)))

earliest_valid_date <- min(all_dates, na.rm = TRUE)
latest_valid_date <- max(all_dates, na.rm = TRUE)

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
      selectInput("level", "Pressure Level:",
                  choices = c("500 mb" = "500", "700 mb" = "700", "850 mb" = "850")),
      selectInput("eddi_scale", "EDDI Timescale:", 
                  choices = c("1-week" = "01wk", "2-week" = "02wk", "1-month" = "01mn", 
                              "2-month" = "02mn", "3-month" = "03mn", "6-month" = "06mn"),
                  selected = "02wk"),
      wellPanel(
        HTML("
        <h4>About This Tool</h4>
        <p>This interactive application provides a daily snapshot of current year fire weather and drought conditions across the contiguous United States (CONUS).</p>
        <ul>
          <li><strong>Geopotential Height Fields</strong>: Upper-air patterns at 500/700/850 mb from the 
            <a href='https://psl.noaa.gov/data/gridded/data.ncep.reanalysis2.html' target='_blank'>NOAA NCEP/DOE Reanalysis II</a>.</li>
          <li><strong>Evaporative Demand Drought Index (EDDI)</strong>: Sub-seasonal drought signals from 
            <a href='https://psl.noaa.gov/eddi' target='_blank'>NOAA/PSL</a>, available at multiple timescales.</li>
          <li><strong>Wildfire Locations</strong>: Incident data from the 
            <a href='https://data-nifc.opendata.arcgis.com/' target='_blank'>WFIGS dataset</a>.</li>
        </ul>
        <p style='font-size: 90%; color: gray;'>
          <strong>EDDI Notes:</strong><br>
          <em>ED</em> = Evaporative Drought (dry anomalies), <em>EW</em> = Evaporative Wetness (wet anomalies)<br>
          Percentile scale: 100% = driest, 0% = wettest
        </p>
        <hr>
        <p style='font-size: 90%; color: gray;'>
          Developed by <strong>Mike Crimmins</strong> | UArizona CSAP<br>
          Contact: <a href='mailto:crimmins@arizona.edu'>crimmins@arizona.edu</a>
        </p>
        <p style='font-size: 90%; color: gray;'>Experimental tool. Data may contain delays or artifacts.</p>
        ")
      )
    ),
    mainPanel(
      leafletOutput("map", height = "700px"),
      uiOutput("warnings_ui")
    )
  )
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
      switch(input$level,
             "500" = hgt_500[[date_idx]],
             "700" = hgt_700[[date_idx]],
             "850" = hgt_850[[date_idx]],
             NULL)
    } else {
      NULL
    }
  })
  
  eddi_raster <- reactive({
    get_eddi_date(input$map_date, scale = input$eddi_scale)
  })
  
  filtered_fires <- reactive({
    wfigs_sf |> filter(DiscoveryDate == input$map_date)
  })
  
  output$map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(lng = -98.5, lat = 39.8, zoom = 5)
  })
  
  output$warnings_ui <- renderUI({
    msgs <- c()
    if (is.null(selected_raster())) {
      msgs <- c(msgs, "⚠️ Reanalysis data (GPH) is not available for this date.")
    }
    if (is.null(eddi_raster())) {
      msgs <- c(msgs, "⚠️ EDDI data is not available for this date and timescale.")
    }
    if (length(msgs) > 0) {
      div(style = "margin-top: 10px; color: #a94442; background-color: #f2dede; border: 1px solid #ebccd1; padding: 10px; border-radius: 5px;",
          HTML(paste(msgs, collapse = "<br>")))
    } else {
      NULL
    }
  })
  
  observe({
    req(filtered_fires())
    
    leafletProxy("map") |>
      clearShapes() |>
      clearMarkers() |>
      clearImages() |>
      clearControls()
    
    if (!is.null(selected_raster())) {
      r <- selected_raster()
      v <- values(r)
      contour_levels <- seq(floor(min(v, na.rm = TRUE)), ceiling(max(v, na.rm = TRUE)), by = 30)
      contours <- as.contour(r, levels = contour_levels)
      contour_pal <- colorNumeric("viridis", domain = contour_levels)
      
      leafletProxy("map") |>
        addPolylines(data = contours, color = ~contour_pal(level), weight = 1.5,
                     label = ~paste("Height:", round(level))) |>
        addLegend(position = "topright", pal = contour_pal, values = contour_levels,
                  title = glue("GPH {input$level} mb (m)"), opacity = 1)
    }
    
    if (!is.null(eddi_raster())) {
      eddi <- eddi_raster()
      eddi_bins <- c(-Inf, -2, -1.6, -1.3, -1.0, -0.8, 0.8, 1.0, 1.3, 1.6, 2, Inf)
      eddi_colors <- c("#08306B", "#2171B5", "#4292C6", "#6BAED6", "#9ECAE1", "#F7F7F7",
                       "#FDD0A2", "#FDAE6B", "#F16913", "#D94801", "#8C2D04")
      eddi_labels <- c("EW4 (0-2%)", "EW3 (2-5%)", "EW2 (5-10%)", "EW1 (10-20%)", "EW0 (20-30%)",
                       "Normal (30-70%)",
                       "ED0 (70-80%)", "ED1 (80-90%)", "ED2 (90-95%)", "ED3 (95-98%)", "ED4 (98-100%)")
      eddi_pal <- colorBin(palette = eddi_colors, domain = c(-5, 5), bins = eddi_bins, na.color = "transparent")
      
      leafletProxy("map") |>
        addRasterImage(eddi, colors = eddi_pal, opacity = 0.5, project = TRUE, layerId = "eddi_layer") |>
        addLegend(position = "bottomright", colors = eddi_colors, labels = eddi_labels,
                  title = glue("EDDI ({input$eddi_scale})<br><small>Percentile categories</small>"),
                  opacity = 1)
    }
    
    leafletProxy("map") |>
      addCircleMarkers(data = filtered_fires(),
                       lng = ~InitialLongitude, lat = ~InitialLatitude,
                       radius = ~log1p(IncidentSize) * 2,
                       fillColor = "red", stroke = FALSE, fillOpacity = 0.7,
                       popup = ~paste0("<b>Name:</b> ", IncidentName, "<br>",
                                       "<b>Size:</b> ", round(IncidentSize, 1), " acres<br>",
                                       "<b>Date:</b> ", DiscoveryDate))
  })
}

# --- Launch App ---
shinyApp(ui, server)
