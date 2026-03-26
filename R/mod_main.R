#' main UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import shinyjs
#' @import shinyFiles
#' @import leaflet

options(shiny.maxRequestSize = 2000 * 1024^2)

mod_main_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    shinyjs::useShinyjs(),
    
    # Included inline CSS to fix Browse button
    tags$head(
      tags$style(HTML("
        .btn-file {  
          background-color: #00688B; 
          border-color: black;
          color: white !important;
        }
        .input-group-btn .btn:hover {
          background-color: lightskyblue !important;
          transform: scale(1.05);
        }
        .modal-confirm-btn {
          transition: all 0.2s ease !important;
        }
        .modal-confirm-btn:hover {
          background-color: lightskyblue !important;
          transform: translateY(-2px) !important;
          box-shadow: 0 4px 8px rgba(0,0,0,0.2) !important;
        }
        .input-section {
          border: 2px solid black; 
          padding: 10px; 
          margin-bottom: 15px; 
          margin-left: 10px;
          border-radius: 5px;
          background-color: #F8F8F8;
        }
        .custom-btn {
          background-color: #00688B !important;
          color: white !important;
          border-color: black !important;
        }
        .custom-btn:hover {
          background-color: lightskyblue !important; 
          transform: scale(1.05);
        }
        .example-btn {
          background-color: #CD950C !important;   /* named CSS color */
          color: white !important;                      /* contrast text */
          border: 1px solid black !important;
        }
        .load-example-btn:hover {
          background-color: #FFD700 !important;
          color: black !important;
          transform: scale(1.05);
        }
        .success-text {
          color: darkgreen;
          font-weight: bold;
          margin-top: 10px;
        }
        .map-container {
          border: 2px solid black; 
          border-radius: 5px;
          padding: 5px;
          background-color: #F8F8F8;
          margin-top: 15px;
          margin-left: 10px;
        }
        .map-toggle-btn {
          width: 100%;
          margin-top: 5px;
          background-color: #00688B;
          color: white;
          border: 1px solid black;
          border-radius: 5px;
          padding: 5px;
        }
        .map-toggle-btn:hover {
          background-color: lightskyblue;
        }
        .shiny-input-container {
          margin-bottom: 5px !important;
        }
      "))
    ),

    ##############################################################
    # SIDE BAR DATA INPUTS AND BUTTONS
    ##############################################################
    tags$div(
      id = ns("sidebar"),
      class = "input-section",
      # h4(tags$span(shiny::icon("file-upload"), " Select Data File:"), style = "color: black;"),
      # div(style = "width: 100%;",
      #     fileInput(ns("data"), NULL, width = "100%", accept = c(".rds"))  # File input
      # ),
      
      h4(tags$span(shiny::icon("users"), " Client Access"), 
         style = "color: black;"),
      
      textInput(
        ns("client_id"),
        label = "Enter Client ID:",
        placeholder = "e.g. ID_12345"
      ),
      div(
        style = "display: flex; width: 100%; align-items: center; gap: 10px;",
        actionButton(ns("submit_id"), "Access Files", icon = shiny::icon("folder-open"), 
                     class = "custom-btn", style = "flex-grow: 1;"),
        # Spinner shown while loading
        div(
          id = ns("client_spinner"),
          style = "display: none;",
          tags$span(
            class = "spinner-border spinner-border-sm text-secondary",
            role  = "status",
            style = "width: 1.2rem; height: 1.2rem;"
          )
        )
      ),
      tags$hr(style = "border-top: 2px solid black; margin-top: 15px; margin-bottom: 15px;"),
      
      h4(tags$span(shiny::icon("file-import"), "Import Data:"), style = "color: black;"), 
      div(style = "display: flex; width: 100%;",  
          fileInput(ns("zip_file"), "Upload a Zip File:", accept = ".zip")
      ),
      
      #shiny::verbatimTextOutput(ns("directory")),
      
      # Submit button with loading spinner
      div(
        style = "display: flex; width: 100%;",
        actionButton(ns("submit_files"), "Load Files", icon = shiny::icon("folder-open"), class = "custom-btn", style = "flex-grow: 1;")
      ),
      shiny::h6(
        "Note: Files up to 2 GB are supported.",
        style = "margin-top: 0px; margin-bottom: 10px; font-size: 0.82rem; color: #888; text-align: left;"
      ),
      
      textOutput(ns("load_status")),
      #br(),
      # Horizontal black line
      tags$hr(style = "border-top: 2px solid black; margin-top: 15px; margin-bottom: 15px;"),
      
      # "Use Example Data" button
      div(
        style = "display: flex; width: 100%;",
        actionButton(
          ns("load_example"), 
          "Use Example Data", 
          icon = icon("flask"), 
          style = "
            flex-grow: 1;
            background-color: #CDAD00; 
            border: 1px solid black; 
            color: white;
          ",
          class = "load-example-btn"
        )
      )
    ),
    
    ##############################################################
    # LEAFLET MAP 
    ##############################################################
    div(
      id = ns("map_container"),
      class = "map-container",
      leaflet::leafletOutput(ns("map"), height = "300px"),
      actionButton(ns("expand_map"), "Expand Map", icon = shiny::icon("map"), class = "map-toggle-btn")
    ),
    div(
      style = "margin-top: 10px; font-size: 0.85rem; color: #333; text-align: center;",
      HTML('Please send any questions or issues to <u>jstephens@oceanscienceanalytics.com</u>')
    )
  )
}
    
#' main Server Functions
#'
#' @noRd 
mod_main_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    load_rds <- function(name) {
      get_rds(
        name            = name,
        rds_paths       = data$rds_paths(),
        rds_cache_val   = data$rds_cache(),
        update_cache_fn = data$rds_cache
      )
    }
    
##############################################################
# DIRECTORY OUTPUT LOGIC
##############################################################
    selected_dir <- reactiveVal(NULL)
    rds_names <- reactiveVal(NULL)
    rds_paths    <- reactiveVal(NULL)   # named vec: name -> file path
    rds_cache    <- reactiveVal(list()) # loads into memory only when selected
    selected_rds <- reactiveVal(NULL)
    #rds_data <- reactiveVal(NULL)
    acoustic_names <- reactiveVal(NULL)
    acoustic_file_tree <- reactiveVal(NULL)
    soundscape_data <- reactiveVal(NULL)
    click_detector_data <-reactiveVal(NULL)
    use_example <- reactiveVal(FALSE)
    
    # volumes <- c(
    #   Home = fs::path_home(),
    #   Downloads = fs::path_home("Downloads"),
    #   #"C Drive" = "C:/",
    #   #"D Drive" = "D:/",
    #   Root = "/"
    # )
    # 
    # # Enable directory selection
    # shinyFiles::shinyDirChoose(
    #   input = input,
    #   id = "dir",
    #   roots = volumes,
    #   session = session,
    #   allowDirCreate = FALSE
    # )
    # 
    # # Selected directory reactive
    # selected_dir <- shiny::reactive({
    #   shiny::req(input$dir)
    #   shinyFiles::parseDirPath(volumes, input$dir)
    # })
    # 
    # # Display selected directory path
    # output$directory <- shiny::renderPrint({
    #   if(length(selected_dir()) == 0) {
    #     "No directory selected"
    #   } else {
    #     selected_dir()
    #   }
    # })
    
    
##############################################################
# LEAFLET MAP LOGIC
##############################################################
    
    location_data <- reactive({
      
      root_path <- selected_dir()
      shiny::req(root_path)
      
      spatial_file <- file.path(root_path, "Metadata.csv")
      
      if (!file.exists(spatial_file)) {
        showNotification("Metadata.csv not found in the selected folder.", type = "warning")
        return(NULL)
      }
      
      locs <- read.csv(spatial_file, stringsAsFactors = FALSE)
      
      # Normalize column names
      names(locs) <- trimws(names(locs))
      
      # Ensure Depth column is consistent
      if (!"Depth_m" %in% names(locs) && "Depth" %in% names(locs)) {
        locs$Depth_m <- locs$Depth
      } else if (!"Depth_m" %in% names(locs)) {
        locs$Depth_m <- NA
      }
      
      locs
    })
    
    # Reactive value to track the map state
    is_expanded <- reactiveVal(FALSE)
    
    output$map <- leaflet::renderLeaflet({
      # Base map setup
      base_map <- leaflet() %>%
        setView(lng = -98.5795, lat = 39.8283, zoom = 1) %>%
        addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Satellite") %>%
        addProviderTiles(leaflet::providers$Esri.WorldTopoMap, group = "Topographic") %>%
        addTiles(group = "OSM") %>%
        addLayersControl(baseGroups = c("OSM", "Topographic", "Satellite"))
      
      locs <- try(location_data(), silent = TRUE)
      
      if (!inherits(locs, "try-error") &&
          !is.null(locs) &&
          is.data.frame(locs) &&
          all(c("Longitude", "Latitude") %in% names(locs)) &&
          nrow(locs) > 0) {
        
        # Ensure Depth_m column exists
        if (!"Depth_m" %in% names(locs)) {
          locs$Depth_m <- NA
        }
        
        # --- Center map on data ---
        first_lon <- locs$Longitude[1]
        first_lat <- locs$Latitude[1]
        base_map <- base_map %>% setView(lng = first_lon, lat = first_lat, zoom = 2)
        
        # --- Add markers ---
        base_map <- base_map %>%
          addMarkers(
            data = locs,
            lng = ~Longitude,
            lat = ~Latitude,
            popup = lapply(seq_len(nrow(locs)), function(i) {
              row <- locs[i, ]
              # Columns to include in the popup
              popup_cols <- c("Site", "Latitude", "Longitude", "Depth_m", "Notes")
              # Keep only existing columns
              popup_cols <- popup_cols[popup_cols %in% names(row)]
              # Build popup HTML
              paste(
                sapply(popup_cols, function(col) {
                  paste0("<b>", col, ":</b> ", row[[col]])
                }),
                collapse = "<br>"
              )
            })
          )
      } else {
        # No data → fallback USA-centered view
        base_map <- base_map %>% setView(lng = -98.5795, lat = 39.8283, zoom = 2)
      }
      
      base_map
    })
    
    ##############################################################
    # MAP EXPAND/SHRINK FUNCTION
    ##############################################################
    observeEvent(input$expand_map, {
      showModal(
        modalDialog(
          title = div(
            style = "display: flex; align-items: center; gap: 10px;",
            shiny::icon("map-location-dot"),
            span("Deployment Locations", style = "font-weight: bold; font-size: 1.1em;")
          ),
          size      = "xl",
          easyClose = TRUE,
          footer    = tagList(
            div(
              style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
              # Site count on the left
              uiOutput(ns("modal_site_count")),
              modalButton("Close")
            )
          ),
          tagList(
            # Map fills modal
            leaflet::leafletOutput(ns("map_large"), height = "600px"),
            # Small legend below map
            div(
              style = "margin-top: 8px; font-size: 0.82rem; color: #666; text-align: right;",
              shiny::icon("circle-info"),
              " Click markers for site details. Use layer control to switch basemaps."
            )
          )
        )
      )
    })
    
    output$modal_site_count <- renderUI({
      locs <- try(location_data(), silent = TRUE)
      if (inherits(locs, "try-error") || is.null(locs) || nrow(locs) == 0) return(NULL)
      div(
        style = "font-size: 0.88rem; color: #555;",
        shiny::icon("location-dot", style = "color: #00688B;"),
        paste(nrow(locs), "deployment site(s) loaded")
      )
    })
    
    output$map_large <- leaflet::renderLeaflet({
      # Base map setup
      base_map <- leaflet() %>%
        setView(lng = -98.5795, lat = 39.8283, zoom = 1) %>%
        addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Satellite") %>%
        addProviderTiles(leaflet::providers$Esri.WorldTopoMap, group = "Topographic") %>%
        addTiles(group = "OSM") %>%
        addLayersControl(baseGroups = c("OSM", "Topographic", "Satellite"))
      
      locs <- try(location_data(), silent = TRUE)
      
      if (!inherits(locs, "try-error") &&
          !is.null(locs) &&
          is.data.frame(locs) &&
          all(c("Longitude", "Latitude") %in% names(locs)) &&
          nrow(locs) > 0) {
        
        # Ensure Depth_m column exists
        if (!"Depth_m" %in% names(locs)) {
          locs$Depth_m <- NA
        }
        
        first_lon <- locs$Longitude[1]
        first_lat <- locs$Latitude[1]
        base_map <- base_map %>% setView(lng = first_lon, lat = first_lat, zoom = 2)
        
        base_map <- base_map %>%
          addMarkers(
            data = locs,
            lng = ~Longitude,
            lat = ~Latitude,
            popup = lapply(seq_len(nrow(locs)), function(i) {
              row <- locs[i, ]
              # Columns to include in the popup
              popup_cols <- c("Site", "Latitude", "Longitude", "Depth_m", "Notes")
              # Keep only existing columns
              popup_cols <- popup_cols[popup_cols %in% names(row)]
              # Build popup HTML
              paste(
                sapply(popup_cols, function(col) {
                  paste0("<b>", col, ":</b> ", row[[col]])
                }),
                collapse = "<br>"
              )
            })
          )
      } else {
        # No data → fallback USA-centered view
        base_map <- base_map %>% setView(lng = -98.5795, lat = 39.8283, zoom = 2)
      }
      
      base_map
    })
    
    
    ##############################################################
    # EXAMPLE DATA LOGIC
    ##############################################################
    observeEvent(input$load_example, {
      showModal(modalDialog(
        title = div(
          style = "display: flex; align-items: center; gap: 10px;",
          shiny::icon("flask", style = "color: #CDAD00; font-size: 1.3em;"),
          span("OOI Example Dataset", style = "font-weight: bold; font-size: 1.1em; color: #001f3f;")
        ),
        easyClose = TRUE,
        size = "l",
        footer = div(
          style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
          span(
            style = "font-size: 0.82rem; color: #888;",
            shiny::icon("clock", style = "margin-right: 4px;"),
            "Loading may take a few moments."
          ),
          div(
            style = "display: flex; gap: 8px;",
            modalButton("Cancel"),
            actionButton(ns("confirm_example_load"), "Load Example Data",
                         icon  = shiny::icon("upload"),
                         class = "modal-confirm-btn",   
                         style = "background-color: #00688B; color: white; border: none;
                      border-radius: 6px; padding: 8px 20px; font-weight: 500;"
            )
          )
        ),
        
        tagList(
          
          # About OOI section
          div(
            style = "display: flex; gap: 12px; align-items: flex-start;
                 padding: 12px; border-radius: 8px;
                 background-color: #e8f4fd; border-left: 4px solid #00688B;
                 margin-bottom: 12px;",
            shiny::icon("water", style = "color: #00688B; font-size: 1.2em; margin-top: 2px; flex-shrink: 0;"),
            div(
              tags$b("About the Ocean Observatories Initiative (OOI)"),
              tags$p(
                style = "margin: 6px 0 0 0; font-size: 0.9rem; color: #555;",
                "The NSF-funded OOI maintains a series of coastal and oceanic monitoring sites
             with a multitude of physical and biological sensors. This program collects
             continuous data from a cabled array along the continental shelf and slope
             off Newport, Oregon."
              ),
              tags$p(
                style = "margin: 6px 0 0 0; font-size: 0.9rem; color: #555;",
                "Ocean Science Analytics is exploring the occurrence of vocally active marine
             mammal species in relation to coastal and offshore oceanographic variables
             following recent persistent changes (i.e. warm water ",
                tags$em("\"blob\""), " anomaly) to this dynamic part of the California Current Ecosystem."
              ),
              div(
                style = "margin-top: 8px; display: flex; gap: 16px; flex-wrap: wrap;",
                a(
                  href = "https://ooinet.oceanobservatories.org/", target = "_blank",
                  style = "font-size: 0.85rem; color: #00688B;",
                  shiny::icon("arrow-up-right-from-square", style = "margin-right: 4px;"),
                  "Ocean Observatories Initiative"
                ),
                a(
                  href = "https://www.oceanscienceanalytics.com/coastal-OR-marine-mammal-study",
                  target = "_blank",
                  style = "font-size: 0.85rem; color: #00688B;",
                  shiny::icon("arrow-up-right-from-square", style = "margin-right: 4px;"),
                  "Coastal OR Marine Mammal Study"
                )
              )
            )
          ),
          
          # Dataset info
          div(
            style = "display: flex; gap: 12px; align-items: flex-start;
                 padding: 12px; border-radius: 8px;
                 background-color: #fff8e1; border-left: 4px solid #CDAD00;
                 margin-bottom: 12px;",
            shiny::icon("database", style = "color: #CDAD00; font-size: 1.2em; margin-top: 2px; flex-shrink: 0;"),
            div(
              tags$b("About This Example Dataset"),
              tags$p(
                style = "margin: 6px 0 0 0; font-size: 0.9rem; color: #555;",
                "This dataset contains a single month of passive acoustic data from
             two OOI hydrophone deployments along the Oregon coast."
              )
            )
          ),
          
          # Deployment cards side by side
          div(
            style = "display: flex; gap: 12px; margin-bottom: 16px;",
            
            # HYDBBA103
            div(
              style = "flex: 1; padding: 12px; border-radius: 8px;
                   background-color: #f8f8f8; border: 1px solid #ddd;",
              div(
                style = "display: flex; align-items: center; gap: 8px; margin-bottom: 8px;",
                shiny::icon("microphone", style = "color: #00688B;"),
                tags$b("HYDBBA103", style = "color: #001f3f;")
              ),
              tags$p(
                style = "font-size: 0.82rem; color: #888; margin-bottom: 8px;",
                "Continental Slope — RS01SBPS"
              ),
              div(
                style = "display: flex; flex-direction: column; gap: 4px;",
                a(
                  href = "https://oceanobservatories.org/site/rs01sbps/", target = "_blank",
                  style = "font-size: 0.85rem; color: #00688B;",
                  shiny::icon("anchor", style = "margin-right: 4px;"),
                  "Mooring Information"
                ),
                a(
                  href = "https://rawdata.oceanobservatories.org/files/RS01SBPS/PC01A/HYDBBA103/",
                  target = "_blank",
                  style = "font-size: 0.85rem; color: #00688B;",
                  shiny::icon("file-waveform", style = "margin-right: 4px;"),
                  "Raw Data"
                )
              )
            ),
            
            # HYDBBA106
            div(
              style = "flex: 1; padding: 12px; border-radius: 8px;
                   background-color: #f8f8f8; border: 1px solid #ddd;",
              div(
                style = "display: flex; align-items: center; gap: 8px; margin-bottom: 8px;",
                shiny::icon("microphone", style = "color: #00688B;"),
                tags$b("HYDBBA106", style = "color: #001f3f;")
              ),
              tags$p(
                style = "font-size: 0.82rem; color: #888; margin-bottom: 8px;",
                "Continental Shelf — CE02SHBP"
              ),
              div(
                style = "display: flex; flex-direction: column; gap: 4px;",
                a(
                  href = "https://oceanobservatories.org/site/ce02shbp/", target = "_blank",
                  style = "font-size: 0.85rem; color: #00688B;",
                  shiny::icon("anchor", style = "margin-right: 4px;"),
                  "Mooring Information"
                ),
                a(
                  href = "https://rawdata.oceanobservatories.org/files/CE02SHBP/LJ01D/HYDBBA106/",
                  target = "_blank",
                  style = "font-size: 0.85rem; color: #00688B;",
                  shiny::icon("file-waveform", style = "margin-right: 4px;"),
                  "Raw Data"
                )
              )
            )
          ),
          
          # OOI image
          div(
            style = "border-radius: 8px; overflow: hidden; border: 1px solid #ddd;
                 box-shadow: 0 2px 6px rgba(0,0,0,0.1);",
            tags$img(
              src   = "www/OOI.png",
              width = "100%",
              style = "display: block;"
            )
          )
        )
      ))
    })

    observeEvent(input$confirm_example_load, {
      removeModal()
      showNotification("Loading Example Data Files...", type = "message", duration = 10)
      
      # Full relative path to the example ZIP file inside the app directory
      example_zip_path <- "inst/data/OSA_OOI_Demo.zip"
      
      # Call the existing ZIP processing function
      result <- process_zip(example_zip_path)
      
      selected_dir(result$root_path)
      rds_names(result$rds_names)
      rds_paths(result$rds_paths)     
      rds_cache(list())               
      selected_rds(NULL)
      #rds_data(result$rds_data)
      acoustic_names(result$acoustic_names)
      acoustic_file_tree(result$acoustic_tree)
      soundscape_data(result$soundscape)
      click_detector_data(result$click_detector)
      use_example(TRUE)
      
      output$load_status <- renderText({
        if (!is.null(result$rds_names) && length(result$rds_names) > 0) {
          "✔️ Example Data Loaded"
        } else {
          "Example Data Did Not Load Properly"
        }
      })
    })
    
    
    ##############################################################
    # CLIENT ID LOGIC
    ##############################################################    
    observeEvent(input$submit_id, {
      req(input$client_id)
      
      client_id <- trimws(input$client_id)
      
      if (client_id == "") {
        showNotification("Please enter a Client ID.", type = "error")
        return()
      }
      
      shinyjs::show("client_spinner")
      shinyjs::disable("submit_id")
      
      on.exit({
        shinyjs::hide("client_spinner")
        shinyjs::enable("submit_id")
      })
      
      showNotification("Loading Data Files...", type = "message")
      
      DATA_ROOT <- Sys.getenv("DATA_ROOT", unset = "inst/data")
      client_folder <- file.path(DATA_ROOT, client_id)
     
      if (!dir.exists(client_folder)) {
        showNotification("Client folder not found.", type = "error")
        return()
      }
      
      # Directly process folder (no zip)
      result <- process_folder(client_folder)

      selected_dir(result$root_path)
      rds_names(result$rds_names)
      rds_paths(result$rds_paths)
      rds_cache(list())
      selected_rds(NULL)
      #rds_data(result$rds_data)
      acoustic_names(result$acoustic_names)
      acoustic_file_tree(result$acoustic_tree)
      soundscape_data(result$soundscape)
      click_detector_data(result$click_detector)
      
      output$load_status <- renderText({
        "✔️ Data Succesfully Loaded"
      })
    })
    
    
##############################################################
# ZIP FILE UPLOAD LOGIC
##############################################################
    
    observeEvent(input$submit_files, {
      req(input$zip_file)  # Wait for zip upload
      showNotification("Reading Uploaded Data Files...", type = "message")
      
      zip_path <- input$zip_file$datapath
      
      tryCatch({
        result <- process_zip(zip_path)
        
        # Set reactive values
        selected_dir(result$root_path)
        
        rds_names(result$rds_names)
        rds_paths(result$rds_paths)     
        rds_cache(list()) 
        selected_rds(NULL)
        #rds_data(result$rds_data)
        acoustic_names(result$acoustic_names)
        acoustic_file_tree(result$acoustic_tree)
        soundscape_data(result$soundscape)
        click_detector_data(result$click_detector)
        use_example(FALSE)
        
        # Status
        output$load_status <- renderText({
          if (!is.null(result$rds_names) && length(result$rds_names) > 0) {
            paste0("✔️ ", length(result$rds_names), " Datasets Loaded")
          } else {
            "No Datasets Loaded"
          }
        })
        
      }, error = function(e) {
        selected_dir(NULL)
        output$load_status <- renderText(paste("❌ Error loading ZIP:", e$message))
        showNotification(paste("Error:", e$message), type = "error")
      })
    })

    ##############################################################
    # DELETE TEMPORARY DIRECTORY
    ##############################################################
    session$onSessionEnded(function() {
      unlink(tempdir(), recursive = TRUE, force = TRUE)
    })
    
    
    ##############################################################
    # STORE PROCESSED DATA FILES FOR OTHER MODULES TO ACCESS
    ##############################################################

    # Return both file paths and original names for use in other modules
    return(list(
      rds_names = rds_names,
      rds_paths = rds_paths,
      rds_cache = rds_cache,       
      selected_rds = selected_rds,
      #rds_data = rds_data,   
      acoustic_names = acoustic_names,
      acoustic_file_tree = acoustic_file_tree,
      soundscape_data = soundscape_data,
      click_detector_data = click_detector_data,
      selected_dir = selected_dir,
      use_example = use_example
    ))
    
    
    # Other modules can access rds data using:
    #   my_data_list <- file_outputs$rds_data()
    #   some_data <- my_data_list[["Event_A"]]
    
  })
}
    
## To be copied in the UI
# mod_main_ui("main_1")
    
## To be copied in the server
# mod_main_server("main_1")
