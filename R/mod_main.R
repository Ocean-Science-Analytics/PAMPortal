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

options(shiny.maxRequestSize = 800 * 1024^2)

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
      
      h4(tags$span(shiny::icon("file-upload"), " Select PAM Folder:"), style = "color: black;"), 
      div(style = "display: flex; width: 100%;",  
          fileInput(ns("zip_file"), "Upload a Zip Folder", accept = ".zip")
          # shinyFiles::shinyDirButton(
          #   id = ns("dir"),
          #   label = "Choose PAM Folder Directory",
          #   class = "custom-btn",
          #   title = "Select PAM Folder",
          #   style = "flex-grow: 1;"
          # )
      ),
      
      #shiny::verbatimTextOutput(ns("directory")),
      
      #br(),
      # Add note above the Load Files button
      shiny::h6(
        "Note: Files up to 800MB are supported. Larger files may take several minutes to load.",
        style = "margin-top: 10px; margin-bottom: 10px; font-size: 0.82rem; color: #888; text-align: left;"
      ),
      
      # Submit button with loading spinner
      div(
        style = "display: flex; width: 100%;",
        actionButton(ns("submit_files"), "Load Files", icon = shiny::icon("folder-open"), class = "custom-btn", style = "flex-grow: 1;")
      ),
      
      textOutput(ns("load_status"))
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
    
##############################################################
# DIRECTORY OUTPUT LOGIC
##############################################################
    selected_dir <- reactiveVal(NULL)
    
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
      
      spatial_file <- file.path(root_path, "Spatial_Data.csv")
      
      if (!file.exists(spatial_file)) {
        showNotification("Spatial_Data.csv not found in the selected folder.", type = "warning")
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
        
        base_map <- base_map %>%
          addMarkers(
            data = locs,
            lng = ~Longitude,
            lat = ~Latitude,
            popup = lapply(seq_len(nrow(locs)), function(i) {
              row <- locs[i, , drop = FALSE]
              paste0(
                "<b>", names(row), ":</b> ", 
                unlist(lapply(row, as.character)), 
                collapse = "<br>"
              )
            })
          )
      }
      
      base_map
    })
    
    ##############################################################
    # MAP EXPAND/SHRINK FUNCTION
    ##############################################################
    observeEvent(input$expand_map, {
      showModal(
        modalDialog(
          title = "Map View",
          size = "xl",  # Large modal
          easyClose = TRUE,
          footer = modalButton("Close"),
          leaflet::leafletOutput(ns("map_large"), height = "600px")
        )
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
        
        base_map <- base_map %>%
          addMarkers(
            data = locs,
            lng = ~Longitude,
            lat = ~Latitude,
            popup = lapply(seq_len(nrow(locs)), function(i) {
              row <- locs[i, , drop = FALSE]
              paste0(
                "<b>", names(row), ":</b> ", 
                unlist(lapply(row, as.character)), 
                collapse = "<br>"
              )
            })
          )
      }
      
      base_map
    })
    
    
##############################################################
# FILE HANDLING LOGIC
##############################################################
    
    rds_names <- reactiveVal(NULL)
    rds_data <- reactiveVal(NULL)
    acoustic_names <- reactiveVal(NULL)
    acoustic_file_tree <- reactiveVal(NULL)
    soundscape_data <- reactiveVal(NULL)
    
    observeEvent(input$submit_files, {
      req(input$zip_file)  # Wait for zip upload
      showNotification("Loading Data Files...", type = "message")
      
      # Unzip uploaded folder into temp directory
      zip_path <- input$zip_file$datapath
      temp_dir <- tempfile()
      dir.create(temp_dir)

      tryCatch({
        unzip(zip_path, exdir = temp_dir)
        extracted_files <- list.files(temp_dir, recursive = TRUE, full.names = TRUE)
        top_level_dirs <- list.dirs(temp_dir, recursive = FALSE, full.names = TRUE)
        if (length(top_level_dirs) == 1) {
          project_root <- top_level_dirs[[1]]
          selected_dir(project_root)
        } else {
          showNotification("Multiple folders found at root of ZIP. Please ensure the ZIP contains a single project folder.", type = "error")
          selected_dir(NULL)
          return()
        }
        
        #### ---- LOAD STATUS ---- ####
        output$load_status <- renderText({
          rds <- rds_names()
          #audio <- acoustic_names()
          
          if (!is.null(rds) && length(rds) > 0) {
            paste0("✔️ ", length(rds), " Datasets Loaded")
          } else {
            "No Datasets Loaded"
          }
        })
        
        #### ==== FILE HANDLING LOGIC START ==== ####
        root_path <- selected_dir()
        rds_folder <- file.path(root_path, "RDS")
        acoustic_dir <- file.path(root_path, "Audio")
        soundscape_dir <- file.path(root_path, "Soundscape")
        
        #### ---- RDS LOADING ---- ####
        if (dir.exists(rds_folder)) {
          rds_paths <- list.files(rds_folder, pattern = "\\.rds$", full.names = TRUE, ignore.case = TRUE)
          
          if (length(rds_paths) > 0) {
            names_only <- tools::file_path_sans_ext(basename(rds_paths))
            rds_names(names_only)
            
            data_list <- setNames(lapply(rds_paths, readRDS), names_only)
            rds_data(data_list)
          } else {
            showNotification("No .rds files found in RDS folder.", type = "warning")
            rds_names(NULL)
            rds_data(NULL)
          }
        } else {
          showNotification("RDS folder not found.", type = "error")
          rds_names(NULL)
        }
        
        #### ---- ACOUSTIC LOADING ---- ####
        if (dir.exists(acoustic_dir)) {
          event_paths <- list.dirs(acoustic_dir, recursive = FALSE, full.names = TRUE)
          event_names <- basename(event_paths)
          cleaned_event_names <- sub("_Clips$", "", event_names)
          acoustic_names(cleaned_event_names)
          
          build_nested_list <- function(base_dir) {
            event_folders <- list.dirs(base_dir, recursive = FALSE, full.names = TRUE)
            structure_list <- list()
            
            for (event_path in event_folders) {
              event_name <- basename(event_path)
              species_paths <- list.dirs(event_path, recursive = FALSE, full.names = TRUE)
              species_list <- list()
              
              for (species_path in species_paths) {
                species_name <- basename(species_path)
                detection_paths <- list.dirs(species_path, recursive = FALSE, full.names = TRUE)
                detection_list <- list()
                
                for (detection_path in detection_paths) {
                  detection_name <- basename(detection_path)
                  wavs <- list.files(detection_path, pattern = "\\.wav$", full.names = TRUE)
                  if (length(wavs) > 0) {
                    detection_list[[detection_name]] <- wavs
                  }
                }
                
                if (length(detection_list) > 0) {
                  species_list[[species_name]] <- detection_list
                }
              }
              
              if (length(species_list) > 0) {
                structure_list[[event_name]] <- species_list
              }
            }
            
            return(structure_list)
          }
          
          tree <- build_nested_list(acoustic_dir)
          acoustic_file_tree(tree)
        } else {
          showNotification("Audio folder not found.", type = "error")
          acoustic_file_tree(NULL)
        }
        
        #### ---- SOUNDSCAPE LOADING ---- ####
        if (dir.exists(soundscape_dir)) {
          site_folders <- list.dirs(soundscape_dir, recursive = FALSE, full.names = FALSE)
          
          if (length(site_folders) > 0) {
            soundscape_data(site_folders)
          } else {
            showNotification("No site folders found in Soundscape.", type = "warning")
            soundscape_data(NULL)
          }
        } else {
          showNotification("Soundscape folder not found.", type = "error")
          soundscape_data(NULL)
        }
        
        #### ==== FILE HANDLING LOGIC END ==== ####
        
      }, error = function(e) {
        output$load_status <- renderText(paste("Error unzipping file:", e$message))
      })
    })
    
 
    # ##############################################################
    # # AUDIO FILE DELETION LOGIC
    # ##############################################################
    # 
    # # Delete temporary audio files when the session ends
    # session$onSessionEnded(function() {
    #   if (!is.null(uploaded_audio_paths) && length(uploaded_audio_paths) > 0) {
    #     file.remove(uploaded_audio_paths)
    #   }
    # })
    # 
    # 
    # ##############################################################
    # # STORE PROCESSED DATA FILES FOR OTHER MODULES TO ACCESS
    # ##############################################################
    # 
    # Return both file paths and original names for use in other modules
    return(list(
      rds_names = rds_names,
      rds_data = rds_data,   
      acoustic_names = acoustic_names,
      acoustic_file_tree = acoustic_file_tree,
      soundscape_data = soundscape_data,
      selected_dir = selected_dir
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
