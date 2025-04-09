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
      h4(tags$span(shiny::icon("file-upload"), " Select Data File:")),
      div(style = "width: 100%;",  
          fileInput(ns("data"), NULL, width = "100%", accept = c(".rds"))  # File input
      ),
      
      h4(tags$span(shiny::icon("file-audio"), " Select Audio Files:")), 
      div(style = "width: 100%;",  
          fileInput(ns("audio"), multiple = TRUE, NULL, width = "100%")  # File input
      ),
      
      # Add note above the Load Files button
      shiny::h6(
        "Note: Files up to 800MB are supported. Larger files may take several minutes to load.",
        style = "margin-top: 10px; margin-bottom: 10px; font-size: 0.82rem; color: #888; text-align: left;"
      ),
      
      # Submit button with loading spinner
      div(
        style = "display: flex; width: 100%;",
        actionButton(ns("submit_files"), "Load Files", class = "custom-btn", style = "flex-grow: 1;")
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
      actionButton(ns("expand_map"), "Expand Map", class = "map-toggle-btn")
    )
  )
}
    
#' main Server Functions
#'
#' @noRd 
mod_main_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Reactive value to track the map state
    is_expanded <- reactiveVal(FALSE)
    
    ##############################################################
    # LEAFLET MAP LOGIC
    ##############################################################
    output$map <- leaflet::renderLeaflet({
      leaflet() %>%
        leaflet::setView(lng = -98.5795, lat = 39.8283, zoom = 2) %>%  # Default: Centered on USA
        leaflet::addProviderTiles(
          leaflet::providers$Esri.WorldImagery,
          group = "Satellite"
        ) %>%
        leaflet::addProviderTiles(
          leaflet::providers$Esri.WorldTopoMap,
          group = "Topographic"
        ) %>%
        leaflet::addLayersControl(
          baseGroups = c("OSM", "Topographic", "Satellite")) %>%
        addTiles() 
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
    
    # Render the larger map when the modal is opened
    output$map_large <- leaflet::renderLeaflet({
      leaflet() %>%
        leaflet::setView(lng = -98.5795, lat = 39.8283, zoom = 2) %>%
        leaflet::addProviderTiles(
          leaflet::providers$Esri.WorldImagery,
          group = "Satellite"
        ) %>%
        leaflet::addProviderTiles(
          leaflet::providers$Esri.WorldTopoMap,
          group = "Topographic"
        ) %>%
        leaflet::addLayersControl(
          baseGroups = c("OSM", "Topographic", "Satellite")
        ) %>%
        addTiles()
    })
    
    
    ##############################################################
    # FILE HANDLING LOGIC
    ##############################################################
    
    # Reactive values to store uploaded file paths and original names
    data_file <- reactiveVal(NULL)
    file_type <- reactiveVal(NULL)
    data_name <- reactiveVal(NULL)
    audio_files <- reactiveVal(NULL)
    audio_names <- reactiveVal(NULL)  # Original filenames
    uploaded_audio_paths <- NULL  # Non-Reactive list to delete audio files when app closes
    
    observeEvent(input$submit_files, {
      # Handle data file (Rdata or JSON)
      if (!is.null(input$data)) {
        ext <- tools::file_ext(input$data$name)  # Get file extension
        name_without_ext <- tools::file_path_sans_ext(input$data$name)  # Extract name without extension
        data_name(name_without_ext)  # Store the file name
        
        if (ext == "rds") {
          # Load RDS file
          data_file(readRDS(input$data$datapath))
          file_type("rds")
          
        } else if (ext == "json") {
          # Load JSON file
          data_file(jsonlite::fromJSON(input$data$datapath))
          file_type("json")
        }
      }
      
      if (!is.null(input$audio)) {
        # Define destination folder
        www_dir <- "inst/app/www"
        if (!dir.exists(www_dir)) dir.create(www_dir)  # Ensure 'www/' exists
        
        # Copy files to www/ and rename to avoid overwrites
        saved_paths <- sapply(seq_along(input$audio$datapath), function(i) {
          src <- input$audio$datapath[i]
          dest <- file.path(www_dir, input$audio$name[i])
          
          file.copy(src, dest, overwrite = TRUE)
          return(dest)
        })
        
        # Store the new file paths and original names
        audio_files(saved_paths)
        uploaded_audio_paths <<- c(uploaded_audio_paths, saved_paths)
        audio_names(input$audio$name)
      }
      
      # Read and process each .wav file if audio is provided
      if (!is.null(input$audio)) {
        wav_list <- lapply(input$audio$datapath, function(file) {
          readWave(file)
        })
      }
      
      # Update the text output dynamically based on loaded files
      output$load_status <- renderText({
        data_loaded <- !is.null(data_file())
        audio_loaded <- !is.null(audio_files())
        
        if (data_loaded & audio_loaded) {
          paste0("✔️ Data file and ", length(audio_files()), " audio files successfully loaded!")
        } else if (data_loaded) {
          "✔️ Data file successfully loaded!"
        } else if (audio_loaded) {
          paste0("✔️ ", length(audio_files()), " audio files successfully loaded!")
        } else {
          "No data or audio files loaded"  # Show nothing if neither is loaded
        }
      })
    })

    
    ##############################################################
    # AUDIO FILE DELETION LOGIC
    ##############################################################
    
    # Delete temporary audio files when the session ends
    session$onSessionEnded(function() {
      if (!is.null(uploaded_audio_paths) && length(uploaded_audio_paths) > 0) {
        file.remove(uploaded_audio_paths)
      }
    })
    
    
    ##############################################################
    # STORE PROCESSED DATA FILES FOR OTHER MODULES TO ACCESS
    ##############################################################
    
    # Return both file paths and original names for use in other modules
    return(list(
      data_file = data_file, 
      file_type = file_type, # type of data (i.e. json or rds)
      data_name = data_name, # name of data file
      audio_files = audio_files,
      audio_names = audio_names # name of audio files
    ))
  })
}
    
## To be copied in the UI
# mod_main_ui("main_1")
    
## To be copied in the server
# mod_main_server("main_1")
