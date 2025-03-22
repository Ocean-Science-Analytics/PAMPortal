#' main UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

options(shiny.maxRequestSize = 700 * 1024^2)

mod_main_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
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
      "))
    ),

    ##############################################################
    # SIDE BAR UI ELEMENTS
    ##############################################################
    tags$div(
      class = "input-section",
      h4(tags$span(shiny::icon("file-upload"), " Select Data File:")),
      div(style = "width: 100%;",  # Ensures full width
          fileInput(ns("data"), NULL, width = "100%"),
          shiny::h6(
            "Note: Files up to 700MB are supported. Larger files may take several minutes to load.",
            style = "margin-top: -30px; margin-bottom: 25px; font-size: 0.82rem; color: #888;"
          )
      ),  # Expands with container
      h4(tags$span(shiny::icon("file-audio"), " Select Audio Files:")), 
      div(style = "width: 100%;",  
          fileInput(ns("audio"), multiple = TRUE, NULL, width = "100%")),
      div(
        style = "display: flex; width: 100%;",
        actionButton(ns("submit_files"), "Load Files", class = "custom-btn", style = "flex-grow: 1;")
      ),
      
      textOutput(ns("load_status"))
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
    # THIS SERVER PREPARES THE DATA FROM THE DIFFERENT FILES
    ##############################################################
    
    # Reactive values to store uploaded file paths and original names
    data_file <- reactiveVal(NULL)
    file_type <- reactiveVal(NULL)
    audio_files <- reactiveVal(NULL)
    audio_names <- reactiveVal(NULL)  # Original filenames
    uploaded_audio_paths <- NULL  # Non-Reactive list to delete files when app closes
    
    observeEvent(input$submit_files, {
      # Handle data file (Rdata or JSON)
      if (!is.null(input$data)) {
        ext <- tools::file_ext(input$data$name)  # Get file extension
        
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

    # Delete temporary audio files when the session ends
    session$onSessionEnded(function() {
      if (!is.null(uploaded_audio_paths) && length(uploaded_audio_paths) > 0) {
        file.remove(uploaded_audio_paths)
      }
    })
    
    # Return both file paths and original names for use in other modules
    return(list(
      data_file = data_file, 
      file_type = file_type,
      audio_files = audio_files,
      audio_names = audio_names  # Include original names
    ))
  })
}
    
## To be copied in the UI
# mod_main_ui("main_1")
    
## To be copied in the server
# mod_main_server("main_1")
