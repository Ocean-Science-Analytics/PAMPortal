#' main UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_main_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Include inline CSS for the Browse button
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
      "))
    ),
    
    tags$div(
      class = "input-section",
      h4(tags$span(shiny::icon("file-upload"), " Select Data File:")), # See https://fontawesome.com/search?q=audio&o=r
      fileInput(ns("data"), NULL),
      selectInput(ns("select1"), "SELECT OPTION 1:", choices = NULL),
      selectInput(ns("select2"), "SELECT OPTION 2:", choices = NULL)
    ),
    tags$br(), # Spacer
    tags$div(
      class = "input-section",
      h4(tags$span(shiny::icon("file-audio"), " Select Audio File:")), 
      fileInput(ns("audio"), NULL),
      sliderInput(ns("freq_range"), "FREQUENCY RANGES:", min = 0, max = 100, value = c(0, 100)),
      sliderInput(ns("time_range"), "TEMPORAL RANGES:", min = 0, max = 1, value = c(0, 1))
      
    )
  )
}
    
#' main Server Functions
#'
#' @noRd 
mod_main_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Reactive value to store the uploaded .wav file path
    audio_file <- reactiveVal(NULL)
    
    observeEvent(input$audio, {
      req(input$audio)  
      audio_file(input$audio$datapath)  
      
      # Read .wav file and extract duration
      wav_data <- readWave(input$audio$datapath)
      duration <- length(wav_data@left) / wav_data@samp.rate  # Calculate duration in seconds
      
      # Update the temporal slider based on duration
      updateSliderInput(session, "time_range", min = 0, max = duration, value = c(0, duration))
    })
    
    # Return the reactive value so other modules can use it
    return(list(audio_file = audio_file))
  })
}
    
## To be copied in the UI
# mod_main_ui("main_1")
    
## To be copied in the server
# mod_main_server("main_1")
