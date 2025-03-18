#' spectro UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import shinycssloaders
#' @import seewave
#' @import tuneR
#' @import ggplot2
mod_spectro_ui <- function(id) {
  ns <- NS(id)
  
  ##############################################################
  # INITIAL UI STRUCTURE, SLIDERS GET ADDED IN SERVER CODE
  ##############################################################
  
  tagList(
    # Card-style container for input selection & button
    div(
      style = "border: 2px solid #ccc; border-radius: 8px; padding: 15px; background-color: #f9f9f9; margin-bottom: 15px;",
      
      h4("Spectrogram Selection", style = "margin-bottom: 10px;"),  # Section title
      
      div(
        style = "display: flex; align-items: center; gap: 10px; width: 100%;",
        
        # Audio File Selection
        selectInput(ns("audio_select"), "Select Audio File(s):", choices = NULL, multiple = TRUE, width = "100%"),
        
        # Load Button (Aligned to the right)
        div(style = "flex-grow: 1; text-align: right;",
            actionButton(ns("load_spectro"), "Load Spectrogram", class = "btn btn-primary", style = "width: 200px;")
        )
      )
    ),
    
    # Scrollable Section for Spectrograms
    div(
      style = "max-height: 600px; overflow-y: auto; border: 1px solid #ddd; padding: 10px; border-radius: 5px; background-color: #fff;",
      
      uiOutput(ns("spectrograms_ui"))  # Dynamic spectrogram display
    )
  )
}
    
#' spectro Server Functions
#'
#' @noRd 
mod_spectro_server <- function(id, data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    ##############################################################
    # READ AUDIO FILES FROM MAIN MODULE AND UPDATE FILE SELECTION
    ##############################################################
    
    observeEvent(data$audio_files(), {
      file_paths <- data$audio_files()
      file_names <- data$audio_names()  # Get original file names
      
      if (!is.null(file_paths) && length(file_paths) > 0) {
        updateSelectInput(session, "audio_select", 
                          choices = setNames(file_paths, file_names),  # Use original names as labels
                          selected = file_paths[1])  # Default to first file
      }
    }, ignoreInit = TRUE)  # Don't run when the module first loads
    
    
    ##############################################################
    # GENERATE UNIQUE SPECTROGRAMS FOR EACH AUDIO FILE SELECTED
    ##############################################################
    
    observeEvent(input$load_spectro, {
      req(data$audio_files(), input$audio_select)
      
      file_paths <- data$audio_files()
      selected_files <- file_paths[file_paths %in% input$audio_select]  # Match full paths
      
      # Generate UI dynamically: Sliders + Spectrograms
      output$spectrograms_ui <- renderUI({
        file_ui <- lapply(seq_along(selected_files), function(i) {
          file <- selected_files[i]
          wav_data <- readWave(file)
          
          # Get WAV file properties
          file_duration <- length(wav_data@left) / wav_data@samp.rate
          nyquist_freq <- wav_data@samp.rate / 2
          
          # Generate unique input IDs
          time_slider_id <- paste0("time_range_", i)
          freq_slider_id <- paste0("freq_range_", i)
          
          # Create a div container for each spectrogram + its sliders
          tagList(
            div(
              style = "border: 2px solid black; padding: 10px; border-radius: 5px; margin-bottom: 15px; background-color: #F8F8F8;",
              sliderInput(ns(time_slider_id), "Temporal Range:", 
                          min = 0, max = file_duration, value = c(0, file_duration)),
              # Uncomment if you want the frequency slider
              # sliderInput(ns(freq_slider_id), "Frequency Range:", 
              #             min = 0, max = nyquist_freq, value = c(0, nyquist_freq)),
              shinycssloaders::withSpinner(
                plotOutput(ns(paste0("spectrogram_", i))),
                type = 4, 
                color = "#00688B",  # DeepSkyBlue4
                caption = "Loading Spectrogram..."
              )
            )
          )
        })
        
        do.call(tagList, file_ui)
      })
      
      # Render spectrograms (will rerender based on individual sliders)
      lapply(seq_along(selected_files), function(i) {
        local({
          idx <- i
          file <- selected_files[idx]
          time_slider_id <- paste0("time_range_", idx)
          freq_slider_id <- paste0("freq_range_", idx) # Not included yet
          spectro_id <- paste0("spectrogram_", idx)
          
          output[[spectro_id]] <- renderPlot({
            wav_data <- readWave(file)
            file_duration <- length(wav_data@left) / wav_data@samp.rate
            nyquist_freq <- wav_data@samp.rate / 2
            
            # Get slider values
            time_range <- input[[time_slider_id]]
            freq_range <- input[[freq_slider_id]]
            
            # Ensure valid ranges
            time_min <- max(0, min(time_range[1], file_duration))
            time_max <- max(0, min(time_range[2], file_duration))
            # freq_min <- max(0, min(freq_range[1], nyquist_freq))
            # freq_max <- max(0, min(freq_range[2], nyquist_freq))
            
            # Generate spectrogram with user-defined limits
            spectro(wav_data,
                    tlim = c(time_min, time_max),
                    flim = c(0, 30), ### will change this once the frequency sliders work
                    wl = 1024,
                    collevels = seq(-124, 0, 2),
                    palette = temp.colors,
                    main = data$audio_names()[which(data$audio_files() == file)]
            )
          })
        })
      })
    })
  })
}
    
#     observeEvent(input$load_spectro, {  # Only trigger when button is clicked
#       output$spectrogram <- renderPlot({
#         req(audio_files())  # Ensure an audio file is uploaded
#         
#         # Read the .wav file
#         wav_data <- readWave(audio_files())
#         
#         # Generate spectrogram
#         spectro(wav_data,
#                 flim = c(5, 20),
#                 wl = 1024,
#                 collevels = seq(-124, 0, 2),
#                 palette = temp.colors,
#                 main = "Spectrogram 1")
#       })
#     })
#   })
# }
    
# seewave - https://rug.mnhn.fr/seewave/
# spectro - https://www.rdocumentation.org/packages/seewave/versions/2.2.3/topics/spectro
# ggspectro - https://www.rdocumentation.org/packages/seewave/versions/2.2.3/topics/ggspectro

## To be copied in the UI
# mod_spectro_ui("spectro_1")
    
## To be copied in the server
# mod_spectro_server("spectro_1")
