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
  
  tagList(
    tags$head(
      tags$style(HTML("
        .full-height {
          height: 100vh;
          display: flex;
          flex-direction: column;
        }
      "))
    ),
    actionButton(ns("add_spectro"), "+ Add Spectrogram"),
    br(),
    uiOutput(ns("spectrograms_ui"))
    # fluidRow(
    #   column(6, card_spectro(ns, "spectro1", 1)),
    #   column(6, card_spectro(ns, "spectro2", 2))
    # ),
    # fluidRow(
    #   column(6, card_spectro(ns, "spectro3", 3)),
    #   column(6, card_spectro(ns, "spectro4", 4))
    # )
  )
}
  
  
  
  
  ##############################################################
  # INITIAL UI STRUCTURE, SLIDERS GET ADDED IN SERVER CODE
  ##############################################################
  
  # tagList(
  #   tags$head(
  #     tags$style(HTML("
  #       .full-height {
  #         height: 100vh;
  #         display: flex;
  #         flex-direction: column;
  #       }
  #     "))
  #   ),
  #   
  #   # Card-style container for input selection & button
  #   div(class = "full-height",
  #     div(
  #       style = "border: 2px solid #ccc; border-radius: 8px; padding: 15px; background-color: #f9f9f9; margin-bottom: 15px;",
  #       h4("Spectrogram Selection", style = "margin-bottom: 10px;"),
  #       
  #       div(
  #         style = "display: flex; align-items: center; gap: 10px; width: 100%;",
  #         # Audio File Selection
  #         selectInput(ns("species_select"), "Select Species:", choices = NULL, multiple = TRUE, width = "50%"),
  #         selectInput(ns("species_event_select"), "Select Event(s):", choices = NULL, multiple = TRUE, width = "50%"),
  #       ),
  #       div(
  #         style = "display: flex; align-items: center; gap: 10px; width: 100%;",
  #         # Audio File Selection
  #         selectInput(ns("audio_select"), "Select Audio File(s):", choices = NULL, multiple = TRUE, width = "80%"),
  #         # Load Button (Aligned to the right)
  #         div(style = "flex-grow: 3; text-align: center;",
  #             actionButton(ns("load_spectro"), "Load Spectrogram", class = "btn btn-primary", style = "width: 200px;")
  #         )
  #       )
  #     ),
  #     
  #     # Section for Spectrograms
  #     div(
  #       style = "max-height: 1000px; overflow-y: auto; border: 1px solid #ddd; padding: 10px; border-radius: 5px; background-color: #fff;",
  #       
  #       uiOutput(ns("spectrograms_ui"))  # Spectrogram display
  #     )

#}
    
#' spectro Server Functions
#'
#' @noRd 
mod_spectro_server <- function(id, data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    tree <- reactive(data$acoustic_file_tree())
    
    spectro_counter <- reactiveVal(1)
    
    observeEvent(input$add_spectro, {
      current <- spectro_counter()
      if (current < 4) {
        spectro_counter(current + 1)
        
        i <- current + 1
        output[[paste0("spectro_ui_", i)]] <- renderUI({
          column(6, card_spectro(ns, paste0("spectro", i), i))
        })
        
        # Start server for the new spectrogram
        createSpectroServer(paste0("spectro", i))
      }
    })
    
    output$spectrograms_ui <- renderUI({
      n_to_show <- spectro_counter()
      tagList(lapply(seq_len(n_to_show), function(i) {
        div(id = paste0("spectro_card_", i), uiOutput(ns(paste0("spectro_ui_", i))))
      }))
    })
    
    createSpectroServer <- function(prefix) {
      observe({
        updateSelectInput(session, paste0(prefix, "-event"),
                          choices = names(tree()))
      })
      
      observeEvent(input[[paste0(prefix, "-event")]], {
        species_choices <- names(tree()[[input[[paste0(prefix, "-event")]]]])
        updateSelectInput(session, paste0(prefix, "-species"),
                          choices = species_choices)
      }, ignoreInit = TRUE)
      
      observeEvent(input[[paste0(prefix, "-species")]], {
        det_choices <- names(tree()[[input[[paste0(prefix, "-event")]]]][[input[[paste0(prefix, "-species")]]]])
        updateSelectInput(session, paste0(prefix, "-detection"),
                          choices = det_choices)
      }, ignoreInit = TRUE)
      
      observeEvent(input[[paste0(prefix, "-detection")]], {
        event <- input[[paste0(prefix, "-event")]]
        species <- input[[paste0(prefix, "-species")]]
        detection <- input[[paste0(prefix, "-detection")]]
        
        wavs <- tryCatch({
          tree()[[event]][[species]][[detection]]
        }, error = function(e) NULL)
        
        # Check if `wavs` is valid before applying basename
        if (!is.null(wavs) && is.character(wavs)) {
          updateSelectInput(session, paste0(prefix, "-file"),
                            choices = basename(wavs))
        } else {
          updateSelectInput(session, paste0(prefix, "-file"),
                            choices = character(0))  # Empty the dropdown
        }
      }, ignoreInit = TRUE)
      
      output[[paste0(prefix, "-spectrogram")]] <- renderPlot({
        req(input[[paste0(prefix, "-file")]])
        wavs <- tree()[[input[[paste0(prefix, "-event")]]]][[input[[paste0(prefix, "-species")]]]][[input[[paste0(prefix, "-detection")]]]]
        wav_path <- wavs[basename(wavs) == input[[paste0(prefix, "-file")]]]
        wav <- tuneR::readWave(wav_path)
        
        time_range <- input[[paste0(prefix, "-time_range")]]
        
        # Styled spectrogram
        seewave::spectro(
          wav,
          tlim = time_range,
          flim = c(0, 25),  # Can be updated with freq slider later
          wl = 1024,
          collevels = seq(-100, 0, 2),
          palette = seewave::temp.colors,
          main = basename(wav_path)
        )
      })
      
      observeEvent(input[[paste0(prefix, "-file")]], {
        req(
          input[[paste0(prefix, "-event")]],
          input[[paste0(prefix, "-species")]],
          input[[paste0(prefix, "-detection")]]
        )
        
        wavs <- tree()[[input[[paste0(prefix, "-event")]]]][[input[[paste0(prefix, "-species")]]]][[input[[paste0(prefix, "-detection")]]]]
        wav_path <- wavs[basename(wavs) == input[[paste0(prefix, "-file")]]]
        wav <- tuneR::readWave(wav_path)
        duration <- length(wav@left) / wav@samp.rate

        updateSliderInput(
          session,
          paste0(prefix, "-time_range"),
          min = 0,
          max = duration,
          value = c(0, duration)
        )
      })
    }
    
    lapply(1:4, function(i) createSpectroServer(paste0("spectro", i)))
  })
}
    
    
    
    
    
#################################################################################################################################    
    # tree <- data$acoustic_file_tree()
    # 
    # spectro_list <- reactiveVal(list(1))
    # spectro_counter <- reactiveVal(1)
    # 
    # observeEvent(input$add_spectro, {
    #   current <- spectro_list()
    #   new_id <- max(current) + 1
    #   spectro_list(c(current, new_id))
    #   spectro_counter(new_id)
    # })
    # 
    # output$spectro_cards <- renderUI({
    #   lapply(spectro_list(), function(i) {
    #     ns_card <- function(x) ns(paste0("spectro_", i, "_", x))
    #     
    #     div(
    #       style = "border: 2px solid #ccc; border-radius: 8px; padding: 15px; background-color: #f9f9f9; margin-bottom: 15px;",
    #       h4(paste("Spectrogram", i)),
    #       
    #       selectInput(ns_card("event_select"), "Select Event (Site):", choices = names(tree())),
    #       selectInput(ns_card("species_select"), "Select Species:", choices = ""),
    #       selectInput(ns_card("detection_select"), "Select Detection:", choices = NULL),
    #       selectInput(ns_card("wav_select"), "Select WAV File:", choices = NULL),
    #       
    #       actionButton(ns_card("load_spectro"), "Load Spectrogram", class = "btn btn-primary"),
    #       plotOutput(ns_card("spectro_plot"))
    #     )
    #   })
    # })
    # 
    # observe({
    #   lapply(spectro_list(), function(i) {
    #     local({
    #       index <- i
    #       print(str(tree()))
    #       # Dynamic namespacing function for inputs inside each spectro card
    #       ns_card <- function(x) ns(paste0("spectro_", index, "_", x))
    #       
    #       # observeEvent(input[[ns_card("event_select")]], {
    #       #   event <- input[[ns_card("event_select")]]
    #       #   cat("Selected Event:", event, "\n")
    #       #   print("Tree content at selected event:")
    #       #   print(str(tree()[[event]]))
    #       # })
    #       
    #       # ----- Update Species based on Event selection -----
    #       observeEvent(input[[ns_card("event_select")]], {
    #         event <- input[[ns_card("event_select")]]
    #         req(event, tree())
    #         species_choices <- names(tree()[[event]])
    #         updateSelectInput(session, ns_card("species_select"), choices = species_choices, selected = species_choices[1])
    #       })
    #       
    #       # ----- Update Detection based on Species selection -----
    #       observeEvent(input[[ns_card("species_select")]], {
    #         event <- input[[ns_card("event_select")]]
    #         species <- input[[ns_card("species_select")]]
    #         req(event, species, tree())
    #         detection_choices <- names(tree()[[event]][[species]])
    #         updateSelectInput(session, ns_card("detection_select"), choices = detection_choices, selected = detection_choices[1])
    #       })
    #       
    #       # ----- Update WAV based on Detection selection -----
    #       observeEvent(input[[ns_card("detection_select")]], {
    #         event <- input[[ns_card("event_select")]]
    #         species <- input[[ns_card("species_select")]]
    #         detection <- input[[ns_card("detection_select")]]
    #         req(event, species, detection, tree())
    #         wav_choices <- tree()[[event]][[species]][[detection]]
    #         updateSelectInput(session, ns_card("wav_select"), choices = wav_choices, selected = wav_choices[1])
    #       })
    #       
    #       # Placeholder: load spectrogram
    #       observeEvent(input[[ns_card("load_spectro")]], {
    #         # Your plotting logic here
    #       })
    #     })
    #   })
    # })
    
    
    
############################################################################################################################################    
    
    
    
    
    
    # ##############################################################
    # # READ AUDIO FILES FROM MAIN MODULE AND UPDATE FILE SELECTION
    # ##############################################################
    # 
    # observeEvent(data$audio_files(), {
    #   file_paths <- data$audio_files()
    #   file_names <- data$audio_names()  # Get original file names
    #   
    #   if (!is.null(file_paths) && length(file_paths) > 0) {
    #     updateSelectInput(session, "audio_select", 
    #                       choices = setNames(file_paths, file_names),  # Use original names as labels
    #                       selected = file_paths[1])  # Default to first file
    #   }
    # }, ignoreInit = TRUE)  # Don't run when the module first loads
    
    
    # ##############################################################
    # # GENERATE UNIQUE SPECTROGRAMS FOR EACH AUDIO FILE SELECTED
    # ##############################################################
    # 
    # observeEvent(input$load_spectro, {
    #   req(data$audio_files(), input$audio_select)
    #   file_paths <- data$audio_files()
    #   selected_files <- file_paths[file_paths %in% input$audio_select]  # Match full paths
    # 
    #   # Generate UI dynamically: Sliders + Spectrograms
    #   output$spectrograms_ui <- renderUI({
    #     file_ui <- lapply(seq_along(selected_files), function(i) {
    #       file <- selected_files[i]
    #       file_name <- basename(file)
    #       wav_data <- readWave(file)
    # 
    #       # Get WAV file properties
    #       file_duration <- round(length(wav_data@left) / wav_data@samp.rate, 1)
    #       nyquist_freq <- wav_data@samp.rate / 2
    # 
    #       # Generate unique input IDs
    #       time_slider_id <- paste0("time_range_", i)
    #       freq_slider_id <- paste0("freq_range_", i)
    #       audio_id <- paste0("audio_", i)
    #       spectro_id <- paste0("spectrogram_", i)
    # 
    #       # Create a div container for each spectrogram + its sliders
    #       tagList(
    #         div(
    #           style = "border: 2px solid black; padding: 10px; border-radius: 5px; margin-bottom: 15px; background-color: #FFFAFA;", #F8F8F8
    # 
    #           # Slider + Audio button in a single row
    #           div(
    #             style = "display: flex; align-items: center; gap: 700px;",
    # 
    #             # Temporal range slider
    #             sliderInput(ns(time_slider_id), "Temporal Range:",
    #                         min = 0, max = file_duration, value = c(0, file_duration), width = "20%"),
    # 
    #             # Audio playback button
    #             tags$audio(
    #               controls = NA,  # Hide default audio player
    #               id = ns(audio_id),
    #               src = paste0("www/", basename(data$audio_files()[i])), # tags$audio automatically looks in the inst/app/ directory
    #               type = "audio/wav"
    #             ),
    #           ),
    # 
    #           # Spectrogram with loading spinner
    #           shinycssloaders::withSpinner(
    #             plotOutput(ns(spectro_id)),
    #             type = 4,
    #             color = "#00688B",
    #             caption = "Loading Spectrogram..."
    #           )
    #         )
    #       )
    #     })
    # 
    #     do.call(tagList, file_ui)
    #   })
    # 
    #   # sliderInput(ns(freq_slider_id), "Frequency Range:",
    #   #             min = 0, max = nyquist_freq, value = c(0, nyquist_freq)),
    # 
    #   # Render spectrograms (will rerender based on individual sliders)
    #   lapply(seq_along(selected_files), function(i) {
    #     local({
    #       idx <- i
    #       file <- selected_files[idx]
    #       time_slider_id <- paste0("time_range_", idx)
    #       freq_slider_id <- paste0("freq_range_", idx) # Not included yet
    #       spectro_id <- paste0("spectrogram_", idx)
    # 
    #       output[[spectro_id]] <- renderPlot({
    #         wav_data <- readWave(file)
    #         file_duration <- length(wav_data@left) / wav_data@samp.rate
    #         nyquist_freq <- wav_data@samp.rate / 2
    # 
    #         # Get slider values
    #         time_range <- input[[time_slider_id]]
    #         freq_range <- input[[freq_slider_id]]
    # 
    #         # Ensure valid ranges
    #         time_min <- max(0, min(time_range[1], file_duration))
    #         time_max <- max(0, min(time_range[2], file_duration))
    #         # freq_min <- max(0, min(freq_range[1], nyquist_freq))
    #         # freq_max <- max(0, min(freq_range[2], nyquist_freq))
    # 
    #         # Generate spectrogram with user-defined limits
    #         spectro(wav_data,
    #                 tlim = c(time_min, time_max),
    #                 flim = c(0, 30), ### will change this once the frequency sliders work
    #                 wl = 1024,
    #                 collevels = seq(-124, 0, 2),
    #                 palette = temp.colors,
    #                 main = data$audio_names()[which(data$audio_files() == file)]
    #         )
    #       })
    #     })
    #   })
    # })
#   })
# }
    
# seewave - https://rug.mnhn.fr/seewave/
# spectro - https://www.rdocumentation.org/packages/seewave/versions/2.2.3/topics/spectro
# ggspectro - https://www.rdocumentation.org/packages/seewave/versions/2.2.3/topics/ggspectro

## To be copied in the UI
# mod_spectro_ui("spectro_1")
    
## To be copied in the server
# mod_spectro_server("spectro_1")
