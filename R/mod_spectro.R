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
#' @import tidyr
mod_spectro_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$head(
      # tags$script(HTML("
      #   Shiny.addCustomMessageHandler('stopAudio', function(message) {
      #     var audioEl = document.getElementById(message.id);
      #     if (audioEl && !audioEl.paused) {
      #       audioEl.pause();
      #       audioEl.currentTime = 0;
      #     }
      #   });
      # ")),
      
      tags$style(HTML("
        .full-height {
          height: 100vh;
          display: flex;
          flex-direction: column;
        }
        .custom-btn {
          background-color: #00688B !important;
          color: white !important;
          border-color: black !important;
        }
        .custom-btn:hover {
          background-color: lightskyblue !important;
          cursor: pointer;
        }
      "))
    ),
    # actionButton(ns("add_spectro"), "+ Add Spectrogram"),
    # br(),
    # uiOutput(ns("spectrograms_ui"))
    div(id = ns("spectro_card"),
      card_spectro(ns, "spectro1", 1)
    ),
    card_spectro(ns, "spectro2", 2)
    # fluidRow(
    #   column(6, card_spectro(ns, "spectro3", 3)),
    #   column(6, card_spectro(ns, "spectro4", 4))
    # )
  )
}
    
#' spectro Server Functions
#'
#' @noRd 
mod_spectro_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    tree <- reactive(data$acoustic_file_tree())
    
    audio_dir <- file.path(tempdir(), "audio")
    dir.create(audio_dir, showWarnings = FALSE, recursive = TRUE)
    shiny::addResourcePath("temp_audio", audio_dir)
    
    # AUDIO FILE DELETION LOGIC
    # session$onSessionEnded(function() {
    #   unlink(audio_dir, recursive = TRUE, force = TRUE)
    # })
    
    for (i in 1:2) {
      local({
        index <- i
        
        locationInput <- paste0("location_", index)
        speciesInput <- paste0("species_", index)
        folderInput <- paste0("folder_", index)
        fileInput <- paste0("file_", index)
        plotOutput <- paste0("plot_", index)
        
        observeEvent(tree(), {
          updateSelectInput(session, locationInput, choices = names(tree()))
        })
        
        observeEvent(input[[locationInput]], {
          loc <- input[[locationInput]]
          updateSelectInput(session, speciesInput, choices = names(tree()[[loc]]))
        }, ignoreInit = TRUE)
        
        observeEvent(input[[speciesInput]], {
          loc <- input[[locationInput]]
          sp <- input[[speciesInput]]
          updateSelectInput(session, folderInput, choices = names(tree()[[loc]][[sp]]))
        }, ignoreInit = TRUE)
        
        observeEvent(input[[folderInput]], {
          loc <- input[[locationInput]]
          sp <- input[[speciesInput]]
          fldr <- input[[folderInput]]
          wavs <- tree()[[loc]][[sp]][[fldr]]
          
          # Display just filenames, keep full paths as values
          named_wavs <- setNames(wavs, basename(wavs))
          
          updateSelectInput(session, fileInput, choices = named_wavs)
        }, ignoreInit = TRUE)
        
        observeEvent(input[[paste0("render_", index)]], {
          req(input[[fileInput]])
          
          wav_path_val <- input[[fileInput]]
          wl_val <- input[[paste0("wl_", index)]]
          if (is.null(wl_val) || is.na(wl_val)) wl_val <- 1024
          audio_name <- paste0("audio_", index, ".wav")
          temp_audio_path <- file.path(audio_dir, audio_name)
          
          ####!!!!!!!!!!!!!!!!!!!
          # Stop any playing audio first
          session$sendCustomMessage("stopAudio", list(id = ns(paste0("audio_element_", index))))
          
          later::later(function() {
            if (file.exists(temp_audio_path)) {
              file.remove(temp_audio_path)
            }
            file.copy(from = wav_path_val, to = temp_audio_path, overwrite = TRUE)
            
            # Now render the audio player and spectrogram
            output[[paste0("audio_", index)]] <- renderUI({
              tagList(
                tags$audio(
                  id = ns(paste0("audio_element_", index)),
                  controls = T,
                  style = "width: 50%; margin-top: 5px;",
                  tags$source(src = file.path("temp_audio", audio_name), type = "audio/wav"),
                  "Your browser does not support the audio element."
                )
              #   tags$script(HTML(sprintf("
              #   setTimeout(function() {
              #     const audio = document.getElementById('%s');
              #     const plotDiv = document.getElementById('%s');
              # 
              #     if (audio && plotDiv) {
              #       audio.addEventListener('timeupdate', function () {
              #         const currentTime = audio.currentTime;
              #         Plotly.relayout(plotDiv, {
              #           'shapes[0].x0': currentTime,
              #           'shapes[0].x1': currentTime
              #         });
              #       });
              #     }
              #   }, 500);
              # ", ns(paste0("audio_element_", index)), ns(paste0("plot_", index))))
              #   )
              )
            })
          }, delay = 0.1)
          
          # # Remove old if exists and copy
          # if (file.exists(temp_audio_path)) {
          #   file.remove(temp_audio_path)
          # }
          # file.copy(from = input[[fileInput]], to = temp_audio_path, overwrite = TRUE)
          
          # output[[paste0("audio_", index)]] <- renderUI({
          #   tagList(
          #     tags$audio(
          #       id = ns(paste0("audio_element_", index)),
          #       controls = NA,
          #       style = "width: 50%; margin-top: 5px;",
          #       tags$source(src = file.path("temp_audio", audio_name), type = "audio/wav"),
          #       "Your browser does not support the audio element."
          #     ),
          #     tags$script(HTML(sprintf("
          #     setTimeout(function() {
          #       const audio = document.getElementById('%s');
          #       const plotDiv = document.getElementById('%s');
          # 
          #       if (audio && plotDiv) {
          #         audio.addEventListener('timeupdate', function () {
          #           const currentTime = audio.currentTime;
          #           Plotly.relayout(plotDiv, {
          #             'shapes[0].x0': currentTime,
          #             'shapes[0].x1': currentTime
          #           });
          #         });
          #       }
          #     }, 500);  // delay to ensure plot is ready
          #   ", ns(paste0("audio_element_", index)), ns(paste0("plot_", index)))))
          #   )
          # })
          
          # Now point to the file via its web-accessible path
          # output[[paste0("audio_", index)]] <- renderUI({
          #   tags$audio(
          #     id = ns(paste0("audio_element_", index)),
          #     controls = NA,
          #     style = "width: 50%; margin-top: 5px;",
          #     tags$source(src = file.path("temp_audio", audio_name), type = "audio/wav"),
          #     "Your browser does not support the audio element."
          #   )
          # })
          
          # Show the spinner immediately by rendering plot_ui_ before starting the processing
          output[[paste0("plot_ui_", index)]] <- renderUI({
              shinycssloaders::withSpinner(
              plotlyOutput(ns(plotOutput), height = "100%"),
              type = 4, color = "#001f3f", size = 1
            )
          })
          
          # Let the UI render the spinner before doing heavy work
          later::later(function() {
            # wav_path <- isolate(input[[fileInput]])
            # wl_val <- isolate(input[[paste0("wl_", index)]])
            # if (is.null(wl_val) || is.na(wl_val)) wl_val <- 1024
            
            wave <- tuneR::readWave(wav_path_val)
            
            output[[plotOutput]] <- renderPlotly({
              spectrogram_plotly(wave, wl = wl_val)
            })
          }, delay = 0.1)
        })
      })
     }
  })
}
    
# seewave - https://rug.mnhn.fr/seewave/
# spectro - https://www.rdocumentation.org/packages/seewave/versions/2.2.3/topics/spectro
# ggspectro - https://www.rdocumentation.org/packages/seewave/versions/2.2.3/topics/ggspectro

## To be copied in the UI
# mod_spectro_ui("spectro_1")
    
## To be copied in the server
# mod_spectro_server("spectro_1")
