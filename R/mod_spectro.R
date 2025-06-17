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
  
  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ########## See Band-Pass filter video on this page for zoomable spectrogram https://rpubs.com/panchorivasf/rthoptera_preprocess ##################
  
  
  tagList(
    tags$head(
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
    card_spectro(ns, "spectro1", 1),
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
          
          # audio_name <- paste0("audio_", index, ".wav")
          # www_path <- file.path(base_path(), "inst", "app", "www", audio_name)
          # 
          # # Remove old file if it exists
          # if (file.exists(www_path)) {
          #   file.remove(www_path)
          # }
          # 
          # # Copy new file to www
          # file.copy(from = input[[fileInput]], to = www_path, overwrite = TRUE)
          # 
          # output[[paste0("audio_", index)]] <- renderUI({
          #   tags$audio(
          #     controls = NA,
          #     style = "width: 50%; margin-top: 5px;",
          #     tags$source(src = audio_name, type = "audio/wav"),
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
            wav_path <- isolate(input[[fileInput]])
            wl_val <- isolate(input[[paste0("wl_", index)]])
            if (is.null(wl_val) || is.na(wl_val)) wl_val <- 1024
            
            wave <- tuneR::readWave(wav_path)
            
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
