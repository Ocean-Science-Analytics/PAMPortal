#' spectro UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import seewave
#' @import tuneR
#' @import ggplot2
mod_spectro_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("load_spectro"), "Load Spectrogram", class = "btn-primary", style = "width: 200px;"),  # Load button
    tags$br(),  # Spacer for better layout
    plotOutput(ns("spectrogram"))
  )
}
    
#' spectro Server Functions
#'
#' @noRd 
mod_spectro_server <- function(id, audio_file){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(input$load_spectro, {  # Only trigger when button is clicked
      output$spectrogram <- renderPlot({
        req(audio_file())  # Ensure an audio file is uploaded
        
        # Read the .wav file
        wav_data <- readWave(audio_file())
        
        # Generate spectrogram
        spectro(wav_data,
                flim = c(5, 20),
                wl = 1024,
                collevels = seq(-124, 0, 2),
                palette = temp.colors,
                main = "Spectrogram 1")
      })
    })
  })
}
    
# seewave - https://rug.mnhn.fr/seewave/
# spectro - https://www.rdocumentation.org/packages/seewave/versions/2.2.3/topics/spectro
# ggspectro - https://www.rdocumentation.org/packages/seewave/versions/2.2.3/topics/ggspectro

## To be copied in the UI
# mod_spectro_ui("spectro_1")
    
## To be copied in the server
# mod_spectro_server("spectro_1")
