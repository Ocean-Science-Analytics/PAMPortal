#' helpers 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd


#' @title Add red "beta" ribbon to the bottom left of the UI
#'
#' @description To be used within Shiny UI
#'
#' @noRd
#' @return html
#'
#' @examples
#' htmltools::html_print(add_beta_ribbon())
add_beta_ribbon <- function(){
  
  # inline CSS
  style <- "
    width: 200px;
    background: #b81b11;
    position: fixed;
    bottom: 25px;
    left: -50px;
    text-align: center;
    line-height: 50px;
    letter-spacing: 3px;
    font-size: 1.5em;
    font-weight: 700;
    color: #f0f0f0;
      -webkit-transform: rotate(45deg);
      -moz-transform: rotate(45deg);
    transform: rotate(45deg);
      -webkit-box-shadow: 0 0 15px rgba(0,0,0,0.5);
      -moz-box-shadow: 0 0 15px rgba(0,0,0,0.5);
    box-shadow: 0 0 15px rgba(0,0,0,0.5);
    z-index: 9999;
    "
  
  beta_div <- htmltools::tags$div(
    class = 'corner-ribbon',
    style = style,
    'BETA'
  )
  
  return(beta_div)
}


#' Color palettes
#' 
#' PAMPortal color palette. Primarily used for static UI elements.
#' 
#' @param .data A raster or numeric vector providing the data domain for the color scale.
#'
#' @return For `palette_main()`, a named list of hex colors.  
#' @export
#'
#' @examples
#' palette_main()$green
palette_main <- function(){
  list(
    slate_blue = "#3e606f",# Mainly for loading page 
    deep_cyan = "#00688B", # Mainly for buttons and triggers
    aquamarine = "#7AC5CD", # Mainly for title bar of app
    gray = "#b0b0b0", # Background of buttons and titles
    gray_light = "#F0F0F0", # Background of sidebar
    seashell = "#EEE5DE" # Background of Guide button 
  )
}



card_spectro <- function(ns, id, num) {
  ns_id <- function(x) ns(paste0(id, "-", x))
  
  bslib::card(
    div(
      style = "border: 2px solid black; padding: 10px; border-radius: 5px; margin-bottom: 15px; background-color: #FFFAFA;",
      
      h4(paste("Spectrogram", num)),
      
      # Row 1: Event + Species
      fluidRow(
        column(6, selectInput(ns_id("event"), "1.) Event", choices = NULL)),
        column(6, selectInput(ns_id("species"), "2.) Species", choices = NULL))
      ),
      
      # Row 2: Detection + File
      fluidRow(
        column(6, selectInput(ns_id("detection"), "3.) Detection ID", choices = NULL)),
        column(6, selectInput(ns_id("file"), "4.) File", choices = NULL))
      ),
      
      # Row 3: Time Range + Audio playback
      fluidRow(
        column(5, sliderInput(ns_id("time_range"), "Time (s)", min = 0, max = 40, value = c(0, 5))),
        column(7,
               tags$audio(
                 controls = NA,
                 id = ns_id("audio_player"),
                 src = "",  # dynamically updated in server
                 type = "audio/wav"
               )
        )
      ),
      
      # Spectrogram Plot (with spinner)
      shinycssloaders::withSpinner(
        plotOutput(ns_id("spectrogram"), height = "250px"),
        type = 4,
        color = "#00688B",
        caption = "Loading Spectrogram..."
      )
    )
  )
}

