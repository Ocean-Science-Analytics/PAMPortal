#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  data <- mod_main_server("main_1")
  mod_spectro_server("spectro_1", data$audio_file)
  mod_analysis_server("analysis_1")
}
