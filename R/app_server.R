#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  data <- mod_main_server("main_1") # Create data list to read into other servers
  mod_overview_server("overview_1")
  mod_spectro_server("spectro_1", data)
  mod_analysis_server("analysis_1")
}
