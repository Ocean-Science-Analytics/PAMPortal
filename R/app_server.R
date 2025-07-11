#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  data <- mod_main_server("main_1") # Create data list to read into other servers
  mod_overview_server("overview_1", data)
  mod_spectro_server("spectro_1", data)
  mod_analysis_server("analysis_1", data)
  mod_soundscape_server("soundscape_1", data)
  
  observeEvent(input$help, {
    guide$init()$start()
  })
  Sys.sleep(5)
  waiter::waiter_hide()
  
}
