#' soundscape UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import pixture
mod_soundscape_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel(
        "PSD Gallery",
        uiOutput(ns("site_ui")),
        pixture::pixgalleryOutput(ns("gallery"))
      ),
      tabPanel(
        "SPL Measurements",
        # You can insert UI components here later
        p("SPL content goes here...")
      )
    )
  )
}
    
#' soundscape Server Functions
#'
#' @noRd 
mod_soundscape_server <- function(id, data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
    observeEvent(data$soundscape_data(), {
      sites <- data$soundscape_data()
      if (!is.null(sites) && length(sites) > 0) {
        updateSelectInput(session, "site_select", choices = sites)
      }
    })
    
    output$site_ui <- renderUI({
      req(data$soundscape_data())
      selectInput(ns("site_select"), "Select Site:", choices = data$soundscape_data())
    })
    
    output$gallery <- pixture::renderPixgallery({
      req(input$site_select)
      
      # Construct path to the "PSD_plots" folder for the selected site
      base_path <- data$selected_dir()  # this assumes `selected_dir()` is globally available
      site_folder <- file.path(base_path, "Soundscape", input$site_select, "PSD_plots")
      req(dir.exists(site_folder))
      
      # Get all .png files in the folder
      png_files <- list.files(site_folder, pattern = "\\.png$", full.names = TRUE)
      req(length(png_files) > 0)
      
      # Create a unique resource alias for this folder (in case multiple sites are selected over time)
      alias <- paste0("psd_", gsub("[^a-zA-Z0-9]", "_", input$site_select))
      shiny::addResourcePath(alias, site_folder)
      
      # Create relative paths for the browser
      relative_paths <- file.path(alias, basename(png_files))
      
      # Render gallery
      pixture::pixgallery(
        relative_paths,
        layout = "grid"
      )
    })
    
  })
}
    
## To be copied in the UI
# mod_soundscape_ui("soundscape_1")
    
## To be copied in the server
# mod_soundscape_server("soundscape_1")
