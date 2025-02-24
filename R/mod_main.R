#' main UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_main_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Include inline CSS for the Browse button
    tags$head(
      tags$style(HTML("
        .btn-file {  
          background-color: #00688B; 
          border-color: black;
          color: white !important;
        }
        .input-group-btn .btn:hover {
          background-color: lightskyblue !important;
        }
      "))
    ),
    
    bslib::page_fluid(
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = '400px',
          style = "background-color: #D3D3D3; padding: 10px;",
          fileInput(ns("directory"), "Directory Link:"),
          selectInput(ns("select1"), "Select Option 1:", choices = NULL),
          selectInput(ns("select2"), "Select Option 2:", choices = NULL)
        )
      ),
      plotly::plotlyOutput(outputId = ns("ac_plot2"))
    )
  )
}
    
#' main Server Functions
#'
#' @noRd 
mod_main_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_main_ui("main_1")
    
## To be copied in the server
# mod_main_server("main_1")
