#' overview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_overview_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(HTML("
        custom-card {
          box-shadow: 2px 2px 10px rgba(0, 0, 0, 0.1); /* Soft shadow */
          border-radius: 10px; /* Smooth rounded corners */
          background-color: #fafafa; /* Light background */
          padding: 15px;
        }
        .custom-card .card-header {
          background-color: #f0f0f0; /* Light grey */
          font-weight: bold;
          font-size: 16px;
          padding: 10px;
          border-top-left-radius: 10px; /* Match card rounding */
          border-top-right-radius: 10px;
        }
      "))
    ),
    
    # Select Input at the top
    selectInput(ns("event_select"), "Select Acoustic Event:", choices = NULL),
    
    # Two bslib cards side by side
    fluidRow(
      column(6,
             bslib::card(
               class = "custom-card",
               bslib::card_header("Card 1"),
               p("Content for the first card.")
             )
      ),
      column(6,
             bslib::card(
               class = "custom-card",
               bslib::card_header("Card 2"),
               p("Content for the second card.")
             )
      )
    ),
    
    selectInput(ns("detector_select"), "Select Detector:", choices = NULL),
    
    # Data Table below the cards
    DT::dataTableOutput(ns("data_table"))
  )
}
    
#' overview Server Functions
#'
#' @noRd 
mod_overview_server <- function(id, data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(data$data_file(), {
      
      acou_data <- data$data_file()
      file_type <- data$file_type()  # Get stored file type
      browser()
      if (file_type == "rds") {
        # Extract names/titles from S4 objects
        event_titles <- sapply(acou_data@events, function(event) {
          slot(event, "id")  # Adjust slot name if necessary
        })
        updateSelectInput(session, "event_select", choices = event_titles)
        
      } else if (file_type == "json") {
        updateSelectInput(session, "event_select", choices = names(acou_data))
      }
      
    }, ignoreInit = TRUE)
  })
}
    
## To be copied in the UI
# mod_overview_ui("overview_1")
    
## To be copied in the server
# mod_overview_server("overview_1")
