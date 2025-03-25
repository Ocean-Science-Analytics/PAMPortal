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
        .custom-card {
          box-shadow: 2px 2px 10px rgba(0, 0, 0, 0.1);
          border-radius: 10px;
          background-color: #fafafa;
          padding: 15px;
        }
        .custom-card .card-header {
          background-color: #f0f0f0;
          font-weight: bold;
          font-size: 16px;
          padding: 10px;
          border-top-left-radius: 10px;
          border-top-right-radius: 10px;
        }
        .full-height {
          height: 100vh;
          display: flex;
          flex-direction: column;
        }
        .content-wrapper {
          flex-grow: 1;
          display: flex;
          flex-direction: column;
        }
        .data-table-container {
          flex-grow: 1;
          overflow-y: auto;
          border: 2px solid black;
          padding: 10px;
          border-radius: 5px;
          background-color: white;
          position: relative;
        }
        .table-header {
          display: flex;
          justify-content: space-between;
          align-items: center;
          margin-bottom: 5px;
        }
        .export-button {
          background-color: #90EE90 !important;
          color: black !important;
          border: 1px solid #77DD77;
          padding: 5px 10px;
          font-weight: bold;
          border-radius: 5px;
          transition: background-color 0.3s ease;
        }
        .export-button:hover {
          background-color: #77DD77 !important;
        }
      "))
    ),
    
    div(class = "full-height",
        div(class = "content-wrapper",
            selectInput(ns("event_select"), "Select Acoustic Event:", choices = ""),
            
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
            br(),
            # Detector Select (Left) and Export CSV Button (Right) Above DataTable
            div(class = "table-header",
                selectInput(ns("detector_select"), "Select Detector:", choices = ""),
                downloadButton(ns("export_csv"), "Export CSV", class = "export-button")
            ),
            
            # Data Table
            div(class = "data-table-container",
                DT::dataTableOutput(ns("data_table"))
            )
        )
    )
  )
}
    
#' overview Server Functions
#'
#' @noRd 
mod_overview_server <- function(id, data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    ##############################################################
    # Initially reads data files and updates acoustic events
    ##############################################################
    observeEvent(data$data_file(), {
      acou_data <- data$data_file()
      file_type <- data$file_type()  # Get stored file type
      #browser()
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
    
    
    ##############################################################
    # Updates detectors based on event selected
    ##############################################################
    observeEvent(input$event_select, {
      req(data$data_file(), input$event_select)  # Ensure data exists
      
      acou_data <- data$data_file()
      selected_event <- acou_data@events[[input$event_select]]  # Extract event
      selected_event2 <- selected_event@detectors
      #browser()
      if (!is.null(selected_event@detectors)) {
        detector_choices <- names(slot(selected_event, "detectors"))  # Extract detector names
        updateSelectInput(session, "detector_select", choices = detector_choices)
      } else {
        updateSelectInput(session, "detector_select", choices = character(0))
      }
    }, ignoreInit = TRUE)
    
    
    ##############################################################
    # Render data table based on event/detector selected
    ##############################################################
    output$data_table <- DT::renderDataTable({
      req(data$data_file(), input$event_select, input$detector_select)
      
      acou_data <- data$data_file()
      selected_event <- acou_data@events[[input$event_select]]
      detector_data <- selected_event@detectors[[input$detector_select]]
      
      if (is.data.frame(detector_data)) {
        return(DT::datatable(detector_data, options = list(pageLength = 15)))
      } else {
        return(DT::datatable(data.frame(Message = "No valid data available"), options = list(dom = 't')))
      }
    })
    
    ##############################################################
    # Read data table and export to csv 
    ##############################################################
    output$export_csv <- downloadHandler(
      filename = function() {
        paste0(input$event_select, "_", input$detector_select, ".csv")
      },
      content = function(file) {
        req(data$data_file(), input$event_select, input$detector_select)
        
        acou_data <- data$data_file()
        selected_event <- acou_data@events[[input$event_select]]
        detector_data <- selected_event@detectors[[input$detector_select]]
        
        if (is.data.frame(detector_data)) {
          write.csv(detector_data, file, row.names = FALSE)
        } else {
          showNotification("No data available to download", type = "warning", duration = 5)
          return(NULL)  # Prevents file creation
        }
      }
    )
  })
}
    
## To be copied in the UI
# mod_overview_ui("overview_1")
    
## To be copied in the server
# mod_overview_server("overview_1")
