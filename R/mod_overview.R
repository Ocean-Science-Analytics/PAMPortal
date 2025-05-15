#' overview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import plotly
#' @import dplyr
#' @import shinyWidgets
#' @import shinyjs
#' @import shinyBS
#' @import bs4Dash
mod_overview_ui <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
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
        .selectInput {
          max-height: 200px;
          overflow-y: auto;
        }
      "))
    ),
    
    div(class = "full-height",
        div(class = "content-wrapper",
            selectInput(ns("rds_select"), "Select Data File:", choices = NULL, width = "250px"),
            fluidRow(
              column(6,
                     bslib::card(
                       style = "height: 610px; overflow-y: auto; padding: 5px;",
                       class = "custom-card",
                       bslib::card_header(HTML(paste0(
                         as.character(shiny::icon("list-alt")), 
                         "<span style='font-weight: bold; font-size: 1.3em; margin-left: 8px;'>Species Events</span>"
                       ))),
                       uiOutput(outputId = ns("dynamic_accordion"))
                     )
              ),
              column(6,
                     bslib::card(
                       class = "custom-card",
                       style = "height: 610px; display: flex; flex-direction: column; padding: 5px;",
                       bslib::card_header(HTML(paste0(
                         as.character(shiny::icon("chart-simple")), 
                         "<span style='font-weight: bold; font-size: 1.3em; margin-left: 8px;'>Species Distribution</span>"
                       ))),
                       plotlyOutput(ns("card2"), height = "100%", width = "100%")
                     )
              )
            ),
            br(),
            
            prettyCheckbox(ns("all_events"), label = "Display All Events", value = FALSE, outline = TRUE, plain = TRUE, bigger = TRUE, icon = icon("square-check")),
            
            # Export CSV Button Above DataTable
            div(class = "table-header",
                div(style = "display: flex; gap: 10px; align-items: center;",
                    selectInput(ns("event_select"), "Select Acoustic Event:", choices = "", width = "250px"),
                    selectInput(ns("detector_select"), "Select Detector:", choices = "", width = "250px")
                ),
                downloadButton(ns("export_csv"), "Export CSV", class = "export-button"),
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
    
    observeEvent(data$rds_data(), {
      updateSelectInput(session, "rds_select", choices = names(data$rds_data()))
    })
    
    ##############################################################
    # Initially reads data files and updates acoustic events
    ##############################################################
    observeEvent(input$rds_select, {
      selected_name <- input$rds_select
      acou_data <- data$rds_data()[[selected_name]]
      event_titles <- sapply(acou_data@events, function(event) {
        slot(event, "id")  # Adjust slot name if necessary
      })
      updateSelectInput(session, "event_select", choices = event_titles)
    }, ignoreInit = TRUE)

    
    ##############################################################
    # Gather detector and species data from each event
    ##############################################################
    
    process_acoustic_data <- function(acou_data) {
      # Initialize an empty list to store results
      event_list <- list()
      
      # Loop through each event in acou_data
      for (event_name in names(acou_data@events)) {
        event <- acou_data@events[[event_name]]
        
        # Check if the event contains detectors
        if (!is.null(event@detectors)) {
          # Loop through each detector within the event
          for (detector_name in names(event@detectors)) {
            detector_data <- event@detectors[[detector_name]]
            
            # Ensure the detector contains the 'eventLabel' column
            if ("eventLabel" %in% colnames(detector_data)) {
              species_list <- unique(detector_data$eventLabel)  # Get unique species labels
              
              # Replace species codes with full names
              species_list <- recode(species_list, 
                                     "Mn" = "Humpback Whale", 
                                     "Oo" = "Killer Whale",
                                     "Unid Odont" = "Unidentified Odont.",
                                     "Delph spp." = "Delphinid Species")
              
              first_utc <- if ("UTC" %in% colnames(detector_data)) {
                format(as.POSIXct(detector_data$UTC[1], tz = "UTC"), "%Y-%m-%d %H:%M")
              } else {
                NA
              }
              
              # Create a data frame for this detector
              event_list[[length(event_list) + 1]] <- data.frame(
                Event = event_name,
                Detector = detector_name,
                Species = paste(species_list, collapse = ", "),
                Time = first_utc,
                stringsAsFactors = FALSE
              )
            }
          }
        }
      }
      
      # Combine into a single data frame
      if (length(event_list) > 0) {
        final_df <- bind_rows(event_list)
      } else {
        final_df <- data.frame(Event = character(0), Detector = character(0), Species = character(0))
      }
      
      return(final_df)
    }
    
    
    ##############################################################
    # Reactive: process species data only when file uploaded
    ##############################################################
    species_data <- reactive({
      req(input$rds_select)       # Require the user to have selected something
      req(data$rds_data())        # Require that rds_data exists
      
      selected_name <- input$rds_select
      acou_data <- data$rds_data()[[selected_name]]
      
      req(!is.null(acou_data))    # Make sure the selected data is not null
      
      process_acoustic_data(acou_data)
    })
    
    
    ##############################################################
    # Updates detectors based on event selected
    ##############################################################
    observeEvent(input$event_select, {
      #req(data$rds_data(), input$event_select)  # Ensure data exists

      selected_name <- input$rds_select
      acou_data <- data$rds_data()[[selected_name]]

      selected_event <- acou_data@events[[input$event_select]]
      shinyjs::enable("detector_select")  # Enable if not JSON

      if (!is.null(selected_event@detectors)) {
        detector_choices <- names(slot(selected_event, "detectors"))  # Extract detector names
        updateSelectInput(session, "detector_select", choices = detector_choices)
      } else {
        updateSelectInput(session, "detector_select", choices = character(0))
      }
    }, ignoreInit = TRUE)
    #
    
    ##############################################################
    # Dynamic Accordion UI
    ##############################################################
    # output$dynamic_accordion <- renderUI({
    #   req(data$data_file())
    #   species_df <- species_data()
    #   species_list <- split(species_df[, c("Event", "Time")], species_df$Species)
    #   
    #   cards <- purrr::imap(species_list, function(df, species_name) {
    #     safe_id <- gsub("[^A-Za-z0-9]", "_", species_name)
    #     output_id <- paste0("table_", safe_id)
    #     
    #     # Dynamically generate table output
    #     output[[output_id]] <- renderTable({
    #       df
    #     })
    #     
    #     # Create card with accordion inside
    #     bslib::card(
    #       full_screen = TRUE,
    #       style = "background-color: #f5f7fa;",
    #       bslib::card_header(species_name),
    #       bslib::accordion(
    #         open = FALSE,
    #         bslib::accordion_panel(
    #           title = "Individual Events",
    #           tableOutput(ns(output_id))
    #         )
    #       )
    #     )
    #   })
    #   
    #   tagList(cards)
    # })
    
    
    output$dynamic_accordion <- renderUI({
      req(data$rds_data())
      species_df <- species_data()
      species_list <- split(species_df[, c("Event", "Time")], species_df$Species)
      
      accordion_boxes <- purrr::imap(species_list, function(df, species_name) {
        safe_id <- gsub("[^A-Za-z0-9]", "_", species_name)
        output_id <- paste0("table_", safe_id)
        
        output[[output_id]] <- renderTable({
          df
        })
        
        div(
          style = "margin-bottom: 15px; padding: 5px; border: 2px solid #ddd; border-radius: 5px;",
          bslib::accordion(
            open = FALSE,
            bslib::accordion_panel(
              title = HTML(paste0("<b>", species_name, "</b>")),
              tableOutput(ns(output_id))
            )
          )
        )
      })
      
      tagList(accordion_boxes)
    })
    
    
    ##############################################################
    # Interactive Pie Chart
    ##############################################################
    output$card2 <- renderPlotly({
      req(data$rds_data())
      species_df <- species_data()  # <-- reuse the reactive result
      
      if (nrow(species_df) == 0) {
        return(
          plot_ly() %>%
            add_trace(type = "pie", labels = c("No Species Data Found"), values = c(1), textinfo = "label")
        )
      }
      
      species_counts <- table(species_df$Species)
      
      # Create a pie chart
      plot_ly(
        labels = names(species_counts),
        values = as.numeric(species_counts),
        type = "pie",
        textinfo = "label+percent",
        textposition = "inside", 
        hoverinfo = "label+value+percent",
        marker = list(colors = RColorBrewer::brewer.pal(length(species_counts), "RdYlBu"))) %>% # Also like 'Set3', 'Blues', and 'RdYlBu'. See https://r-graph-gallery.com/38-rcolorbrewers-palettes
        layout(title = NULL, 
               legend = list(
                 orientation = "h", 
                 x = 0.1,
                 y = -0.1,
                 font = list(size = 12)
               ),
               width = 490, 
               height = 520,
               margin = list(l = 20, r = 20, t = 20, b = 20),
               autosize = TRUE,
               marker = list(
                 size = 11  # Adjusts legend color swatch size
               ))
    })
    
    
    ##############################################################
    # Render data table based on event/detector selected
    ##############################################################
    # Disable the selectInputs if "Display all events" is selected
    observe({
      if (input$all_events) {
        # Disable the selectInputs when 'all_events' is selected
        disable("event_select")
        disable("detector_select")
      } else {
        # Enable them when 'all_events' is not selected
        enable("event_select")
        enable("detector_select")
      }
    })
    
    # Cool data table features - https://laustep.github.io/stlahblog/posts/DTcallbacks.html
    
    output$data_table <- DT::renderDataTable({
      req(data$rds_data())
      selected_name <- input$rds_select
      acou_data <- data$rds_data()[[selected_name]]
      
      if (isTRUE(input$all_events)) {
        showNotification("Loading data for all events. This may take some time.", type = "message", duration = 6)
        # Combine all detector data from all events
        all_data <- list()
        
        for (event_name in names(acou_data@events)) {
          event <- acou_data@events[[event_name]]
          
          if (!is.null(event@detectors)) {
            for (detector_name in names(event@detectors)) {
              detector_data <- event@detectors[[detector_name]]
              if (is.data.frame(detector_data)) {
                detector_data$Event <- event_name
                detector_data$Detector <- detector_name
                all_data[[length(all_data) + 1]] <- detector_data
              }
            }
          }
        }
        
        if (length(all_data) > 0) {
          final_df <- dplyr::bind_rows(all_data)
          return(DT::datatable(final_df, filter = 'top', class = 'cell-border stripe', options = list(pageLength = 15)))
        } else {
          return(DT::datatable(data.frame(Message = "No data available"), options = list(dom = 't')))
        }
        
      } else {
        # Show data only for the selected event and detector
        req(input$event_select, input$detector_select)
        selected_event <- acou_data@events[[input$event_select]]
        detector_data <- selected_event@detectors[[input$detector_select]]
        
        if (is.data.frame(detector_data)) {
          return(DT::datatable(detector_data, filter = 'top', class = 'cell-border stripe', options = list(pageLength = 15)))
        } else {
          return(DT::datatable(data.frame(Message = "No valid data available"), options = list(dom = 't')))
        }
      }
    })
    
    
    
    ##############################################################
    # Read data table and export to csv 
    ##############################################################
    # output$export_csv <- downloadHandler(
    #   filename = function() {
    #     paste0(input$event_select, "_", input$detector_select, ".csv")
    #   },
    #   content = function(file) {
    #     req(data$data_file(), input$event_select)
    # 
    #     acou_data <- data$data_file()
    #     file_type <- data$file_type()
    # 
    #     # Select event based on file type
    #     selected_event <- acou_data@events[[input$event_select]]
    # 
    #     req(input$detector_select)
    #     detector_data <- selected_event@detectors[[input$detector_select]]
    # 
    #     if (is.data.frame(detector_data)) {
    #       write.csv(detector_data, file, row.names = FALSE)
    #     } else {
    #       showNotification("No valid data available to download", type = "warning", duration = 5)
    #       return(NULL)
    #     }
    #   }
    # )
    
    output$export_csv <- downloadHandler(
      filename = function() {
        selected_name <- input$rds_select
        acou_data <- data$rds_data()[[selected_name]]
        
        if (input$all_events) {
          paste0(slot(acou_data, "id"), "_All_Events.csv")
        } else {
          paste0(input$event_select, "_", input$detector_select, ".csv")
        }
      },
      content = function(file) {
        showNotification("Preparing CSV download. This can take some time depending on the amount of data being exported.", type = "message", duration = 8)
        
        req(data$rds_data())
        selected_name <- input$rds_select
        acou_data <- data$rds_data()[[selected_name]]
        
        if (input$all_events) {
          # Combine all detector data across all events
          all_data <- list()
          
          for (event_name in names(acou_data@events)) {
            event <- acou_data@events[[event_name]]
            if (!is.null(event@detectors)) {
              for (detector_name in names(event@detectors)) {
                detector_data <- event@detectors[[detector_name]]
                if (is.data.frame(detector_data)) {
                  detector_data$Event <- event_name
                  detector_data$Detector <- detector_name
                  all_data[[length(all_data) + 1]] <- detector_data
                }
              }
            }
          }
          
          final_df <- if (length(all_data) > 0) {
            dplyr::bind_rows(all_data)
          } else {
            data.frame(Message = "No valid data available")
          }
          
          write.csv(final_df, file, row.names = FALSE)
          
        } else {
          req(input$event_select, input$detector_select)
          selected_event <- acou_data@events[[input$event_select]]
          detector_data <- selected_event@detectors[[input$detector_select]]
          
          if (is.data.frame(detector_data)) {
            write.csv(detector_data, file, row.names = FALSE)
          } else {
            showNotification("No valid data available to download", type = "warning", duration = 5)
            return(NULL)
          }
        }
      }
    )
    
  })
}
    
## To be copied in the UI
# mod_overview_ui("overview_1")
    
## To be copied in the server
# mod_overview_server("overview_1")
