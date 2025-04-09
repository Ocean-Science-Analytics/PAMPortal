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
            # Event Select and Detector Select in the Same Row
            # fluidRow(
            #   column(3, selectInput(ns("event_select"), "Select Acoustic Event:", choices = "")),
            #   column(3, selectInput(ns("detector_select"), "Select Detector:", choices = ""))
            # ),
            
            fluidRow(
              column(5,
                     bslib::card(
                       style = "height: 610px; overflow-y: auto; padding: 10px;",
                       class = "custom-card",
                       bslib::card_header("Species and Events"),
                       htmlOutput(ns("species_text"))
                     )
              ),
              column(7,
                     bslib::card(
                       style = "height: 610px; overflow-y: auto; padding: 10px;",
                       class = "custom-card",
                       bslib::card_header("Species Distribution"),
                       plotlyOutput(ns("card2"))
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
    
    ##############################################################
    # Initially reads data files and updates acoustic events
    ##############################################################
    observeEvent(data$data_file(), {
      acou_data <- data$data_file()
      file_type <- data$file_type()  # Get stored file type
      #browser()
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
              
              # Create a data frame for this detector
              event_list[[length(event_list) + 1]] <- data.frame(
                Event = event_name,
                Detector = detector_name,
                Species = paste(species_list, collapse = ", ")  # Combine species if multiple
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
    # Updates detectors based on event selected
    ##############################################################
    observeEvent(input$event_select, {
      req(data$data_file(), input$event_select)  # Ensure data exists

      acou_data <- data$data_file()
      file_type <- data$file_type()

      selected_event <- acou_data@events[[input$event_select]]
      shinyjs::enable("detector_select")  # Enable if not JSON

      if (!is.null(selected_event@detectors)) {
        detector_choices <- names(slot(selected_event, "detectors"))  # Extract detector names
        updateSelectInput(session, "detector_select", choices = detector_choices)
      } else {
        updateSelectInput(session, "detector_select", choices = character(0))
      }
    }, ignoreInit = TRUE)


    
    ##############################################################
    # Text output for Card 1
    ##############################################################
    output$species_text <- renderUI({
      req(data$data_file())
      
      species_data <- process_acoustic_data(data$data_file())
      
      if (nrow(species_data) == 0) {
        HTML('<p style="font-size: 18px;">No species data available.</p>')
      } else {
        # Sort the species list alphabetically
        species_list <- sort(unique(unlist(strsplit(species_data$Species, ",\\s*"))))
        
        # Create a list to store the HTML content
        text_html <- '<div>'
        
        # Title
        #text_html <- paste0(text_html, '<p style="font-size: 24px; font-weight: bold;">The species identified in this acoustic study are:</p>')
        
        for (species in species_list) {
          # Get the event titles that correlate with the species
          events_for_species <- species_data$Event[species_data$Species == species]
          
          # Create a selectInput for this species
          select_input <- selectInput(
            inputId = paste0("event_select_", gsub("\\s+", "_", species)),  # Create a unique ID for each species
            label = NULL,
            choices = unique(events_for_species),
            selected = NULL,  # Default to no event selected
            width = "100%"  # Full width of the container
          )
          
          # Add the species name, selectInput, and event list to the HTML content
          text_html <- paste0(
            text_html,
            '<p style="font-size: 18px; font-weight: bold;">', species, '</p>',
            select_input,
            # '<ul style="font-size: 16px; padding-left: 20px;">',
            # paste(sprintf("<li>%s</li>", events_for_species), collapse = ""),
            "</ul>"
            #"<br>"  # Adds a break between species
          )
        }
        
        text_html <- paste0(text_html, '</div>')
        HTML(text_html)
      }
    })
    
    
    
    ##############################################################
    # Interactive Pie Chart
    ##############################################################
    output$card2 <- renderPlotly({
      req(data$data_file())  # Ensure a file is loaded
      
      # Process the acoustic data to get species information
      acou_data <- data$data_file()
      species_data <- process_acoustic_data(acou_data)
      
      # Check if species_data is empty
      if (nrow(species_data) == 0) {
        return(
          plot_ly() %>%
            add_trace(type = "pie", labels = c("No Species Data Found"), values = c(1), textinfo = "label")
        )
      }
      
      # Count occurrences of each species
      species_counts <- table(species_data$Species)
      
      # Create a pie chart
      plot_ly(
        labels = names(species_counts),
        values = as.numeric(species_counts),
        type = "pie",
        textinfo = "label+percent",
        hoverinfo = "label+value+percent",
        marker = list(colors = RColorBrewer::brewer.pal(length(species_counts), "RdYlBu"))) %>% # Also like 'Set3', 'Blues', and 'RdYlBu'. See https://r-graph-gallery.com/38-rcolorbrewers-palettes
        layout(title = NULL, width = 650, height = 560,
               legend = list(
                 font = list(size = 10),  # Change legend text size
                 itemsizing = "constant",  # Ensures consistent sizing
                 tracegroupgap = 5  # Adjusts spacing between legend items
               ),
               marker = list(
                 size = 10  # Adjusts legend color swatch size
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
      req(data$data_file())
      acou_data <- data$data_file()
      file_type <- data$file_type()
      
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
        acou_data <- data$data_file()
        
        if (input$all_events) {
          paste0(slot(acou_data, "id"), "_All_Events.csv")
        } else {
          paste0(input$event_select, "_", input$detector_select, ".csv")
        }
      },
      content = function(file) {
        showNotification("Preparing CSV download. This can take some time depending on the amount of data being exported.", type = "message", duration = 8)
        
        req(data$data_file())
        acou_data <- data$data_file()
        file_type <- data$file_type()
        
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
