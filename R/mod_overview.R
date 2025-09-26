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
        .checkbox-group-tight {
          display: flex;
          flex-wrap: wrap;
          align-items: center;
          gap: 4px !important;
          margin-bottom: 10px;
        }
        .accordion-panel table {
          table-layout: fixed;
          width: 100%;
          word-wrap: break-word;
          white-space: normal;
        }
        .accordion-panel td, .accordion-panel th {
          word-wrap: break-word;
          white-space: normal;
        }
        table th:nth-child(1), table td:nth-child(1) {
          white-space: normal !important;
          word-wrap: break-word !important;
          max-width: 120px;  /* keep Event column narrower */
        }
        table th:nth-child(2), table td:nth-child(2) {
          white-space: normal !important;
          word-wrap: break-word !important;
          max-width: 120px;  /* keep Event column narrower */
        }
      "))
    ),
    
    div(class = "full-height",
        div(class = "content-wrapper",
            tags$h4("Species Analysis:"),
            div(id = ns("overview_input"),
                style = "border: 2px solid black; border-radius: 8px; padding: 15px; margin-bottom: 20px;",
                fluidRow(
                  column(3,
                         selectInput(ns("rds_select"), "Select Main Data File:", choices = NULL, width = "100%")
                  ),
                  column(1, align = "right",
                         div(style = "height: 100%; border-right: 2px solid black;")
                  ),
                  column(3,
                         prettyCheckbox(
                           ns("compare"),
                           label = "Compare Two Deployments",
                           value = FALSE,
                           outline = TRUE,
                           plain = TRUE,
                           bigger = TRUE,
                           icon = icon("square-check")
                         ),
                         selectInput(ns("rds2_select"), "Select Comparison Data File:", choices = NULL, width = "100%")
                  )
                )
            ),
            div(id = ns("overview_cards"),
                uiOutput(ns("dynamic_cards_layout"))
            ),
            
            br(),
            
            div(id = ns("overview_datatable"),
            # Add the three checkboxes
            tags$h4("Data Table:"),
            div(style = "border: 2px solid black; border-radius: 8px; padding: 15px; margin-bottom: 5px;",
              div(class = "checkbox-group-tight",
                  prettyCheckbox(ns("all_events"), label = "Display All Events", value = FALSE,
                                 outline = TRUE, plain = TRUE, bigger = TRUE, icon = icon("square-check")),
                  prettyCheckbox(ns("filter_species"), label = "Filter by Species", value = TRUE,
                                 outline = TRUE, plain = TRUE, bigger = TRUE, icon = icon("square-check")),
                  prettyCheckbox(ns("filter_events"), label = "Filter by Event", value = FALSE,
                                 outline = TRUE, plain = TRUE, bigger = TRUE, icon = icon("square-check")),
              ),
              
              # Controls shown conditionally
              div(class = "table-header",
                  div(style = "display: flex; gap: 10px; align-items: center;",
                      
                      selectInput(ns("file_select"), "1.) Select Deployment File:", choices = "", width = "220px"),
                      
                      div(style = "border-left: 1px solid black; height: 40px; margin-left: 10px; margin-right: 10px;"),
                      
                      # Only show if filter_events is checked
                      conditionalPanel(
                        condition = sprintf("input['%s']", ns("filter_events")),
                        tagList(
                          selectInput(ns("event_select"), "2.) Select Acoustic Event:", choices = "", width = "310px"),
                          selectInput(ns("detector_select"), "3.) Select Detector:", choices = "", width = "240px")
                        )
                      ),
                      
                      # Only show if filter_species is checked
                      conditionalPanel(
                        condition = sprintf("input['%s']", ns("filter_species")),
                        selectInput(ns("species_select"), "2.) Select Species:", choices = "", width = "240px")
                      )
                  ),
                  
                  downloadButton(ns("export_csv"), "Export CSV", class = "export-button")
              )
            )
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
    
    base_path <- reactive({
      req(data$selected_dir())
    })
    
    output$dynamic_cards_layout <- renderUI({
      if (isTRUE(input$compare)) {
        fluidRow(
          # Row 1: Two Species Events cards side by side
          column(6,
                 bslib::card(
                   style = "height: 610px; overflow-y: auto; padding: 5px;",
                   class = "custom-card",
                   bslib::card_header(HTML(paste0(
                     as.character(shiny::icon("list-alt")),
                     sprintf("<span style='font-weight: bold; font-size: 1.3em; margin-left: 8px;'>Species Events (%s)</span>", input$rds_select)
                   ))),
                   uiOutput(ns("dynamic_accordion"))
                 )
          ),
          column(6,
                 bslib::card(
                   style = "height: 610px; overflow-y: auto; padding: 5px;",
                   class = "custom-card",
                   bslib::card_header(HTML(paste0(
                     as.character(shiny::icon("list-alt")),
                     sprintf("<span style='font-weight: bold; font-size: 1.3em; margin-left: 8px;'>Species Events (%s)</span>", input$rds2_select)
                   ))),
                   uiOutput(ns("dynamic_accordion_cmp"))
                 )
          ),
          
          # Row 2: Two pie charts for Species Distribution
          column(6,
                 bslib::card(
                   class = "custom-card",
                   style = "height: 610px; display: flex; flex-direction: column; padding: 5px; margin-top: 20px;",
                   bslib::card_header(HTML(paste0(
                     as.character(shiny::icon("chart-simple")),
                     sprintf("<span style='font-weight: bold; font-size: 1.3em; margin-left: 8px;'>Species Distribution (%s)</span>", input$rds_select)
                   ))),
                   plotlyOutput(ns("card2"), height = "100%", width = "100%")
                 )
          ),
          column(6,
                 bslib::card(
                   class = "custom-card",
                   style = "height: 610px; display: flex; flex-direction: column; padding: 5px; margin-top: 20px;",
                   bslib::card_header(HTML(paste0(
                     as.character(shiny::icon("chart-simple")),
                     sprintf("<span style='font-weight: bold; font-size: 1.3em; margin-left: 8px;'>Species Distribution (%s)</span>", input$rds2_select)
                   ))),
                   plotlyOutput(ns("card2_cmp"), height = "100%", width = "100%")
                 )
          )
        )
      } else {
        fluidRow(
          column(6,
                 bslib::card(
                   style = "height: 610px; overflow-y: auto; padding: 5px;",
                   class = "custom-card",
                   bslib::card_header(HTML(paste0(
                     as.character(shiny::icon("list-alt")), 
                     "<span style='font-weight: bold; font-size: 1.4em; margin-left: 8px;'>Species Events</span>"
                   ))),
                   uiOutput(ns("dynamic_accordion"))
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
        )
      }
    })
    ###########
    
    observeEvent(data$rds_data(), {
      choices <- names(data$rds_data())
      
      updateSelectInput(session, "rds_select", choices = choices, selected = choices[1])
      updateSelectInput(session, "file_select", choices = choices, selected = choices[1])
      
      # Select the second item for rds2_select, if there is one
      second_choice <- if (length(choices) >= 2) choices[2] else choices[1]
      updateSelectInput(session, "rds2_select", choices = choices, selected = second_choice)
    })
    ##########
    
    # Disable second comparison input unless checkbox is checked
    observe({
      if (isTRUE(input$compare)) {
        shinyjs::enable("rds2_select")
      } else {
        shinyjs::disable("rds2_select")
      }
    })
    
    #########################################################################
    # Initially reads data files and updates acoustic events for datatable
    #########################################################################
    observeEvent(input$file_select, {
      selected_name <- input$file_select
      acou_data <- data$rds_data()[[selected_name]]
      event_titles <- sapply(acou_data@events, function(event) {
        slot(event, "id")  # Adjust slot name if necessary
      })
      updateSelectInput(session, "event_select", choices = event_titles)
    }, ignoreInit = TRUE)
    
    
    #########################################################################
    # Reactive: process species data only when file uploaded
    #########################################################################
    species_data <- reactive({
      req(input$rds_select)       # Require the user to have selected something
      req(data$rds_data())        # Require that rds_data exists
      
      selected_name <- input$rds_select
      acou_data <- data$rds_data()[[selected_name]]
      
      req(!is.null(acou_data))    # Make sure the selected data is not null
      
      process_acoustic_data(acou_data)
    })
    
    species2_data <- reactive({
      req(input$rds2_select)       # Require the user to have selected something
      req(data$rds_data())        # Require that rds_data exists
      
      selected2_name <- input$rds2_select
      acou2_data <- data$rds_data()[[selected2_name]]
      
      req(!is.null(acou2_data))    # Make sure the selected data is not null
      
      process_acoustic_data(acou2_data)
    })
    
    
    #########################################################################
    # Updates detectors based on event selected
    #########################################################################
    observeEvent(input$event_select, {
      #req(data$rds_data(), input$event_select)  # Ensure data exists

      selected_name <- input$file_select
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
    
    # Update Species Input
    observeEvent(input$file_select, {
      #req(data$rds_data(), input$file_select)
      
      selected_name <- input$file_select
      acou_data <- data$rds_data()[[selected_name]]
      
      all_species <- character(0)
      
      for (event_name in names(acou_data@events)) {
        event <- acou_data@events[[event_name]]
        
        if (!is.null(event@species)) {
          species_values <- unlist(event@species, use.names = FALSE)
          all_species <- c(all_species, species_values)
        }
      }
      unique_species <- unique(all_species)
      unique_species <- recode(unique_species, 
                             "Unid Odont" = "Unidentified Odont.",
                             "Delph spp." = "Delphinid Species")
      updateSelectInput(session, "species_select", choices = unique_species)
    }, ignoreInit = TRUE)
    
    #########################################################################
    # Dynamic Accordion UI
    #########################################################################
    output$dynamic_accordion <- renderUI({
      req(data$rds_data(), req(base_path))
      selected_name <- input$rds_select

      # Species dataframe from your process_acoustic_data
      species_df <- species_data()
      #print(species_df)

      # Load analyst comments file
      comments_path <- file.path(base_path(), "Audio", selected_name, paste0(selected_name, "_species_list.csv"))
      if (file.exists(comments_path)) {
        comments_df <- read.csv(comments_path, stringsAsFactors = FALSE)
        # Keep only relevant columns
        comments_df <- comments_df[, c("Event", "Description", "Analyst_Comments")]
      } else {
        comments_df <- data.frame(
          Event = character(0), 
          Description = character(0), 
          Analyst_Comments = character(0)
        )
      }
      
      # Merge species data with analyst comments by Event
      # merged_df <- dplyr::left_join(
      #   species_df,
      #   comments_df,
      #   by = "Event"
      # )
      merged_df <- dplyr::left_join(
        species_df,
        comments_df,
        by = "Event"
      ) 
      
      merged_df_clean <- merged_df %>%
        mutate(
          Detector = case_when(
            Detector == "Whistle_and_Moan_Detector" ~ "Whistle & Moan",
            grepl("^Click", Detector)               ~ "Click",
            TRUE                                    ~ Detector
          )
        ) %>%
        group_by(Event, Detector) %>%
        summarise(
          Start  = if (first(Detector) == "Click") min(Start) else first(Start),
          Finish = if (first(Detector) == "Click") max(Finish) else first(Finish),
          Species = first(Species),
          Description = first(Description),
          .groups = "drop"
        )
      
      # Split into species-specific tables
      species_list <- split(merged_df_clean[, c("Event", "Detector", "Start", "Finish")], # split(merged_df[, c("Event", "Start", "Finish", "Description", "Analyst_Comments")]
                            merged_df_clean$Species)
      #print(species_list)

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
    
    # Comparison Accordian Layout
    output$dynamic_accordion_cmp <- renderUI({
      req(isTRUE(input$compare))
      req(species2_data())
      selected_name <- input$rds_select
      species2_df <- species2_data()
      selected_name <- input$rds_select
      
      # Load analyst comments file
      comments_path <- file.path("Audio", selected_name, paste0(selected_name, "_species_list.csv"))
      if (file.exists(comments_path)) {
        comments_df <- read.csv(comments_path, stringsAsFactors = FALSE)
      } else {
        comments_df <- data.frame(Event = character(0), 
                                  Description = character(0), 
                                  Analyst_Comments = character(0))
      }
      
      merged_df <- dplyr::left_join(
        species2_df,
        comments_df,
        by = "Event"
      )
      
      merged_df_clean <- merged_df %>%
        mutate(
          Detector = case_when(
            Detector == "Whistle_and_Moan_Detector" ~ "Whistle & Moan",
            grepl("^Click", Detector)               ~ "Click",
            TRUE                                    ~ Detector
          )
        ) %>%
        group_by(Event, Detector) %>%
        summarise(
          Start  = if (first(Detector) == "Click") min(Start) else first(Start),
          Finish = if (first(Detector) == "Click") max(Finish) else first(Finish),
          Species = first(Species),
          Description = first(Description),
          .groups = "drop"
        )
      
      species2_list <- split(merged_df_clean[, c("Event", "Detector", "Start", "Finish")],
                             merged_df_clean$Species)
      
      purrr::imap(species2_list, function(df, species_name) {
        safe_id <- paste0("cmp_", gsub("[^A-Za-z0-9]", "_", species_name))
        output_id <- paste0("table_", safe_id)
        
        output[[output_id]] <- renderTable({ df })
        
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
      }) |> tagList()
    })
    
    #########################################################################
    # Interactive Pie Chart
    #########################################################################
    # Make sure the same species are colored the same across pie charts
    species_colors <- reactive({
      # Get all unique species across both datasets
      sp1 <- unique(species_data()$Species)
      sp2 <- if (isTRUE(input$compare)) unique(species2_data()$Species) else character(0)
      all_species <- sort(unique(c(sp1, sp2)))
      
      # Pick a palette long enough for all species
      palette <- RColorBrewer::brewer.pal(max(3, min(length(all_species), 12)), "Set3")
      
      # Recycle colors if needed
      colors <- rep(palette, length.out = length(all_species))
      
      # Create named vector mapping
      setNames(colors, all_species)
    })
    
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
      color_map <- species_colors()
      
      
      # Create a pie chart
      plot_ly(
        labels = names(species_counts),
        values = as.numeric(species_counts),
        type = "pie",
        textinfo = "label+percent",
        textposition = "inside", 
        hoverinfo = "label+value+percent",
        marker = list(colors = unname(color_map[names(species_counts)])) #marker = list(colors = RColorBrewer::brewer.pal(length(species_counts), "RdYlBu"))) %>% # Also like 'Set3', 'Blues', and 'RdYlBu'. See https://r-graph-gallery.com/38-rcolorbrewers-palettes
        ) %>% 
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
    
    output$card2_cmp <- renderPlotly({
      req(isTRUE(input$compare))
      req(species2_data())
      species2_df <- species2_data()
      
      if (nrow(species2_df) == 0) {
        return(plot_ly() %>%
                 add_trace(type = "pie", labels = c("No Species Data Found"), values = c(1), textinfo = "label"))
      }
      
      species_counts <- table(species2_df$Species)
      color_map <- species_colors()
      
      plot_ly(
        labels = names(species_counts),
        values = as.numeric(species_counts),
        type = "pie",
        textinfo = "label+percent",
        textposition = "inside",
        hoverinfo = "label+value+percent",
        marker = list(colors = unname(color_map[names(species_counts)])) #marker = list(colors = RColorBrewer::brewer.pal(length(species_counts), "Set3"))
      ) %>%
        layout(
          title = NULL,
          legend = list(orientation = "h", x = 0.1, y = -0.1, font = list(size = 12)),
          width = 490,
          height = 520,
          margin = list(l = 20, r = 20, t = 20, b = 20),
          autosize = TRUE
        )
    })
    
    #########################################################################
    # Render data table based on event/detector selected
    #########################################################################
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
      selected_name <- input$file_select
      acou_data <- data$rds_data()[[selected_name]]
      
      # Show data for all species-matching events
      if (isTRUE(input$filter_species)) {
        req(input$species_select)
        selected_species <- input$species_select
        
        all_data <- list()
        
        for (event_name in names(acou_data@events)) {
          event <- acou_data@events[[event_name]]
          
          # Check if selected species is present in this event
          if (!is.null(event@species)) {
            species_values <- unlist(event@species, use.names = FALSE)
            if (selected_species %in% species_values) {
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
          }
        }
        
        if (length(all_data) > 0) {
          final_df <- dplyr::bind_rows(all_data)
          return(DT::datatable(final_df, filter = 'top', class = 'cell-border stripe', options = list(pageLength = 15)))
        } else {
          return(DT::datatable(data.frame(Message = "No data available for selected species"), options = list(dom = 't')))
        }
      }
      
      # Show data for all events (regardless of species)
      else if (isTRUE(input$all_events)) {
        showNotification("Loading data for all events. This may take some time.", type = "message", duration = 6)
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
      }
      
      # Show data only for the selected event and detector
      else {
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
    
    
    #########################################################################
    # Read data table and export to csv 
    #########################################################################
    output$export_csv <- downloadHandler(
      filename = function() {
        selected_name <- input$file_select
        
        if (isTRUE(input$filter_species)) {
          paste0(selected_name, "_", input$species_select, "_Filtered.csv")
        } else if (isTRUE(input$all_events)) {
          paste0(selected_name, "_All_Events.csv")
        } else {
          paste0(input$event_select, "_", input$detector_select, ".csv")
        }
      },
      content = function(file) {
        showNotification(
          "Preparing CSV download. This can take some time depending on the amount of data being exported.",
          type = "message", duration = 8
        )
        
        req(data$rds_data())
        selected_name <- input$file_select
        acou_data <- data$rds_data()[[selected_name]]
        
        final_df <- NULL
        
        if (isTRUE(input$filter_species)) {
          # Combine all detector data from events with matching species
          all_data <- list()
          for (event_name in names(acou_data@events)) {
            event <- acou_data@events[[event_name]]
            
            if (!is.null(event@species)) {
              species_values <- unlist(event@species, use.names = FALSE)
              if (!(input$species_select %in% species_values)) next
            } else {
              next
            }
            
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
          }
          
        } else if (isTRUE(input$all_events)) {
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
          }
          
        } else {
          # Export only the selected event and detector
          req(input$event_select, input$detector_select)
          selected_event <- acou_data@events[[input$event_select]]
          detector_data <- selected_event@detectors[[input$detector_select]]
          
          if (is.data.frame(detector_data)) {
            final_df <- detector_data
          }
        }
        
        # Handle empty case
        if (is.null(final_df) || !is.data.frame(final_df)) {
          showNotification("No valid data available to download", type = "warning", duration = 5)
          write.csv(data.frame(Message = "No valid data available"), file, row.names = FALSE)
          return(NULL)
        }
        
        # Apply filtering from the DataTable
        filtered_idx <- input$data_table_rows_all
        if (!is.null(filtered_idx)) {
          final_df <- final_df[filtered_idx, , drop = FALSE]
        }
        
        # Write out
        write.csv(final_df, file, row.names = FALSE)
      }
    )

    #########################################################################
    # Ensure only one of the datatable checkboxes is checked at a time
    #########################################################################
    observeEvent(input$all_events, {
      if (isTRUE(input$all_events)) {
        updatePrettyCheckbox(session, "filter_events", value = FALSE)
        updatePrettyCheckbox(session, "filter_species", value = FALSE)
      }
    })
    
    observeEvent(input$filter_events, {
      if (isTRUE(input$filter_events)) {
        updatePrettyCheckbox(session, "all_events", value = FALSE)
        updatePrettyCheckbox(session, "filter_species", value = FALSE)
      }
    })
    
    observeEvent(input$filter_species, {
      if (isTRUE(input$filter_species)) {
        updatePrettyCheckbox(session, "all_events", value = FALSE)
        updatePrettyCheckbox(session, "filter_events", value = FALSE)
      }
    })
    
  })
}
    
## To be copied in the UI
# mod_overview_ui("overview_1")
    
## To be copied in the server
# mod_overview_server("overview_1")
