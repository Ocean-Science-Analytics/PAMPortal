#' analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import ggplot2
#' @import suncalc
#' @import lutz
#' @import glue

var_names <- c(
  "Beginning Frequency (Hz)" = "freqBeg",
  "Ending Frequency (Hz)" = "freqEnd",
  "Mean Frequency (Hz)" = "freqMean",
  "Frequency Standard Deviation (Hz)" = "freqStdDev",
  "Duration (s)" = "duration",
  "Frequency Slope Mean (Hz)" = "freqSlopeMean",
  "Frequency Slope Ratio (Hz)" = "freqSlopeRatio",
  "Frequency Spread (Hz)" = "freqSpread",
  "Minimum Frequency (Hz)" = "freqMin",
  "Maximum Frequency (Hz)" = "freqMax",
  "Frequency Range (Hz)" = "freqRange",
  "Frequency Median (Hz)" = "freqMedian",
  "Frequency Maximum:Minimum Ratio (Hz)" = "freqMaxMinRatio",
  "Frequency Beginning:End Ratio (Hz)" = "freqBegEndRatio",
  "Step Duration (s)" = "stepDur"
)

env_var_choices <- c(
  "None" = "None",
  "Sea Level Anomaly"  = "sla",
  "Sea Surface Temperature" = "analysed_sst",
  "Chlorophyll A"  = "chlor_a",
  "KD490"  = "kd_490",
  "Lunar Cycles" = "moon_illum"
)

mod_analysis_ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    tags$head(
      tags$style(HTML("
        .custom-btn {
          background-color: #00688B !important;
          color: white !important;
          border-color: black !important;
        }
        .custom-btn:hover {
          background-color: #8DB6CD !important;
          transform: scale(1.05);
          cursor: pointer;
        }
        .custom-btn-success:hover {
          background-color: darkolivegreen43!important;
          transform: scale(1.05);
          cursor: pointer;
        }
        .load-container {
          align-items: flex-start !important; /* move spinner to the top */
          padding-top: 20px;                  /* add some spacing from top */
        }
        "))
    ),
    
    tabsetPanel(
      tabPanel(
        "Occurrence",
        br(),
        div(style = "border: 2px solid black; border-radius: 8px; padding: 15px; margin-bottom: 10px; box-shadow: 0 8px 10px rgba(0,0,0.08,0.4);",
            fluidRow(
              column(
                width = 3,
                selectInput(ns("location_occr"), "Select Location", choices = NULL),
                selectInput(
                  inputId = ns("env_var_occr"),
                  label   = "Environmental Variable",
                  choices = env_var_choices,
                  multiple = FALSE,
                  selected = ""
                )
                #numericInput(ns("duty"), "Duty Cycle (min)", value = 60, min = 1, step = 1),
              ),
              column(
                width = 3,
                selectInput(ns("species_filter_occr"), "Select Species", choices = NULL, multiple = TRUE),
                div(
                  style = "padding-top: 28px;",
                  checkboxInput(
                    inputId = ns("show_effort"),
                    label = "Show Effort?",
                    value = FALSE
                  )
                )
              ),
              column(
                width = 3,
                selectInput(ns("month_filter_occr"), "Select Month", choices = c("All", month.name), selected = "All", multiple = TRUE)
              ),
              column(
                width = 1, 
                align = "middle",
                div(style = "height: 100%; border-right: 2px solid black;")
              ),
              column(
                width = 2,
                tags$div(
                  style = "margin-bottom: 10px;",
                  actionButton(ns("render_occr"), "Render Plot", icon = shiny::icon("file-lines"), class = "custom-btn")
                ),
                downloadButton(ns("download_occr_plot"), "Download Plot", class = "btn-success custom-btn-success")
              )
            ),
        ),
        div(
          style = "display: flex; justify-content: flex-end; width: 100%;",
          actionButton(ns("occr_description"), "", icon = shiny::icon("question"), class = "custom-btn")
        ),
        br(),
        withSpinner(plotOutput(ns("occr_plot"), height = "750px"), type = 4, color = "#001f3f",caption="Loading Occurrence Plot...")
      ),
      tabPanel(
        "Call Count",
        br(),
        div(style = "border: 2px solid black; border-radius: 8px; padding: 15px; margin-bottom: 10px; box-shadow: 0 8px 10px rgba(0,0,0.08,0.4);",
            fluidRow(
              column(
                width = 3,
                selectInput(ns("location_call_count"), "Select Location", choices = NULL),
                selectInput(
                  inputId = ns("env_var_call_count"),
                  label   = "Environmental Variable",
                  choices = env_var_choices,
                  multiple = FALSE,
                  selected = ""
                )
              ),
              column(
                width = 3,
                selectInput(ns("species_filter_call_count"), "Select Species", choices = NULL, multiple = TRUE),
                div(
                  style = "padding-top: 28px;",
                  checkboxInput(
                    inputId = ns("log_scale"),
                    label = "Log Scale",
                    value = FALSE
                  )
                )
              ),
              column(
                width = 3,
                selectInput(ns("month_filter_call_count"), "Select Month", choices = c("All", month.name), selected = "All", multiple = TRUE)
              ),
              column(
                width = 1, 
                align = "middle",
                div(style = "height: 100%; border-right: 2px solid black;")
              ),
              column(
                width = 2,
                tags$div(
                  style = "margin-bottom: 10px;",
                  actionButton(ns("render_call_count"), "Render Plot", icon = shiny::icon("file-lines"), class = "custom-btn")
                ),
                downloadButton(ns("download_call_count_plot"), "Download Plot", class = "btn-success custom-btn-success")
              )
            )
        ),
        div(
          style = "display: flex; justify-content: flex-end; width: 100%;",
          actionButton(ns("call_count_description"), "", icon = shiny::icon("question"), class = "custom-btn")
        ),
        br(),
        withSpinner(plotOutput(ns("call_count_plot"), height = "750px"), type = 4, color = "#001f3f", caption="Loading Call Count Plot...")
      ),
      tabPanel(
        "Call Density",
        br(),
        div(style = "border: 2px solid black; border-radius: 8px; padding: 15px; margin-bottom: 10px; box-shadow: 0 8px 10px rgba(0,0,0.08,0.4);",
            fluidRow(
              column(
                width = 3,
                selectInput(ns("location_call_den"), "Select Location", choices = NULL),
                selectInput(
                  inputId = ns("env_var_call_den"),
                  label   = "Environmental Variable",
                  choices = env_var_choices,
                  multiple = FALSE,
                  selected = ""
                )
                #numericInput(ns("duty"), "Duty Cycle (min)", value = 60, min = 1, step = 1),
              ),
              column(
                width = 3,
                selectInput(ns("species_filter_call_den"), "Select Species", choices = NULL, multiple = TRUE),
                # div(
                #   style = "padding-top: 28px;",
                #   checkboxInput(
                #     inputId = ns("log_scale"),
                #     label = "Log Scale",
                #     value = FALSE
                #   )
                # )
              ),
              column(
                width = 3,
                selectInput(ns("month_filter_call_den"), "Select Month", choices = c("All", month.name), selected = "All", multiple = TRUE)
              ),
              column(
                width = 1, 
                align = "middle",
                div(style = "height: 100%; border-right: 2px solid black;")
              ),
              column(
                width = 2,
                tags$div(
                  style = "margin-bottom: 10px;",
                  actionButton(ns("render_call_den"), "Render Plot", icon = shiny::icon("file-lines"), class = "custom-btn")
                ),
                downloadButton(ns("download_call_den_plot"), "Download Plot", class = "btn-success custom-btn-success")
              )
            ),
        ),
        div(
          style = "display: flex; justify-content: flex-end; width: 100%;",
          actionButton(ns("call_den_description"), "", icon = shiny::icon("question"), class = "custom-btn")
        ),
        br(),
        withSpinner(plotOutput(ns("call_den_plot"), height = "750px"), type = 4, color = "#001f3f", caption="Loading Call Density Plot...")
        #plotOutput(ns("effort_plot"),height = "600px")
      ),
      tabPanel(
        "Call Measurments",
        br(),
        div(style = "border: 2px solid black; border-radius: 8px; padding: 15px; margin-bottom: 10px; box-shadow: 0 8px 10px rgba(0,0,0.08,0.4);",
            fluidRow(
              column(
                width = 4,
                selectInput(ns("location_dis"), "Select Location", choices = NULL, multiple = TRUE),
                selectInput(ns("event_filter"), "Select Events", choices = c("All"), selected = "ALL", multiple = TRUE)
              ),
              column(
                width = 3,
                selectInput(ns("distribution_variable"), "Select Variable", selected = "freqBeg", choices = var_names, multiple = TRUE),
                selectInput(ns("detector_filter"), "Select Detector", choices = c("Whistle & Moan", "Click"), selected = "Whistle & Moan", multiple = FALSE)
              ),
              column(
                width = 2,
                selectInput(ns("species_filter"), "Select Species", choices = c("All"), selected = "ALL", multiple = TRUE)
              ),
              column(
                width = 1, 
                align = "middle",
                div(style = "height: 100%; border-right: 2px solid black;")
              ),
              column(
                width = 2,
                tags$div(
                  style = "margin-bottom: 10px;",
                  actionButton(ns("render_distribution"), "Render Plot", icon = shiny::icon("file-lines"), class = "custom-btn")
                ),
                downloadButton(ns("download_distribution_plot"), "Download Plot", class = "btn-success custom-btn-success")
              )
            ),
        ),
        div(
          style = "display: flex; justify-content: flex-end; width: 100%;",
          actionButton(ns("dstrb_description"), "", icon = shiny::icon("question"), class = "custom-btn")
        ),
        br(),
        withSpinner(plotOutput(ns("distribution_plot"), height = "750px"), type = 4, color = "#001f3f",caption="Loading Call Measurment Plot...")
        #plotOutput(ns("distribution_plot"), height = "600px")
      ),
      tabPanel(
        "Presence",
        br(),
        div(style = "border: 2px solid black; border-radius: 8px; padding: 15px; margin-bottom: 10px; box-shadow: 0 8px 10px rgba(0,0,0.08,0.4);",
            fluidRow(
              column(
                width = 3,
                selectInput(ns("location_presence"), "Select Location", choices = NULL),
                selectInput(ns("metric_presence"), "Select Metric", choices = c("Count", "Duration"), selected = "Count"),
                #numericInput(ns("duty"), "Duty Cycle (min)", value = 60, min = 1, step = 1),
              ),
              column(
                width = 3,
                selectInput(ns("species_filter_presence"), "Select Species", choices = NULL, multiple = TRUE),
                div(
                  style = "padding-top: 28px;",
                  checkboxInput(
                    inputId = ns("log_scale_presence"),
                    label = "Log Scale",
                    value = FALSE
                  )
                )
              ),
              column(
                width = 3,
                selectInput(ns("month_filter_presence"), "Select Month", choices = c("All", month.name), selected = "All", multiple = TRUE)
              ),
              column(
                width = 1, 
                align = "middle",
                div(style = "height: 100%; border-right: 2px solid black;")
              ),
              column(
                width = 2,
                tags$div(
                  style = "margin-bottom: 10px;",
                  actionButton(ns("render_presence"), "Render Plot", icon = shiny::icon("file-lines"), class = "custom-btn")
                ),
                downloadButton(ns("download_presence_plot"), "Download Plot", class = "btn-success custom-btn-success")
              )
            ),
        ),
        div(
          style = "display: flex; justify-content: flex-end; width: 100%;",
          actionButton(ns("presence_description"), "", icon = shiny::icon("question"), class = "custom-btn")
        ),
        br(),
        withSpinner(plotOutput(ns("presence_plot"), height = "750px"), type = 4, color = "#001f3f", caption="Loading Presence Plot...")
      ),
      tabPanel(
        "Detection",
        br(),
        div(style = "border: 2px solid black; border-radius: 8px; padding: 15px; margin-bottom: 10px; box-shadow: 0 8px 10px rgba(0,0,0.08,0.4);",
            fluidRow(
              column(
                width = 3,
                selectInput(ns("location_detection"), "Select Location", choices = NULL),
                checkboxInput(ns("see_duty_detection"), "Show Full Duty Cycle", value = FALSE),
                uiOutput(ns("duty_text"))
              ),
              column(
                width = 3,
                selectInput(ns("species_filter_detection"), "Select Species", choices = NULL, multiple = TRUE),
              ),
              column(
                width = 3,
                selectInput(ns("month_filter_detection"), "Select Month", choices = c("All", month.name), selected = "All", multiple = TRUE)
              ),
              column(
                width = 1,
                align = "middle",
                div(style = "height: 100%; border-right: 2px solid black;")
              ),
              column(
                width = 2,
                tags$div(
                  style = "margin-bottom: 10px;",
                  actionButton(ns("render_plot_detection"), "Render Plot", icon = shiny::icon("file-lines"), class = "custom-btn")
                ),
                downloadButton(ns("download_detection_plot"), "Download Plot", class = "btn-success custom-btn-success")
              )
            ),
        ),
        div(
          style = "display: flex; justify-content: flex-end; width: 100%;",
          actionButton(ns("detection_description"), "", icon = shiny::icon("question"), class = "custom-btn")
        ),
        br(),
        withSpinner(plotOutput(ns("detection_plot"), height = "700px"), type = 4, color = "#001f3f", caption="Loading Detection Plot...")
      )
    )
  )
}
    
#' analysis Server Functions
#'
#' @noRd 
mod_analysis_server <- function(id, data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    library(lubridate)
    call_count_plot_obj <- reactiveVal(NULL)
    call_den_plot_obj <- reactiveVal(NULL)
    presence_plot_obj <- reactiveVal(NULL)
    plot_measurements_obj <- reactiveVal(NULL)
    occurrence_plot_obj <- reactiveVal(NULL)
    detection_plot_obj <- reactiveVal(NULL)
    
    base_path <- reactive({
      req(data$selected_dir())
    })

    observeEvent(data$rds_data(), {
      req(data$rds_data())
      locations <- names(data$rds_data())
      updateSelectInput(session, "location_call_count", choices = locations)
      updateSelectInput(session, "location_call_den", choices = locations)
      updateSelectInput(session, "location_presence", choices = locations)
      updateSelectInput(session, "location_dis", choices = locations)
      updateSelectInput(session, "location_occr", choices = locations)
      updateSelectInput(session, "location_detection", choices = locations)
    })
    
    observeEvent(input$location_dis, {
      req(data$rds_data(), input$location_dis)
      
      selected_data <- data$rds_data()[input$location_dis]
      
      # Collect all events and species from the selected locations
      event_choices <- unique(unlist(lapply(selected_data, function(x) names(x@events))))
      species_choices <- unique(unlist(lapply(selected_data, function(x) {
        sapply(x@events, function(ev) ev@species$id)
      })))
      
      updateSelectInput(session, "event_filter", choices = c("All", event_choices), selected = "All")
      updateSelectInput(session, "species_filter", choices = c("All", species_choices), selected = "All")
    })
    
    ###################################################################
    # Occurance Plot
    ###################################################################
    observeEvent(input$location_occr, {
      req(input$location_occr)
      
      # Load RDS data and extract unique species
      rds <- readRDS(file.path(base_path(), "RDS", paste0(input$location_occr, ".rds")))
      species <- unique(unlist(lapply(rds@events, function(ev) ev@species$id)))
      
      updateSelectInput(session, "species_filter_occr",
                        choices = c("All", species),
                        selected = "All")
    })
    
    occurrence_plot_obj <- eventReactive(input$render_occr, {
      req(base_path(), input$location_occr, input$species_filter_occr)
      showNotification("Rendering Occurrence Plot...", type = "message")
      
      plot_occurrence(
        location = input$location_occr,
        base_path = base_path(),
        species_of_interest = input$species_filter_occr,
        months_of_interest = input$month_filter_occr,
        environmental_variable = input$env_var_occr,
        show_effort = input$show_effort
      )
    }, ignoreNULL = TRUE)
    
    output$occr_plot <- renderPlot({
      req(occurrence_plot_obj())
      occurrence_plot_obj()
    })
    
    ###################################################################
    # Call Count Plot
    ###################################################################
    observeEvent(input$location_call_count, {
      req(input$location_call_count)
      
      # Load RDS data and extract unique species
      rds <- readRDS(file.path(base_path(), "RDS", paste0(input$location_call_count, ".rds")))
      species <- unique(unlist(lapply(rds@events, function(ev) ev@species$id)))
      
      updateSelectInput(session, "species_filter_call_count",
                        choices = c("All", species),
                        selected = "All")
    })
    
    call_count_plot_obj <- eventReactive(input$render_call_count, {
      req(base_path(), input$location_call_count)
      showNotification("Loading Call Count Plot...", type = "message")
      plot_call_count(
        location = input$location_call_count,
        base_path = base_path(),
        species_of_interest = input$species_filter_call_count,
        months_of_interest = input$month_filter_call_count,
        environmental_variable = input$env_var_call_count,
        log_scale = input$log_scale
      )
    }, ignoreNULL = TRUE)
    
    output$call_count_plot <- renderPlot({
      req(call_count_plot_obj())
      call_count_plot_obj()
    })
    
    ###################################################################
    # Call Density Plot
    ###################################################################
    observeEvent(input$location_call_den, {
      req(input$location_call_den)
      
      # Load RDS data and extract unique species
      rds <- readRDS(file.path(base_path(), "RDS", paste0(input$location_call_den, ".rds")))
      species <- unique(unlist(lapply(rds@events, function(ev) ev@species$id)))
      
      updateSelectInput(session, "species_filter_call_den",
                        choices = c("All", species),
                        selected = "All")
    })
    
    call_den_plot_obj <- eventReactive(input$render_call_den, {
      req(base_path(), input$location_call_den)
      showNotification("Loading Call Density Plot...", type = "message")
      plot_call_density(
        location = input$location_call_den,
        base_path = base_path(),
        species_of_interest = input$species_filter_call_den,
        months_of_interest = input$month_filter_call_den,
        environmental_variable = input$env_var_call_den
        #log_scale = input$log_scale
      )
    }, ignoreNULL = TRUE)
    
    output$call_den_plot <- renderPlot({
      req(call_den_plot_obj())
      call_den_plot_obj()
    })
    
    ###################################################################
    # Call Measurement Plot
    ###################################################################
    observeEvent(input$species_filter, {
      req(data$rds_data(), input$location_dis, input$species_filter)
      
      selected_data <- data$rds_data()[input$location_dis]
      
      # Filter events based on selected species
      filtered_events <- lapply(selected_data, function(loc_data) {
        Filter(function(ev) {
          species_id <- ev@species$id
          "All" %in% input$species_filter || species_id %in% input$species_filter
        }, loc_data@events)
      })
      
      # Extract event names that match the species filter
      event_choices <- unique(unlist(lapply(filtered_events, names)))
      
      updateSelectInput(session, "event_filter", choices = c("All", event_choices), selected = "All")
    })
    
    plot_measurements_obj <- eventReactive(input$render_distribution, {
      req(base_path(), input$location_dis, input$distribution_variable)
      showNotification("Loading Call Measurement Plot...", type = "message")
      
      plot_measurements(
        location_list = input$location_dis,
        base_path = base_path(),
        events_of_interest = input$event_filter,
        variables_of_interest = input$distribution_variable,
        species = input$species_filter,
        detector_type = input$detector_filter
      )
    }, ignoreNULL = TRUE)
    
    output$distribution_plot <- renderPlot({
      req(plot_measurements_obj())
      plot_measurements_obj()
    })
    
    ###################################################################
    # Hourly Presence Plot
    ###################################################################
    observeEvent(input$location_presence, {
      req(input$location_presence)
      
      # Load RDS data and extract unique species
      rds <- readRDS(file.path(base_path(), "RDS", paste0(input$location_presence, ".rds")))
      species <- unique(unlist(lapply(rds@events, function(ev) ev@species$id)))
      
      updateSelectInput(session, "species_filter_presence",
                        choices = c("All", species),
                        selected = "All")
    })
    
    presence_plot_obj <- eventReactive(input$render_presence, {
      req(base_path(), input$location_presence)
      showNotification("Loading Hourly Presence Plot...", type = "message")
      plot_hourly_presence(
        location = input$location_presence,
        base_path = base_path(),
        species_of_interest = input$species_filter_presence,
        months_of_interest = input$month_filter_presence,
        metric = input$metric_presence,
        log_scale = input$log_scale
      )
    }, ignoreNULL = TRUE)
    
    output$presence_plot <- renderPlot({
      req(presence_plot_obj())
      presence_plot_obj()
    })
    
    ###################################################################
    # Detection Plot
    ###################################################################
    observeEvent(input$location_detection, {
      req(input$location_detection)
      
      # Load RDS data and extract unique species
      rds <- readRDS(file.path(base_path(), "RDS", paste0(input$location_detection, ".rds")))
      species <- unique(unlist(lapply(rds@events, function(ev) ev@species$id)))
      
      updateSelectInput(session, "species_filter_detection",
                        choices = c("All", species),
                        selected = "All")
    })
    
    detection_plot_obj <- eventReactive(input$render_plot_detection, {
      req(base_path(), input$location_detection)
      showNotification("Loading Detection Plot...", type = "message")

      dc_file <- file.path(base_path(), "Duty_Cycles.csv")
      duty_lookup <- if (file.exists(dc_file)) {
        dc_df <- read.csv(dc_file, stringsAsFactors = FALSE)
        setNames(dc_df$dc, dc_df$location)
      } else {
        list()
      }

      duty_min <- if (isTRUE(input$see_duty_detection)) duty_lookup[[input$location_detection]] else 60

      plot_detections_by_minute(
        location = input$location_detection,
        base_path = base_path(),
        species_of_interest = input$species_filter_detection,
        months_of_interest = input$month_filter_detection,
        see_duty_cycle = input$see_duty_detection
        #duty_cycle_min = if (is.null(duty_min)) 60 else duty_min
      )
    }, ignoreNULL = TRUE)

    output$detection_plot <- renderPlot({
      req(detection_plot_obj())
      detection_plot_obj()
    })

    # still provide the duty text UI
    output$duty_text <- renderUI({
      req(input$see_duty, input$location_detection)
      dc_file <- file.path(base_path(), "Duty_Cycles.csv")
      if (!file.exists(dc_file)) return(NULL)
      duty_cycles_df <- read.csv(dc_file, stringsAsFactors = FALSE)
      duty_lookup <- setNames(duty_cycles_df$dc, duty_cycles_df$location)
      #print(duty_lookup)
      duty_val <- duty_lookup[[input$location_detection]]
      if (is.null(duty_val)) return(NULL)
      HTML(glue::glue("<div style='margin-top: 5px; color: #444;'>Duty cycle at <b>{input$location}</b> was <b>{duty_val} minutes</b> every hour.</div>"))
    })
    
    ###################################################################
    # Download Plot Logic
    ###################################################################
    output$download_call_count_plot <- downloadHandler(
      filename = function() {
        paste0("call_count_plot_", Sys.Date(), ".png")
      },
      content = function(file) {
        req(call_count_plot_obj())
        ggsave(file, plot = call_count_plot_obj(), width = 10, height = 6, dpi = 300)
      }
    )
    ####
    output$download_call_den_plot <- downloadHandler(
      filename = function() {
        paste0("call_den_plot_", Sys.Date(), ".png")
      },
      content = function(file) {
        req(call_den_plot_obj())
        ggsave(file, plot = call_den_plot_obj(), width = 10, height = 6, dpi = 300)
      }
    )
    ####
    output$download_distribution_plot <- downloadHandler(
      filename = function() {
        paste0("distribution_plot_", Sys.Date(), ".png")
      },
      content = function(file) {
        req(distribution_plot_obj())
        ggsave(file, plot = distribution_plot_obj(), width = 10, height = 6, dpi = 300)
      }
    )
    ####
    output$download_occr_plot <- downloadHandler(
      filename = function() {
        paste0("occurrence_plot_", Sys.Date(), ".png")
      },
      content = function(file) {
        req(occurrence_plot_obj())
        ggsave(file, plot = occurrence_plot_obj(), width = 10, height = 6, dpi = 300)
      }
    )
    ####
    output$download_presence_plot <- downloadHandler(
      filename = function() {
        paste0("presence_plot_", Sys.Date(), ".png")
      },
      content = function(file) {
        req(presence_plot_obj())
        ggsave(file, plot = presence_plot_obj(), width = 10, height = 6, dpi = 300)
      }
    )
    ####
    output$download_detection_plot <- downloadHandler(
      filename = function() {
        paste0("detection_plot_", Sys.Date(), ".png")
      },
      content = function(file) {
        req(detection_plot_obj())
        ggsave(file, plot = detection_plot_obj(), width = 10, height = 6, dpi = 300)
      }
    )
    
    ###################################################################
    # Description Pages
    ###################################################################
    observeEvent(input$call_count_description, {
      showModal(modalDialog(
        title = "Call Count Plot Description",
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Close"),
        tagList(
          p("This plot visualizes the daily acoustic effort and species detections."),
          p("Each row represents one day, and time progresses from left to right across each row."),
          p("Time is shown on the x-axis (hours and minutes). Colors represent different detection categories:"),
          tags$ul(
            tags$li(strong("Species names:"), " Indicate detections for that species during a duty cycle window."),
            tags$li(strong("No Events:"), " The recorder was active (within duty cycle), but no species were detected."),
            tags$li(strong("Not Sampled:"), " The recorder was off (outside the duty cycle) or no data was available.")
          ),
          p("If 'Show Full Duty Cycle' is checked, the actual duty cycle duration per site is applied when determining sampled periods."),
          p("Sunrise and sunset data are used to infer day/night cycles when available.")
        )
      ))
    })
    
    observeEvent(input$dstrb_description, {
      showModal(modalDialog(
        title = "Call Measurement Plot Description",
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Close"),
        tagList(
          p("This figure displays the distribution of selected call characteristics (e.g., duration, frequency max, frequency median, etc.)."),
          p("Each distribution corresponds to selected species and events, allowing comparison of call features across categories."),
          tags$ul(
            tags$li("Violin plots show the full distribution of values for the selected variables."),
            tags$li("The user can filter by species, the detector used, and the acoustic event using the dropdowns above.")
          ),
          p("This tool is helpful for comparing how different species or events vary in acoustic properties.")
        )
      ))
    })
    
    observeEvent(input$occr_description, {
      showModal(modalDialog(
        title = "Occurrence Plot Description",
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Close"),
        tagList(
          p("This figure displays the occurrence of different species based on recorded acoustic vocalizations."),
          p("Each point on the figure corresponds to an event in which that species was identified, and the color corresponds to the duration of time in which they were present in the recording."),
          p("This tool is helpful for visualizing the vocal occurrence of different species both short term (time of day) and long-term (date).")
        )
      ))
    })
    
  })
}
    
## To be copied in the UI
# mod_analysis_ui("analysis_1")
    
## To be copied in the server
# mod_analysis_server("analysis_1")
