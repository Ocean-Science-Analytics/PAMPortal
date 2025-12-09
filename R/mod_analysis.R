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
var_names2 <- c(
  "Noise Level (dB)" = "noiseLevel",
  "Duration (s)" = "duration",
  "Peak Time (s)" = "peakTime",
  "Peak Amplitude (dB)" = "peak",
  "Second Peak Amplitude (dB)" = "peak2",
  "Third Peak Amplitude (dB)" = "peak3",
  "Trough Amplitude (dB)" = "trough",
  "Second Trough Amplitude (dB)" = "trough2",
  "Peak-to-Peak Amplitude (1–2) (dB)" = "peakToPeak2",
  "Peak-to-Peak Amplitude (1–3) (dB)" = "peakToPeak3",
  "Peak Amplitude Ratio (Peak2:Peak3)" = "peak2ToPeak3",
  "Peak-to-Peak Level (dB)" = "dBPP",
  "Q10 (Quality Factor at -10 dB)" = "Q_10dB",
  "Minimum Frequency (-10 dB) (Hz)" = "fmin_10dB",
  "Maximum Frequency (-10 dB) (Hz)" = "fmax_10dB",
  "Bandwidth (-10 dB) (Hz)" = "BW_10dB",
  "Center Frequency (-10 dB) (kHz)" = "centerkHz_10dB",
  "Q3 (Quality Factor at -3 dB)" = "Q_3dB",
  "Minimum Frequency (-3 dB) (Hz)" = "fmin_3dB",
  "Maximum Frequency (-3 dB) (Hz)" = "fmax_3dB",
  "Bandwidth (-3 dB) (Hz)" = "BW_3dB",
  "Center Frequency (-3 dB) (kHz)" = "centerkHz_3dB"
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
        "Daily Occurrence",
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
                    label = "Show Effort",
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
          style = "display: flex; justify-content: flex-end; width: 100%; padding-right: 20px;",
          actionButton(ns("occr_description"), "", icon = shiny::icon("question"), class = "custom-btn")
        ),
        br(),
        withSpinner(plotOutput(ns("occr_plot"), height = "750px"), type = 4, color = "#001f3f",caption="Loading Occurrence Plot...")
      ),
      tabPanel(
        "Diel Density",
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
          style = "display: flex; justify-content: flex-end; width: 100%; padding-right: 20px;",
          actionButton(ns("presence_description"), "", icon = shiny::icon("question"), class = "custom-btn")
        ),
        br(),
        withSpinner(plotOutput(ns("presence_plot"), height = "750px"), type = 4, color = "#001f3f", caption="Loading Presence Plot...")
      ),
      tabPanel(
        "Diel Detection",
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
          style = "display: flex; justify-content: flex-end; width: 100%; padding-right: 20px;",
          actionButton(ns("detection_description"), "", icon = shiny::icon("question"), class = "custom-btn")
        ),
        br(),
        withSpinner(plotOutput(ns("detection_plot"), height = "700px"), type = 4, color = "#001f3f", caption="Loading Detection Plot...")
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
          style = "display: flex; justify-content: flex-end; width: 100%; padding-right: 20px;",
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
          style = "display: flex; justify-content: flex-end; width: 100%; padding-right: 20px;",
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
          style = "display: flex; justify-content: flex-end; width: 100%; padding-right: 20px;",
          actionButton(ns("call_measurment_description"), "", icon = shiny::icon("question"), class = "custom-btn")
        ),
        br(),
        withSpinner(plotOutput(ns("distribution_plot"), height = "750px"), type = 4, color = "#001f3f",caption="Loading Call Measurment Plot...")
        #plotOutput(ns("distribution_plot"), height = "600px")
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
    
    observeEvent(input$detector_filter, {
      req(input$detector_filter)
      
      # Choose variable set based on detector
      if (input$detector_filter %in% c("Whistle & Moan")) {
        choices <- var_names
      } else if (input$detector_filter == "Click") {
        choices <- var_names2
      } else {
        choices <- character(0)  # fallback if needed
      }
      
      # Update the selectInput
      updateSelectInput(session, "distribution_variable", choices = choices, selected = choices[1])
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
      showNotification("Loading Diel Presence Plot...", type = "message")
      plot_hourly_presence(
        location = input$location_presence,
        base_path = base_path(),
        species_of_interest = input$species_filter_presence,
        months_of_interest = input$month_filter_presence,
        metric = input$metric_presence,
        log_scale = input$log_scale_presence
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
      showNotification("Loading Diel Detection Plot...", type = "message")

      dc_file <- file.path(base_path(), "Metadata.csv")
      
      duty_lookup <- if (file.exists(dc_file)) {
        dc_df <- read.csv(dc_file, stringsAsFactors = FALSE)
        setNames(dc_df$dc, dc_df$Site)
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
      req(input$see_duty_detection, input$location_detection)
      dc_file <- file.path(base_path(), "Metadata.csv")
      if (!file.exists(dc_file)) return(NULL)
      duty_cycles_df <- read.csv(dc_file, stringsAsFactors = FALSE)
      duty_print <- setNames(duty_cycles_df$dc, duty_cycles_df$Site)
      #print(duty_lookup)
      duty_val <- duty_print[[input$location_detection]]
      if (is.null(duty_val)) return(NULL)
      duty_message <- if (duty_val == 60) {
        glue::glue("Duty cycle at <b>{input$location_detection}</b> was a full 60 minutes every hour.")
      } else {
        glue::glue("Duty cycle at <b>{input$location_detection}</b> was <b>{duty_val} minutes</b> every hour.")
      }
      
      HTML(glue::glue("<div style='margin-top: 5px; color: #444;'>{duty_message}</div>"))
      #HTML(glue::glue("<div style='margin-top: 5px; color: #444;'>Duty cycle at <b>{input$location_detection}</b> was <b>{duty_val} minutes</b> every hour.</div>"))
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
        ggsave(file, plot = call_count_plot_obj(), bg = "white", width = 10, height = 6, dpi = 300)
      }
    )
    ####
    output$download_call_den_plot <- downloadHandler(
      filename = function() {
        paste0("call_den_plot_", Sys.Date(), ".png")
      },
      content = function(file) {
        req(call_den_plot_obj())
        ggsave(file, plot = call_den_plot_obj(), bg = "white", width = 10, height = 6, dpi = 300)
      }
    )
    ####
    output$download_distribution_plot <- downloadHandler(
      filename = function() {
        paste0("call_measurement_plot_", Sys.Date(), ".png")
      },
      content = function(file) {
        req(plot_measurements_obj())
        ggsave(file, plot = plot_measurements_obj(), bg = "white", width = 10, height = 6, dpi = 300)
      }
    )
    ####
    output$download_occr_plot <- downloadHandler(
      filename = function() {
        paste0("occurrence_plot_", Sys.Date(), ".png")
      },
      content = function(file) {
        req(occurrence_plot_obj())
        ggsave(file, plot = occurrence_plot_obj(), bg = "white", width = 10, height = 6, dpi = 300)
      }
    )
    ####
    output$download_presence_plot <- downloadHandler(
      filename = function() {
        paste0("presence_plot_", Sys.Date(), ".png")
      },
      content = function(file) {
        req(presence_plot_obj())
        ggsave(file, plot = presence_plot_obj(), bg = "white", width = 10, height = 6, dpi = 300)
      }
    )
    ####
    output$download_detection_plot <- downloadHandler(
      filename = function() {
        paste0("detection_plot_", Sys.Date(), ".png")
      },
      content = function(file) {
        req(detection_plot_obj())
        ggsave(file, plot = detection_plot_obj(), bg = "white", width = 10, height = 6, dpi = 300)
      }
    )
    
    ###################################################################
    # Description Pages
    ###################################################################
    observeEvent(input$occr_description, {
      showModal(modalDialog(
        title = "Occurrence Plot",
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Close"),
        tagList(
          p("This plot shows the total number of minutes per day in which each species was acoustically detected at the selected deployment."),
          
          p("Each colored bar represents one day of detections. The height of the bar indicates how many minutes that species was present in the acoustic data for that day."),
          
          p("This visualization is useful for examining short-term and long-term occurrence patterns, identifying periods of high activity, and comparing species presence across the deployment timeline."),
          
          p("If an environmental variable is selected, it is displayed on a secondary axis to help identify relationships between detection patterns and environmental conditions.")
        )
      ))
    })
    
    observeEvent(input$call_count_description, {
      showModal(modalDialog(
        title = "Call Count Plot",
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Close"),
        tagList(
          p("This plot displays the total number of acoustic calls detected per day for each species at the selected deployment."),
          
          p("Unlike the Occurrence Plot, which shows the number of detected minutes per day, this figure shows the actual count of individual calls identified in the dataset."),
          
          p("Each bar represents one day of detections. The height or position on the plot corresponds to the total number of calls recorded for that species on that date."),
          
          p("This visualization is useful for examining calling behavior, comparing call rates across time, identifying peaks in acoustic activity, and distinguishing changes in daily calling patterns."),
          
          p("If an environmental variable is selected, it is displayed on a secondary axis to help identify relationships between call rates and environmental conditions.")
        )
      ))
    })
    
    observeEvent(input$call_den_description, {
      showModal(modalDialog(
        title = "Call Density Plot",
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Close"),
        tagList(
          p("This plot displays the normalized call density for each species across all recorded days at the selected deployment."),
          
          p("Call density represents the relative concentration of calls rather than the total number of detections. 
         The density curve shows how calling activity is distributed through time, highlighting periods of increased or decreased vocal behavior."),
          
          p("Unlike the Occurrence Plot (detected minutes per day) or the Call Count Plot (number of calls per day), 
         this plot emphasizes the overall shape and intensity of calling patterns, making it easy to compare calling behavior across species."),
          
          p("Each species is shown as a smoothed density curve, normalized so species with different call numbers can be compared on the same scale."),
          
          p("If an environmental variable is selected, it appears as a line overlay with a secondary axis, enabling comparisons between calling activity and environmental conditions.")
        )
      ))
    })
    
    observeEvent(input$call_measurment_description, {
      showModal(modalDialog(
        title = "Call Measurement Plot",
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Close"),
        tagList(
          p("This plot displays the distribution of selected acoustic call measurements (e.g., duration, maximum frequency, median frequency, frequency spread, and other variables)."),
          
          p("Each violin represents the full distribution of values for the chosen variable(s), grouped by the selected species and event(s). 
         This allows users to visually compare how call characteristics differ across categories."),
          
          tags$ul(
            tags$li(strong("Violin plots:"), " show the shape and spread of the data, highlighting patterns such as skewness, multimodality, and overall variability."),
            tags$li(strong("Filters:"), " Users can filter by species, detector type, acoustic event, and measurement variable using the dropdown selectors above.")
          ),
          
          p("This tool is especially useful for examining how call structure varies between species or across different acoustic events.")
        )
      ))
    })
    
    observeEvent(input$presence_description, {
      showModal(modalDialog(
        title = "Presence Plot",
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Close"),
        tagList(
          p("This plot displays species presence, showing how call counts vary over time of day and across the deployment period."),
          
          p("Each tile represents a specific combination of date (x-axis) and time of day (y-axis). 
         The color intensity of the tile corresponds to the number of detected calls within that time window."),
          
          tags$ul(
            tags$li(strong("X-axis:"), " Calendar date, showing how detections change over days or months."),
            tags$li(strong("Y-axis:"), " Time of day, allowing users to identify diel calling patterns (e.g., dawn/dusk peaks)."),
            tags$li(strong("Color scale:"), " Represents the call count—darker or more saturated colors indicate higher presence.")
          ),
          
          p("This visualization is useful for identifying daily or seasonal patterns in calling behavior, detecting shifts in activity over time, and comparing presence across species or detectors.")
        )
      ))
    })
    
    observeEvent(input$detection_description, {
      showModal(modalDialog(
        title = "Detections Plot",
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Close"),
        tagList(
          p("This plot displays each individual call detection as a point, allowing users to see the precise timing of every detected call throughout the deployment."),
          
          p("Every point corresponds to a single detection event, plotted by its date and exact time of day. 
         This high-resolution view reveals fine-scale temporal patterns, clustering of detections, and potential behavioral rhythms."),
          
          tags$ul(
            tags$li(strong("X-axis:"), " Calendar date, showing how detections are distributed over the deployment period."),
            tags$li(strong("Y-axis:"), " Time of day (24-hour clock), enabling users to identify diel patterns or recurring detection windows."),
            tags$li(strong("Points:"), " Each point represents one detected call. Dense clusters indicate periods of increased calling activity.")
          ),
          
          p("This visualization is particularly useful for exploring the timing and frequency of individual detection events, 
         identifying bursts of calling, and comparing fine-scale temporal patterns across species, deployments, or detectors.")
        )
      ))
    })
  })
}
    
## To be copied in the UI
# mod_analysis_ui("analysis_1")
    
## To be copied in the server
# mod_analysis_server("analysis_1")
