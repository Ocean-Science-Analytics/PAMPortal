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
          width: auto;
          display: inline-block;
          white-space: nowrap;
        }
        .custom-btn:hover {
          background-color: #8DB6CD !important;
          box-shadow: 0 0 0 3px rgba(141, 182, 205, 0.6);
          cursor: pointer;
        }
        .custom-btn-success:hover {
          background-color: darkolivegreen43!important;
          box-shadow: 0 0 0 3px rgba(141, 182, 205, 0.6);
          cursor: pointer;
        }
        .load-container {
          align-items: flex-start !important; /* move spinner to the top */
          padding-top: 20px;                  /* add some spacing from top */
        }
        .location-spinner {
          display: inline-block;
          margin-left: 8px;
          vertical-align: middle;
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
                div(
                  style = "display: flex; align-items: center; gap: 8px;",
                  div(style = "flex: 1; min-width: 0;",
                      selectInput(ns("location_occr"), "Select Location", choices = NULL, width = "100%")
                  ),
                  div(
                    id = ns("spinner_occr"),
                    style = "display: none; margin-top: 18px;",
                    tags$span(
                      class = "spinner-border spinner-border-sm text-secondary",
                      role  = "status",
                      style = "width: 1.2rem; height: 1.2rem;"
                    )
                  )
                ),
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
                div(
                  style = "display: flex; align-items: center; gap: 8px;",
                  div(style = "flex: 1; min-width: 0;",
                      selectInput(ns("location_presence"), "Select Location", choices = NULL, width = "100%")
                  ),
                  div(
                    id = ns("spinner_presence"),
                    style = "display: none; margin-top: 18px;",
                    tags$span(
                      class = "spinner-border spinner-border-sm text-secondary",
                      role  = "status",
                      style = "width: 1.2rem; height: 1.2rem;"
                    )
                  )
                ),
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
                div(
                  style = "display: flex; align-items: center; gap: 8px;",
                  div(style = "flex: 1; min-width: 0;",
                      selectInput(ns("location_detection"), "Select Location", choices = NULL, width = "100%")
                  ),
                  div(
                    id = ns("spinner_detection"),
                    style = "display: none; margin-top: 18px;",
                    tags$span(
                      class = "spinner-border spinner-border-sm text-secondary",
                      role  = "status",
                      style = "width: 1.2rem; height: 1.2rem;"
                    )
                  )
                ),
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
                div(
                  style = "display: flex; align-items: center; gap: 8px;",
                  div(style = "flex: 1; min-width: 0;",
                      selectInput(ns("location_call_count"), "Select Location", choices = NULL, width = "100%")
                  ),
                  div(
                    id = ns("spinner_call_count"),
                    style = "display: none; margin-top: 18px;",
                    tags$span(
                      class = "spinner-border spinner-border-sm text-secondary",
                      role  = "status",
                      style = "width: 1.2rem; height: 1.2rem;"
                    )
                  )
                ),
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
                div(
                  style = "display: flex; align-items: center; gap: 8px;",
                  div(style = "flex: 1; min-width: 0;",
                      selectInput(ns("location_call_den"), "Select Location", choices = NULL, width = "100%")
                  ),
                  div(
                    id = ns("spinner_call_den"),
                    style = "display: none; margin-top: 18px;",
                    tags$span(
                      class = "spinner-border spinner-border-sm text-secondary",
                      role  = "status",
                      style = "width: 1.2rem; height: 1.2rem;"
                    )
                  )
                ),
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
                div(
                  style = "display: flex; align-items: center; gap: 8px;",
                  div(style = "flex: 1; min-width: 0;",
                      selectInput(ns("location_dis"), "Select Location", choices = NULL, width = "100%", multiple = FALSE)
                  ),
                  div(
                    id = ns("spinner_dis"),
                    style = "display: none; margin-top: 18px;",
                    tags$span(
                      class = "spinner-border spinner-border-sm text-secondary",
                      role  = "status",
                      style = "width: 1.2rem; height: 1.2rem;"
                    )
                  )
                ),
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
      ),
      tabPanel(
        "Deep Acoustics",
        br(),
        div(style = "border: 2px solid black; border-radius: 8px; padding: 15px; margin-bottom: 10px; box-shadow: 0 8px 10px rgba(0,0,0.08,0.4);",
            fluidRow(
              column(
                width = 3,
                div(
                  style = "display: flex; align-items: center; gap: 8px;",
                  div(style = "flex: 1; min-width: 0;",
                      selectInput(ns("location_deep_acou"), "Select Location", choices = NULL, width = "100%")
                  ),
                  div(
                    id = ns("spinner_call_den"),
                    style = "display: none; margin-top: 18px;",
                    tags$span(
                      class = "spinner-border spinner-border-sm text-secondary",
                      role  = "status",
                      style = "width: 1.2rem; height: 1.2rem;"
                    )
                  )
                ),
                selectInput(
                  inputId = ns("grouping_deep_acou"),
                  label   = "Time Increment",
                  choices = c("day", "week", "month"),
                  multiple = FALSE,
                  selected = "day"
                )
              ),
              column(
                width = 3,
                selectInput(ns("species_filter_deep_acou"), "Select Species", choices = c("Fin whale", "Blue whale", "Sei whale"), multiple = TRUE),
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
                selectInput(ns("month_filter_deep_acou"), "Select Month", choices = c("All", month.name), selected = "All", multiple = TRUE)
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
                  actionButton(ns("render_deep_acou"), "Render Plot", icon = shiny::icon("file-lines"), class = "custom-btn")
                ),
                downloadButton(ns("download_deep_acou_plot"), "Download Plot", class = "btn-success custom-btn-success")
              )
            ),
        ),
        div(
          style = "display: flex; justify-content: flex-end; width: 100%; padding-right: 20px;",
          actionButton(ns("deep_acou_description"), "", icon = shiny::icon("question"), class = "custom-btn")
        ),
        br(),
        withSpinner(plotOutput(ns("deep_acou_plot"), height = "750px"), type = 4, color = "#001f3f", caption="Loading Deep Acoustic Plot...")
        #plotOutput(ns("effort_plot"),height = "600px")
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
    
    #########################################################################
    # Function and variables to disable when dataset is loading
    #########################################################################
    set_loading <- function(spinner_id, input_ids, loading = TRUE) {
      if (loading) {
        shinyjs::show(spinner_id)
        for (id in input_ids) shinyjs::disable(id)
      } else {
        shinyjs::hide(spinner_id)
        for (id in input_ids) shinyjs::enable(id)
      }
    }
    
    # Define which inputs to disable per tab
    occr_inputs      <- c("species_filter_occr", "month_filter_occr", 
                          "env_var_occr", "show_effort", "render_occr",
                          "download_occr_plot", "occr_description")
    
    call_count_inputs <- c("species_filter_call_count", "month_filter_call_count",
                           "env_var_call_count", "log_scale", "render_call_count",
                           "download_call_count_plot", "call_count_description")
    
    call_den_inputs  <- c("species_filter_call_den", "month_filter_call_den",
                          "env_var_call_den", "render_call_den",
                          "download_call_den_plot", "call_den_description")
    
    presence_inputs  <- c("species_filter_presence", "month_filter_presence",
                          "metric_presence", "log_scale_presence", "render_presence",
                          "download_presence_plot", "presence_description")
    
    detection_inputs <- c("species_filter_detection", "month_filter_detection",
                          "see_duty_detection", "render_plot_detection",
                          "download_detection_plot", "detection_description")
    
    dis_inputs       <- c("species_filter", "event_filter", "distribution_variable",
                          "detector_filter", "render_distribution",
                          "download_distribution_plot", "call_measurment_description")
    
    deep_acou_inputs <- c()
    
    #########################################################################
    # Initial Paths and Functions for this Module
    #########################################################################
    library(lubridate)
    call_count_plot_obj <- reactiveVal(NULL)
    call_den_plot_obj <- reactiveVal(NULL)
    presence_plot_obj <- reactiveVal(NULL)
    plot_measurements_obj <- reactiveVal(NULL)
    occurrence_plot_obj <- reactiveVal(NULL)
    detection_plot_obj <- reactiveVal(NULL)
    deep_acou_plot_obj <- reactiveVal(NULL)
    
    base_path <- reactive({
      req(data$selected_dir())
    })
    
    load_rds <- function(name) {
      get_rds(
        name            = name,
        rds_paths       = data$rds_paths(),
        rds_cache_val   = data$rds_cache(),
        update_cache_fn = data$rds_cache
      )
    }
    
    #########################################################################
    # Update Input choices
    #########################################################################
    observeEvent(data$rds_names(), {
      req(data$rds_names())
      locations <- data$rds_names() 
      updateSelectInput(session, "location_call_count", choices = locations)
      updateSelectInput(session, "location_call_den", choices = locations)
      updateSelectInput(session, "location_presence", choices = locations)
      updateSelectInput(session, "location_dis", choices = locations)
      updateSelectInput(session, "location_occr", choices = locations)
      updateSelectInput(session, "location_detection", choices = locations)
      updateSelectInput(session, "location_deep_acou", choices = locations)
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
      req(data$rds_paths(), input$location_dis)
      set_loading("spinner_dis", dis_inputs, loading = TRUE)
      on.exit(set_loading("spinner_dis", dis_inputs, loading = FALSE))
      selected_data <- lapply(setNames(input$location_dis, input$location_dis), load_rds)
      event_choices <- unique(unlist(lapply(selected_data, function(x) names(x@events))))
      species_choices <- unique(unlist(lapply(selected_data, function(x) {
        sapply(x@events, function(ev) ev@species$id)
      })))
      updateSelectInput(session, "event_filter",   choices = c("All", event_choices),   selected = "All")
      updateSelectInput(session, "species_filter", choices = c("All", species_choices), selected = "All")
    })
    
    ###################################################################
    # Occurance Plot
    ###################################################################
    observeEvent(input$location_occr, {
      req(input$location_occr)
      set_loading("spinner_occr", occr_inputs, loading = TRUE)
      on.exit(set_loading("spinner_occr", occr_inputs, loading = FALSE))
      rds <- load_rds(input$location_occr)
      req(!is.null(rds))
      species <- unique(unlist(lapply(rds@events, function(ev) ev@species$id)))
      updateSelectInput(session, "species_filter_occr", choices = c("All", species), selected = "All")
    })
    
    occurrence_plot_obj <- eventReactive(input$render_occr, {
      req(base_path(), input$location_occr) #input$species_filter_occr
      # Warn if All species selected
      if ("All" %in% input$species_filter_occr) {
        showNotification(
          "Gathering data for all species — this may take a little longer depending on the size of the dataset.",
          type     = "warning",
          duration = 8
        )
      } else {
        showNotification("Rendering Occurrence Plot...", type = "message")
      }
      
      if (length(input$month_filter_occr) == 0) {
        showNotification("Occurence Plot Stopped", type = "error", duration = 8)
        showNotification("Please select specific months to view or set months to 'All'.", type = "warning", duration = 8)
        stop("Please select specific months to view or set months to 'All'.")
      }
      
      if (length(input$species_filter_occr) == 0) {
        showNotification("Occurence Plot Stopped", type = "error", duration = 8)
        showNotification("Please select specific species to view or set species to 'All'.", type = "warning", duration = 8)
        stop("Please select specific species to view or set species to 'All'.")
      }
      
      plot_occurrence(
        location = input$location_occr,
        base_path = base_path(),
        species_of_interest = input$species_filter_occr,
        months_of_interest = input$month_filter_occr,
        environmental_variable = input$env_var_occr,
        show_effort = input$show_effort,
        load_rds_fn = load_rds
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
      set_loading("spinner_call_count", call_count_inputs, loading = TRUE)
      on.exit(set_loading("spinner_call_count", call_count_inputs, loading = FALSE))
      rds <- load_rds(input$location_call_count)
      req(!is.null(rds))
      species <- unique(unlist(lapply(rds@events, function(ev) ev@species$id)))
      updateSelectInput(session, "species_filter_call_count", choices = c("All", species), selected = "All")
    })
    
    call_count_plot_obj <- eventReactive(input$render_call_count, {
      req(base_path(), input$location_call_count)
      showNotification("Loading Call Count Plot...", type = "message")
      
      if (length(input$month_filter_call_count) == 0) {
        showNotification("Call Count Plot Stopped", type = "error", duration = 8)
        showNotification("Please select specific months to view or set months to 'All'.", type = "warning", duration = 8)
        stop("Please select specific months to view or set months to 'All'.")
      }
      
      if (length(input$species_filter_call_count) == 0) {
        showNotification("Call Count Plot Stopped", type = "error", duration = 8)
        showNotification("Please select specific species to view or set species to 'All'.", type = "warning", duration = 8)
        stop("Please select specific species to view or set species to 'All'.")
      }
      
      plot_call_count(
        location = input$location_call_count,
        base_path = base_path(),
        species_of_interest = input$species_filter_call_count,
        months_of_interest = input$month_filter_call_count,
        environmental_variable = input$env_var_call_count,
        log_scale = input$log_scale,
        load_rds_fn = load_rds
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
      set_loading("spinner_call_den", call_den_inputs, loading = TRUE)
      on.exit(set_loading("spinner_call_den", call_den_inputs, loading = FALSE))
      rds <- load_rds(input$location_call_den)
      req(!is.null(rds))
      species <- unique(unlist(lapply(rds@events, function(ev) ev@species$id)))
      updateSelectInput(session, "species_filter_call_den", choices = c("All", species), selected = "All")
    })
    
    
    call_den_plot_obj <- eventReactive(input$render_call_den, {
      req(base_path(), input$location_call_den)
      showNotification("Loading Call Density Plot...", type = "message")
      
      if (length(input$species_filter_call_den) == 0) {
        showNotification("Density Plot Stopped", type = "error", duration = 8)
        showNotification("Please select specific species to view or set species to 'All'.", type = "warning", duration = 8)
        stop("Please select specific species to view or set species to 'All'.")
      }
      if (length(input$month_filter_call_den) == 0) {
        showNotification("Density Plot Stopped", type = "error", duration = 8)
        showNotification("Please select specific months to view or set months to 'All'.", type = "warning", duration = 8)
        stop("Please select specific months to view or set months to 'All'.")
      }
      
      plot_call_density(
        location = input$location_call_den,
        base_path = base_path(),
        species_of_interest = input$species_filter_call_den,
        months_of_interest = input$month_filter_call_den,
        environmental_variable = input$env_var_call_den,
        load_rds_fn = load_rds
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
      req(data$rds_paths(), input$location_dis, input$species_filter)
      
      # Only load locations that are actually needed
      selected_data <- lapply(
        setNames(input$location_dis, input$location_dis), 
        load_rds
      )
      
      filtered_events <- lapply(selected_data, function(loc_data) {
        if (is.null(loc_data)) return(list())
        Filter(function(ev) {
          "All" %in% input$species_filter || ev@species$id %in% input$species_filter
        }, loc_data@events)
      })
      
      event_choices <- unique(unlist(lapply(filtered_events, names)))
      updateSelectInput(session, "event_filter", 
                        choices  = c("All", event_choices), 
                        selected = "All")
    }, ignoreNULL = TRUE)
    
    plot_measurements_obj <- eventReactive(input$render_distribution, {
      req(base_path(), input$location_dis, input$distribution_variable)
      showNotification("Loading Call Measurement Plot...", type = "message")
      
      plot_measurements(
        location_list = input$location_dis,
        base_path = base_path(),
        events_of_interest = input$event_filter,
        variables_of_interest = input$distribution_variable,
        species = input$species_filter,
        detector_type = input$detector_filter,
        load_rds_fn = load_rds
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
      set_loading("spinner_presence", presence_inputs, loading = TRUE)
      on.exit(set_loading("spinner_presence", presence_inputs, loading = FALSE))
      rds <- load_rds(input$location_presence)
      req(!is.null(rds))
      species <- unique(unlist(lapply(rds@events, function(ev) ev@species$id)))
      updateSelectInput(session, "species_filter_presence", choices = c("All", species), selected = "All")
    })
    
    presence_plot_obj <- eventReactive(input$render_presence, {
      req(base_path(), input$location_presence)
      showNotification("Loading Diel Presence Plot...", type = "message")
      
      if (length(input$month_filter_presence) == 0) {
        showNotification("Presence Plot Stopped", type = "error", duration = 8)
        showNotification("Please select specific months to view or set months to 'All'.", type = "warning", duration = 8)
        stop("Please select specific months to view or set months to 'All'.")
      }
      
      if (length(input$species_filter_presence) == 0) {
        showNotification("Presence Plot Stopped", type = "error", duration = 8)
        showNotification("Please select specific species to view or set species to 'All'.", type = "warning", duration = 8)
        stop("Please select specific species to view or set species to 'All'.")
      }
      
      plot_hourly_presence(
        location           = input$location_presence,
        base_path          = base_path(),
        species_of_interest = input$species_filter_presence,
        months_of_interest  = input$month_filter_presence,
        metric             = input$metric_presence,
        log_scale          = input$log_scale_presence,
        load_rds_fn        = load_rds    # <-- pass the cache function
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
      set_loading("spinner_detection", detection_inputs, loading = TRUE)
      on.exit(set_loading("spinner_detection", detection_inputs, loading = FALSE))
      rds <- load_rds(input$location_detection)
      req(!is.null(rds))
      species <- unique(unlist(lapply(rds@events, function(ev) ev@species$id)))
      updateSelectInput(session, "species_filter_detection", choices = c("All", species), selected = "All")
    })
    
    detection_plot_obj <- eventReactive(input$render_plot_detection, {
      req(base_path(), input$location_detection)
      showNotification("Loading Diel Detection Plot...", type = "message")
      
      if (length(input$month_filter_detection) == 0) {
        showNotification("Detection Plot Stopped", type = "error", duration = 8)
        showNotification("Please select specific months to view or set months to 'All'.", type = "warning", duration = 8)
        stop("Please select specific months to view or set months to 'All'.")
      }
      
      if (length(input$species_filter_detection) == 0) {
        showNotification("Detection Plot Stopped", type = "error", duration = 8)
        showNotification("Please select specific species to view or set species to 'All'.", type = "warning", duration = 8)
        stop("Please select specific species to view or set species to 'All'.")
      }

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
        see_duty_cycle = input$see_duty_detection,
        load_rds_fn = load_rds
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
    # Deep Acoustics Plot
    ###################################################################
    observeEvent(input$location_deep_acou, {
      req(input$location_deep_acou)
      set_loading("spinner_deep_acou", call_den_inputs, loading = TRUE)
      on.exit(set_loading("spinner_deep_acou", call_den_inputs, loading = FALSE))
      rds <- load_rds(input$location_deep_acou)
      req(!is.null(rds))
      species <- unique(unlist(lapply(rds@events, function(ev) ev@species$id)))
      updateSelectInput(session, "species_filter_deep_acou", choices = c("Fin whale", "Blue whale", "Sei whale"), selected = "Fin whale")
    })
    
    
    deep_acou_plot_obj <- eventReactive(input$render_deep_acou, {
      req(base_path(), input$location_deep_acou)
      
      # Check if DeepAcoustics folder exists for this dataset
      da_folder <- file.path(base_path(), "DeepAcoustics")
      if (!dir.exists(da_folder)) {
        showNotification("No Deep Acoustics data available for this dataset", type = "warning", duration = 8)
        return(NULL)
      }
      
      showNotification("Loading Deep Acoustics Plot...", type = "message")
      
      if (length(input$species_filter_deep_acou) == 0) {
        showNotification("Deep Acoustics Stopped", type = "error", duration = 8)
        showNotification("Please select specific species to view or set species to 'All'.", type = "warning", duration = 8)
        stop("Please select specific species to view or set species to 'All'.")
      }
      if (length(input$month_filter_deep_acou) == 0) {
        showNotification("Deep Acoustics Stopped", type = "error", duration = 8)
        showNotification("Please select specific months to view or set months to 'All'.", type = "warning", duration = 8)
        stop("Please select specific months to view or set months to 'All'.")
      }
      
      plot_deep_acoustics(
        location = input$location_deep_acou,
        base_path = base_path(),
        species_of_interest = input$species_filter_deep_acou,
        months_of_interest = input$month_filter_deep_acou,
        grouping = input$grouping_deep_acou,
        load_rds_fn = load_rds
        #log_scale = input$log_scale
      )
    }, ignoreNULL = TRUE)
    
    output$deep_acou_plot <- renderPlot({
      req(deep_acou_plot_obj())
      deep_acou_plot_obj()
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
    ###
    output$download_deep_acou_plot <- downloadHandler(
      filename = function() {
        paste0("deep_acoustic_plot_", Sys.Date(), ".png")
      },
      content = function(file) {
        req(deep_acou_plot_obj())
        ggsave(file, plot = deep_acou_plot_obj(), bg = "white", width = 10, height = 6, dpi = 300)
      }
    )
    
    ###################################################################
    # Description Pages
    ###################################################################
    observeEvent(input$occr_description, {
      showModal(modalDialog(
        title = div(
          style = "display: flex; align-items: center; gap: 10px;",
          shiny::icon("calendar-days", style = "color: #00688B; font-size: 1.2em;"),
          span("Daily Occurrence Plot", style = "font-weight: bold; color: #001f3f;")
        ),
        size = "l", easyClose = TRUE, footer = modalButton("Close"),
        tagList(
          div(
            style = "display: flex; gap: 12px; align-items: flex-start;
                 padding: 12px; border-radius: 8px;
                 background-color: #e8f4fd; border-left: 4px solid #00688B;
                 margin-bottom: 12px;",
            shiny::icon("chart-bar", style = "color: #00688B; font-size: 1.2em; margin-top: 2px; flex-shrink: 0;"),
            div(
              tags$b("Overview"),
              tags$p(style = "margin: 6px 0 0 0; font-size: 0.9rem; color: #555;",
                     "This plot shows the total number of minutes per day in which each species 
             was acoustically detected at the selected deployment. It is useful for examining 
             short-term and long-term occurrence patterns and identifying periods of high activity."
              )
            )
          ),
          div(
            style = "display: flex; gap: 12px; align-items: flex-start;
                 padding: 12px; border-radius: 8px;
                 background-color: #fff8e1; border-left: 4px solid #CDAD00;
                 margin-bottom: 12px;",
            shiny::icon("layer-group", style = "color: #CDAD00; font-size: 1.2em; margin-top: 2px; flex-shrink: 0;"),
            div(
              tags$b("Plot Details"),
              tags$ul(
                style = "font-size: 0.9rem; color: #555; margin-top: 4px;",
                tags$li(tags$b("X-axis: "), "Calendar date across the deployment period."),
                tags$li(tags$b("Y-axis: "), "Number of minutes with at least one detection."),
                tags$li(tags$b("Bars: "), "Each bar represents one day. Height indicates detected minutes for that species."),
                tags$li(tags$b("Facets: "), "Each species is shown in its own panel for easy comparison.")
              )
            )
          ),
          div(
            style = "display: flex; gap: 10px; align-items: flex-start;
                 padding: 10px 12px; background-color: #f9f9f9;
                 border-radius: 6px; border: 1px solid #ddd;
                 font-size: 0.82rem; color: #666;",
            shiny::icon("circle-info", style = "color: #00688B; flex-shrink: 0; margin-top: 2px;"),
            "If an environmental variable is selected, it is overlaid as a line on a secondary 
         axis to help identify relationships between detection patterns and environmental conditions."
          )
        )
      ))
    })
    
    observeEvent(input$call_count_description, {
      showModal(modalDialog(
        title = div(
          style = "display: flex; align-items: center; gap: 10px;",
          shiny::icon("hashtag", style = "color: #00688B; font-size: 1.2em;"),
          span("Call Count Plot", style = "font-weight: bold; color: #001f3f;")
        ),
        size = "l", easyClose = TRUE, footer = modalButton("Close"),
        tagList(
          div(
            style = "display: flex; gap: 12px; align-items: flex-start;
                 padding: 12px; border-radius: 8px;
                 background-color: #e8f4fd; border-left: 4px solid #00688B;
                 margin-bottom: 12px;",
            shiny::icon("chart-bar", style = "color: #00688B; font-size: 1.2em; margin-top: 2px; flex-shrink: 0;"),
            div(
              tags$b("Overview"),
              tags$p(style = "margin: 6px 0 0 0; font-size: 0.9rem; color: #555;",
                     "This plot displays the total number of acoustic calls detected per day for each 
             species. Unlike the Occurrence Plot which shows detected minutes, this figure 
             counts individual calls identified in the dataset."
              )
            )
          ),
          div(
            style = "display: flex; gap: 12px; align-items: flex-start;
                 padding: 12px; border-radius: 8px;
                 background-color: #fff8e1; border-left: 4px solid #CDAD00;
                 margin-bottom: 12px;",
            shiny::icon("layer-group", style = "color: #CDAD00; font-size: 1.2em; margin-top: 2px; flex-shrink: 0;"),
            div(
              tags$b("Plot Details"),
              tags$ul(
                style = "font-size: 0.9rem; color: #555; margin-top: 4px;",
                tags$li(tags$b("X-axis: "), "Calendar date across the deployment period."),
                tags$li(tags$b("Y-axis: "), "Total number of individual calls recorded that day."),
                tags$li(tags$b("Bars: "), "Each bar represents one day of detections."),
                tags$li(tags$b("Facets: "), "Each species is shown in its own panel.")
              )
            )
          ),
          div(
            style = "display: flex; gap: 10px; align-items: flex-start;
                 padding: 10px 12px; background-color: #f9f9f9;
                 border-radius: 6px; border: 1px solid #ddd;
                 font-size: 0.82rem; color: #666;",
            shiny::icon("circle-info", style = "color: #00688B; flex-shrink: 0; margin-top: 2px;"),
            "If an environmental variable is selected, it is overlaid as a line on a secondary 
         axis to help identify relationships between call rates and environmental conditions."
          )
        )
      ))
    })
    
    observeEvent(input$call_den_description, {
      showModal(modalDialog(
        title = div(
          style = "display: flex; align-items: center; gap: 10px;",
          shiny::icon("wave-square", style = "color: #00688B; font-size: 1.2em;"),
          span("Call Density Plot", style = "font-weight: bold; color: #001f3f;")
        ),
        size = "l", easyClose = TRUE, footer = modalButton("Close"),
        tagList(
          div(
            style = "display: flex; gap: 12px; align-items: flex-start;
                 padding: 12px; border-radius: 8px;
                 background-color: #e8f4fd; border-left: 4px solid #00688B;
                 margin-bottom: 12px;",
            shiny::icon("chart-area", style = "color: #00688B; font-size: 1.2em; margin-top: 2px; flex-shrink: 0;"),
            div(
              tags$b("Overview"),
              tags$p(style = "margin: 6px 0 0 0; font-size: 0.9rem; color: #555;",
                     "This plot displays the normalized call density for each species, showing how 
             calling activity is distributed through time. Unlike call count or occurrence 
             plots, this emphasizes the overall shape and intensity of calling patterns."
              )
            )
          ),
          div(
            style = "display: flex; gap: 12px; align-items: flex-start;
                 padding: 12px; border-radius: 8px;
                 background-color: #fff8e1; border-left: 4px solid #CDAD00;
                 margin-bottom: 12px;",
            shiny::icon("layer-group", style = "color: #CDAD00; font-size: 1.2em; margin-top: 2px; flex-shrink: 0;"),
            div(
              tags$b("Plot Details"),
              tags$ul(
                style = "font-size: 0.9rem; color: #555; margin-top: 4px;",
                tags$li(tags$b("X-axis: "), "Calendar date across the deployment period."),
                tags$li(tags$b("Y-axis: "), "Normalized call density — relative concentration of calls over time."),
                tags$li(tags$b("Curves: "), "Smoothed density curves, normalized so species with different call volumes can be compared on the same scale."),
                tags$li(tags$b("Facets: "), "Each species is shown in its own panel.")
              )
            )
          ),
          div(
            style = "display: flex; gap: 10px; align-items: flex-start;
                 padding: 10px 12px; background-color: #f9f9f9;
                 border-radius: 6px; border: 1px solid #ddd;
                 font-size: 0.82rem; color: #666;",
            shiny::icon("circle-info", style = "color: #00688B; flex-shrink: 0; margin-top: 2px;"),
            "If an environmental variable is selected, it appears as a line overlay with a 
         secondary axis, enabling comparisons between calling activity and environmental conditions."
          )
        )
      ))
    })
    
    observeEvent(input$call_measurment_description, {
      showModal(modalDialog(
        title = div(
          style = "display: flex; align-items: center; gap: 10px;",
          shiny::icon("ruler", style = "color: #00688B; font-size: 1.2em;"),
          span("Call Measurements Plot", style = "font-weight: bold; color: #001f3f;")
        ),
        size = "l", easyClose = TRUE, footer = modalButton("Close"),
        tagList(
          div(
            style = "display: flex; gap: 12px; align-items: flex-start;
                 padding: 12px; border-radius: 8px;
                 background-color: #e8f4fd; border-left: 4px solid #00688B;
                 margin-bottom: 12px;",
            shiny::icon("chart-simple", style = "color: #00688B; font-size: 1.2em; margin-top: 2px; flex-shrink: 0;"),
            div(
              tags$b("Overview"),
              tags$p(style = "margin: 6px 0 0 0; font-size: 0.9rem; color: #555;",
                     "This plot displays the distribution of selected acoustic call measurements 
             such as duration, maximum frequency, median frequency, and frequency spread. 
             It is useful for comparing call structure across species or acoustic events."
              )
            )
          ),
          div(
            style = "display: flex; gap: 12px; align-items: flex-start;
                 padding: 12px; border-radius: 8px;
                 background-color: #fff8e1; border-left: 4px solid #CDAD00;
                 margin-bottom: 12px;",
            shiny::icon("layer-group", style = "color: #CDAD00; font-size: 1.2em; margin-top: 2px; flex-shrink: 0;"),
            div(
              tags$b("Plot Details"),
              tags$ul(
                style = "font-size: 0.9rem; color: #555; margin-top: 4px;",
                tags$li(tags$b("Violin plots: "), "Show the full distribution of values, highlighting skewness, multimodality, and variability."),
                tags$li(tags$b("Box plots: "), "Overlaid on each violin to show median and interquartile range."),
                tags$li(tags$b("X-axis: "), "Acoustic events, grouped by the selected species."),
                tags$li(tags$b("Y-axis: "), "Measurement value in the appropriate unit (Hz, seconds, etc.)"),
                tags$li(tags$b("Facets: "), "Each selected measurement variable is shown in its own panel.")
              )
            )
          ),
          div(
            style = "display: flex; gap: 10px; align-items: flex-start;
                 padding: 10px 12px; background-color: #f9f9f9;
                 border-radius: 6px; border: 1px solid #ddd;
                 font-size: 0.82rem; color: #666;",
            shiny::icon("circle-info", style = "color: #00688B; flex-shrink: 0; margin-top: 2px;"),
            "Use the dropdown selectors to filter by species, detector type (Whistle & Moan or Click), 
         acoustic event, and measurement variable."
          )
        )
      ))
    })
    
    observeEvent(input$presence_description, {
      showModal(modalDialog(
        title = div(
          style = "display: flex; align-items: center; gap: 10px;",
          shiny::icon("table-cells", style = "color: #00688B; font-size: 1.2em;"),
          span("Diel Density Plot", style = "font-weight: bold; color: #001f3f;")
        ),
        size = "l", easyClose = TRUE, footer = modalButton("Close"),
        tagList(
          div(
            style = "display: flex; gap: 12px; align-items: flex-start;
                 padding: 12px; border-radius: 8px;
                 background-color: #e8f4fd; border-left: 4px solid #00688B;
                 margin-bottom: 12px;",
            shiny::icon("grid", style = "color: #00688B; font-size: 1.2em; margin-top: 2px; flex-shrink: 0;"),
            div(
              tags$b("Overview"),
              tags$p(style = "margin: 6px 0 0 0; font-size: 0.9rem; color: #555;",
                     "This heatmap displays species presence across both calendar date and time of day, 
             making it ideal for identifying diel patterns (e.g., dawn/dusk peaks) and how 
             calling behavior shifts across seasons."
              )
            )
          ),
          div(
            style = "display: flex; gap: 12px; align-items: flex-start;
                 padding: 12px; border-radius: 8px;
                 background-color: #fff8e1; border-left: 4px solid #CDAD00;
                 margin-bottom: 12px;",
            shiny::icon("layer-group", style = "color: #CDAD00; font-size: 1.2em; margin-top: 2px; flex-shrink: 0;"),
            div(
              tags$b("Plot Details"),
              tags$ul(
                style = "font-size: 0.9rem; color: #555; margin-top: 4px;",
                tags$li(tags$b("X-axis: "), "Calendar date across the deployment period."),
                tags$li(tags$b("Y-axis: "), "Time of day (24-hour clock)."),
                tags$li(tags$b("Color: "), "Represents call count or duration — more saturated colors indicate higher presence."),
                tags$li(tags$b("Shading: "), "Dark shading indicates nighttime hours based on local sunrise/sunset times."),
                tags$li(tags$b("Facets: "), "Each species is shown in its own panel.")
              )
            )
          ),
          div(
            style = "display: flex; gap: 10px; align-items: flex-start;
                 padding: 10px 12px; background-color: #f9f9f9;
                 border-radius: 6px; border: 1px solid #ddd;
                 font-size: 0.82rem; color: #666;",
            shiny::icon("circle-info", style = "color: #00688B; flex-shrink: 0; margin-top: 2px;"),
            "Switch between Count and Duration metrics using the Select Metric dropdown. 
         Enable Log Scale to better visualize data with large value ranges."
          )
        )
      ))
    })
    
    observeEvent(input$detection_description, {
      showModal(modalDialog(
        title = div(
          style = "display: flex; align-items: center; gap: 10px;",
          shiny::icon("circle-dot", style = "color: #00688B; font-size: 1.2em;"),
          span("Diel Detection Plot", style = "font-weight: bold; color: #001f3f;")
        ),
        size = "l", easyClose = TRUE, footer = modalButton("Close"),
        tagList(
          div(
            style = "display: flex; gap: 12px; align-items: flex-start;
                 padding: 12px; border-radius: 8px;
                 background-color: #e8f4fd; border-left: 4px solid #00688B;
                 margin-bottom: 12px;",
            shiny::icon("braille", style = "color: #00688B; font-size: 1.2em; margin-top: 2px; flex-shrink: 0;"),
            div(
              tags$b("Overview"),
              tags$p(style = "margin: 6px 0 0 0; font-size: 0.9rem; color: #555;",
                     "This plot displays each individual call detection as a point, providing a 
             high-resolution view of the precise timing of every detected call throughout 
             the deployment. Dense clusters indicate periods of increased calling activity."
              )
            )
          ),
          div(
            style = "display: flex; gap: 12px; align-items: flex-start;
                 padding: 12px; border-radius: 8px;
                 background-color: #fff8e1; border-left: 4px solid #CDAD00;
                 margin-bottom: 12px;",
            shiny::icon("layer-group", style = "color: #CDAD00; font-size: 1.2em; margin-top: 2px; flex-shrink: 0;"),
            div(
              tags$b("Plot Details"),
              tags$ul(
                style = "font-size: 0.9rem; color: #555; margin-top: 4px;",
                tags$li(tags$b("X-axis: "), "Calendar date across the deployment period."),
                tags$li(tags$b("Y-axis: "), "Time of day (24-hour clock)."),
                tags$li(tags$b("Points: "), "Each point represents one individual detected call."),
                tags$li(tags$b("Facets: "), "Each species is shown in its own panel.")
              )
            )
          ),
          div(
            style = "display: flex; gap: 10px; align-items: flex-start;
                 padding: 10px 12px; background-color: #f9f9f9;
                 border-radius: 6px; border: 1px solid #ddd;
                 font-size: 0.82rem; color: #666;",
            shiny::icon("circle-info", style = "color: #00688B; flex-shrink: 0; margin-top: 2px;"),
            "Enable Show Full Duty Cycle to display the complete recording window for each hour, 
         giving context to gaps in detection data relative to the instrument's duty cycle."
          )
        )
      ))
    })
    
    observeEvent(input$deep_acou_description, {
      showModal(modalDialog(
        title = div(
          style = "display: flex; align-items: center; gap: 10px;",
          shiny::icon("water", style = "color: #00688B; font-size: 1.2em;"),
          span("Deep Acoustics Plot", style = "font-weight: bold; color: #001f3f;")
        ),
        size      = "l",
        easyClose = TRUE,
        footer    = modalButton("Close"),
        tagList(
          
          # Overview
          div(
            style = "display: flex; gap: 12px; align-items: flex-start;
                 padding: 12px; border-radius: 8px;
                 background-color: #e8f4fd; border-left: 4px solid #00688B;
                 margin-bottom: 12px;",
            shiny::icon("chart-bar", 
                        style = "color: #00688B; font-size: 1.2em; margin-top: 2px; flex-shrink: 0;"),
            div(
              tags$b("Overview"),
              tags$p(
                style = "margin: 6px 0 0 0; font-size: 0.9rem; color: #555;",
                "This plot combines detection outputs from the ",
                tags$b("Deep Acoustics"), " program with PAMGuard detections to provide 
            a comprehensive view of low-frequency species activity over the deployment period."
              )
            )
          ),
          
          # Description
          div(
            style = "display: flex; gap: 12px; align-items: flex-start;
                 padding: 12px; border-radius: 8px;
                 background-color: #fff8e1; border-left: 4px solid #CDAD00;
                 margin-bottom: 12px;",
            shiny::icon("layer-group",
                        style = "color: #CDAD00; font-size: 1.2em; margin-top: 2px; flex-shrink: 0;"),
            div(
              tags$b("Plot Layers"),
              tags$p(
                style = "margin: 6px 0 0 0; font-size: 0.9rem; color: #555;",
                "The plot contains two overlaid data sources:"
              ),
              tags$ul(
                style = "font-size: 0.9rem; color: #555; margin-top: 4px;",
                tags$li(
                  tags$b("Stacked bars"), " — Deep Acoustics detections per time period, 
              colored by detection type (e.g. call type or confidence category)."
                ),
                tags$li(
                  tags$b("Solid line"), " — PAMGuard confirmed detections per time period."
                ),
                tags$li(
                  tags$b("Dashed line"), " — PAMGuard possible detections per time period 
              (includes both confirmed and possible calls)."
                )
              )
            )
          ),
          
          # Axes and controls
          div(
            style = "display: flex; gap: 12px; align-items: flex-start;
                 padding: 12px; border-radius: 8px;
                 background-color: #f4f6f8; border-left: 4px solid #aaa;
                 margin-bottom: 12px;",
            shiny::icon("sliders",
                        style = "color: #555; font-size: 1.2em; margin-top: 2px; flex-shrink: 0;"),
            div(
              tags$b("Axes and Controls"),
              tags$ul(
                style = "font-size: 0.9rem; color: #555; margin-top: 4px;",
                tags$li(tags$b("X-axis: "), "Calendar date or time, depending on the selected grouping interval."),
                tags$li(tags$b("Y-axis: "), "Number of detections per grouping interval (day, week, month, or hour)."),
                tags$li(tags$b("Species: "), "Filter to Fin whale, Blue whale, or Sei whale — each displayed in its own facet panel."),
                tags$li(tags$b("Grouping: "), "Controls the temporal resolution of the bars and lines (hour, day, week, or month)."),
                tags$li(tags$b("Months: "), "Optionally filter to specific months of the deployment.")
              )
            )
          ),
          
          # Note about data sources
          div(
            style = "display: flex; gap: 10px; align-items: flex-start;
                 padding: 10px 12px;
                 background-color: #f9f9f9;
                 border-radius: 6px;
                 border: 1px solid #ddd;
                 font-size: 0.82rem; color: #666;",
            shiny::icon("circle-info", style = "color: #00688B; flex-shrink: 0; margin-top: 2px;"),
            "PAMGuard lines will only appear if matching species detections exist in the RDS dataset 
         for the selected location. If no PAMGuard data is available, only the Deep Acoustics 
         bars will be shown."
          )
        )
      ))
    })
  })
}
    
## To be copied in the UI
# mod_analysis_ui("analysis_1")
    
## To be copied in the server
# mod_analysis_server("analysis_1")
