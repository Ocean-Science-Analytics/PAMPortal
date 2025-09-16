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

var_names <- c(freqBeg = "Beginning Frequency (Hz)",
               freqEnd = "Ending Frequency (Hz)",
               freqMean = "Mean Frequency (Hz)",
               freqStdDev = "Frequency Standard Deviation (Hz)",
               duration = 'Duration (s)',
               freqSlopeMean = 'Frequency Slope Mean (Hz)',
               freqSlopeRatio = 'Frequency Slope Ratio (Hz)',
               freqSpread= 'Frequency Spread (Hz)',
               freqMin = 'Minimum Frequency (Hz)',
               freqMax = 'Maximum Frequency (Hz)',
               freqRange = 'Frequency Range (Hz)',
               freqMedian = 'Frequency Median (Hz)',
               freqMaxMinRatio = 'Frequency Maximum:Minimum Ratio (Hz)',
               freqBegEndRatio = 'Frequency Beginning:End Ratio (Hz)',
               stepDur = 'Step Duration (s)')

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
          background-color: lightskyblue !important;
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
        div(style = "border: 2px solid black; border-radius: 8px; padding: 15px; margin-bottom: 10px;",
            fluidRow(
              column(
                width = 3,
                selectInput(ns("location_occr"), "Select Location", choices = NULL),
                #numericInput(ns("duty"), "Duty Cycle (min)", value = 60, min = 1, step = 1),
              ),
              column(
                width = 3,
                selectInput(ns("species_filter_occr"), "Select Species", choices = NULL, multiple = TRUE)
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
                downloadButton(ns("download_occr_plot"), "Download Plot", class = "btn-success")
              )
            ),
        ),
        div(
          style = "display: flex; justify-content: flex-end; width: 100%;",
          actionButton(ns("occr_description"), "", icon = shiny::icon("question"), class = "custom-btn")
        ),
        br(),
        # withSpinner(
        #   plotOutput(ns("occr_plot"), height = "700px"),
        #   type = 4,        # spinner style (1–8)
        #   color = "#001f3f" # customize color (green here)
        # )
        withSpinner(plotOutput(ns("occr_plot"), height = "700px"), type = 4, color = "#001f3f",caption="Loading Occurrence Plot...")
        #plotOutput(ns("occr_plot"),height = "600px")
      ),
      tabPanel(
        "Call Measurments",
        br(),
        div(style = "border: 2px solid black; border-radius: 8px; padding: 15px; margin-bottom: 10px;",
            fluidRow(
              column(
                width = 3,
                selectInput(ns("location_dis"), "Select Location", choices = NULL, multiple = TRUE),
                selectInput(ns("event_filter"), "Select Events", choices = c("All"), selected = "ALL", multiple = TRUE)
              ),
              column(
                width = 2,
                selectInput(ns("distribution_variable"), "Select Variable", selected = "freqbeg", choices = names(var_names)),
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
                downloadButton(ns("download_distribution_plot"), "Download Plot", class = "btn-success")
              )
            ),
        ),
        div(
          style = "display: flex; justify-content: flex-end; width: 100%;",
          actionButton(ns("dstrb_description"), "", icon = shiny::icon("question"), class = "custom-btn")
        ),
        br(),
        withSpinner(plotOutput(ns("distribution_plot"), height = "700px"), type = 4, color = "#001f3f",caption="Loading Distribution Plot...")
        #plotOutput(ns("distribution_plot"), height = "600px")
      ),
      tabPanel(
        "Effort & Detections",
        br(),
        div(style = "border: 2px solid black; border-radius: 8px; padding: 15px; margin-bottom: 10px;",
            fluidRow(
              column(
                width = 3,
                selectInput(ns("location"), "Select Location", choices = NULL),
                checkboxInput(ns("see_duty"), "Show Full Duty Cycle", value = FALSE),
                uiOutput(ns("duty_text"))
                #numericInput(ns("duty"), "Duty Cycle (min)", value = 60, min = 1, step = 1),
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
                  actionButton(ns("render_plot"), "Render Plot", icon = shiny::icon("file-lines"), class = "custom-btn")
                ),
                downloadButton(ns("download_effort_plot"), "Download Plot", class = "btn-success")
              )
            ),
        ),
        div(
          style = "display: flex; justify-content: flex-end; width: 100%;",
          actionButton(ns("effort_description"), "", icon = shiny::icon("question"), class = "custom-btn")
        ),
        br(),
        withSpinner(plotOutput(ns("effort_plot"), height = "700px"), type = 4, color = "#001f3f", caption="Loading Effort Plot...")
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
    library(lubridate)
    effort_plot_obj <- reactiveVal(NULL)
    distribution_plot_obj <- reactiveVal(NULL)
    occurrence_plot_obj <- reactiveVal(NULL)
    
    base_path <- reactive({
      req(data$selected_dir())
    })

    observeEvent(data$rds_data(), {
      req(data$rds_data())
      locations <- names(data$rds_data())
      updateSelectInput(session, "location", choices = locations)
      updateSelectInput(session, "location_dis", choices = locations)
      updateSelectInput(session, "location_occr", choices = locations)
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
    # Effort & Detection Plot
    ###################################################################
    effort_plot_obj <- eventReactive(input$render_plot, {
      req(base_path(), input$location)
      showNotification("Loading Effort & Detection Plot...", type = "message")
      
      dc_file <- file.path(base_path(), "Duty_Cycles.csv")
      duty_lookup <- if (file.exists(dc_file)) {
        dc_df <- read.csv(dc_file, stringsAsFactors = FALSE)
        setNames(dc_df$dc, dc_df$location)
      } else {
        list()
      }
      
      duty_min <- if (isTRUE(input$see_duty)) duty_lookup[[input$location]] else 60
      
      effort_plot(
        location = input$location,
        base_path = base_path(),
        see_duty_cycle = input$see_duty
        #duty_cycle_min = if (is.null(duty_min)) 60 else duty_min
      )
    }, ignoreNULL = TRUE)
    
    output$effort_plot <- renderPlot({
      req(effort_plot_obj())
      effort_plot_obj()
    })
    
    # still provide the duty text UI
    output$duty_text <- renderUI({
      req(input$see_duty, input$location)
      dc_file <- file.path(base_path(), "Duty_Cycles.csv")
      if (!file.exists(dc_file)) return(NULL)
      duty_cycles_df <- read.csv(dc_file, stringsAsFactors = FALSE)
      duty_lookup <- setNames(duty_cycles_df$dc, duty_cycles_df$location)
      #print(duty_lookup)
      duty_val <- duty_lookup[[input$location]]
      if (is.null(duty_val)) return(NULL)
      HTML(glue::glue("<div style='margin-top: 5px; color: #444;'>Duty cycle at <b>{input$location}</b> was <b>{duty_val} minutes</b> every hour.</div>"))
    })
    
    
    ###################################################################
    # Distribution Plot
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
    
    distribution_plot_obj <- eventReactive(input$render_distribution, {
      req(base_path(), input$location_dis, input$distribution_variable)
      showNotification("Loading Distribution Plot...", type = "message")
      
      distribution_plot(
        location_list = input$location_dis,
        base_path = base_path(),
        event_list = input$event_filter,
        variable = input$distribution_variable,
        species_list = input$species_filter
      )
    }, ignoreNULL = TRUE)
    
    output$distribution_plot <- renderPlot({
      req(distribution_plot_obj())
      distribution_plot_obj()
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
      
      occurrence_plot(
        location = input$location_occr,
        base_path = base_path(),
        species_list = input$species_filter_occr
      )
    }, ignoreNULL = TRUE)
    
    output$occr_plot <- renderPlot({
      req(occurrence_plot_obj())
      occurrence_plot_obj()
    })
    
    ###################################################################
    # Download Plot Logic
    ###################################################################
    output$download_effort_plot <- downloadHandler(
      filename = function() {
        paste0("effort_plot_", Sys.Date(), ".png")
      },
      content = function(file) {
        req(effort_plot_obj())
        ggsave(file, plot = effort_plot_obj(), width = 10, height = 6, dpi = 300)
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
    
    ###################################################################
    # Description Pages
    ###################################################################
    observeEvent(input$effort_description, {
      showModal(modalDialog(
        title = "Effort & Detection Plot Description",
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
        title = "Distribution Plot Description",
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Close"),
        tagList(
          p("This figure displays the distribution of selected call characteristics (e.g., duration, frequency max, frequency median, etc.)."),
          p("Each distribution corresponds to selected species and events, allowing comparison of call features across categories."),
          tags$ul(
            tags$li("Violin plots show the full distribution of values for the selected variable."),
            tags$li("The user can filter by acoustic event and species using the dropdowns above.")
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
