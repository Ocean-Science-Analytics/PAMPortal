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

var_names <- c(freqBeg = "Beginning Frequency",
               freqEnd = "Ending Frequency",
               freqMean = "Mean Frequency",
               freqStdDev = "Frequency Standard Deviation.",
               duration = 'Duration',
               freqSlopeMean = 'Frequency Slope Mean',
               freqSlopeRatio = 'Frequency Slope Ratio',
               freqSpread= 'Frequency Spread',
               freqMin = 'Minimum Frequency',
               freqMax = 'Maximum Frequency',
               freqRange = 'Frequency Range',
               freqMedian = 'Frequency Median',
               freqMaxMinRatio = 'Frequency Maximum:Minimum Ratio',
               freqBegEndRatio = 'Frequency Beginning:End Ratio',
               stepDur = 'Step Duration')

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
        "))
    ),
    
    tabsetPanel(
      tabPanel(
        "Effort & Detections",
        br(),
        div(style = "border: 2px solid black; border-radius: 8px; padding: 15px; margin-bottom: 20px;",
          fluidRow(
            column(
              width = 4,
              selectInput(ns("location"), "Select Location", choices = NULL),
              checkboxInput(ns("see_duty"), "Show Full Duty Cycle", value = FALSE),
            ),
            column(
              width = 2,
              numericInput(ns("duty"), "Duty Cycle (min)", value = 60, min = 1, step = 1),
            )
          ),
          fluidRow(
            column(
              width = 3,
              actionButton(ns("render_plot"), "Render Plot", icon = shiny::icon("file-lines"),
                           class = "custom-btn"
                           #style = "background-color: #00688B; color: white; border: none; width = 200px;"
                           )
            ),
            column(
              width = 3,
              downloadButton(ns("download_effort_plot"), "Download Plot", class = "btn-success")
            )
          )
        ),
        br(),
        plotOutput(ns("effort_plot"),height = "500px")
      ),
      tabPanel(
        "Distribution",
        br(),
        div(style = "border: 2px solid black; border-radius: 8px; padding: 15px;",
            fluidRow(
              column(
                width = 4,
                selectInput(ns("location_dis"), "Select Location", choices = NULL, multiple = TRUE),
                selectInput(ns("event_filter"), "Select Events", choices = c("All"), selected = "ALL", multiple = TRUE)
              ),
              column(
                width = 3,
                selectInput(ns("distribution_variable"), "Select Variable", selected = "freqbeg", choices = names(var_names))
              ),
              column(
                width = 3,
                selectInput(ns("species_filter"), "Select Species", choices = c("All"), selected = "ALL", multiple = TRUE)
              )
            ),
            fluidRow(
              column(
                width = 3,
                actionButton(ns("render_distribution"), "Render Plot", icon = shiny::icon("file-lines"),
                             class = "custom-btn"
                             #style = "background-color: #00688B; color: white; border: none; width = 200px;"
                             )
              ),
              column(
                width = 3,
                downloadButton(ns("download_distribution_plot"), "Download Plot", class = "btn-success")
              )
            )
        ),
        br(),
        plotOutput(ns("distribution_plot"), height = "600px")
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
    
    effort_plot_obj <- reactiveVal(NULL)
    distribution_plot_obj <- reactiveVal(NULL)
    
    base_path <- reactive({
      req(data$selected_dir())
    })

    observeEvent(data$rds_data(), {
      req(data$rds_data())
      locations <- names(data$rds_data())
      updateSelectInput(session, "location", choices = locations)
      updateSelectInput(session, "location_dis", choices = locations)
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
    observeEvent(input$render_plot, {
      effort_plot_obj({
        isolate({
          req(input$location, input$duty)
          showNotification("Loading Effort & Detection Plot...", type = "message")
          effort_plot(
            location = input$location,
            base_path = base_path(),
            see_duty_cycle = input$see_duty,
            duty_cycle_min = input$duty
          )
        })
      })
      
      output$effort_plot <- renderPlot({
        effort_plot_obj()
      })
    })
    
    
    ###################################################################
    # Distribution Plot
    ###################################################################
    observeEvent(input$render_distribution, {
      distribution_plot_obj({
        isolate({
          req(input$distribution_variable)
          showNotification("Loading Distribution Plot...", type = "message")
          distribution_plot(
            location_list = input$location_dis,
            base_path = base_path(),
            event_list = input$event_filter,
            variable = input$distribution_variable,
            species_list = input$species_filter
          )
        })
      })
      
      output$distribution_plot <- renderPlot({
        distribution_plot_obj()
      })
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
    
    output$download_distribution_plot <- downloadHandler(
      filename = function() {
        paste0("distribution_plot_", Sys.Date(), ".png")
      },
      content = function(file) {
        req(distribution_plot_obj())
        ggsave(file, plot = distribution_plot_obj(), width = 10, height = 6, dpi = 300)
      }
    )
    
  })
}
    
## To be copied in the UI
# mod_analysis_ui("analysis_1")
    
## To be copied in the server
# mod_analysis_server("analysis_1")
