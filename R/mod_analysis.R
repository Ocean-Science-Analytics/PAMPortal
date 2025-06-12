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
mod_analysis_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel(
        "Effort & Detections",
        div(style = "border: 2px solid black; border-radius: 8px; padding: 15px; margin-bottom: 20px;",
          fluidRow(
            column(
              width = 4,
              selectInput(ns("location"), "Select Location", choices = NULL),
              checkboxInput(ns("see_duty"), "Show Full Duty Cycle", value = FALSE),
              actionButton(ns("render_plot"), "Render Plot", class = "btn-primary")
            ),
            column(
              width = 2,
              numericInput(ns("duty"), "Duty Cycle (min)", value = 60, min = 1, step = 1),
              br(),
              actionButton(ns("export_plot"), "Export Plot", class = "btn-success")
            )
          )
        ),
        br(),
        plotOutput(ns("effort_plot"),height = "500px")
      )
    ),
    tabsetPanel(
      tabPanel(
        "Distribution",
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
    
    base_path <- reactive({
      req(data$selected_dir())
    })
    
    observeEvent(data$rds_data(), {
      req(data$rds_data())
      locations <- names(data$rds_data())
      updateSelectInput(session, "location", choices = locations)
    })
    
    observeEvent(input$render_plot, {
      output$effort_plot <- renderPlot({
        req(input$location, input$duty)
        
        effort_plot(
          location = input$location,
          base_path = base_path(),
          see_duty_cycle = input$see_duty,
          duty_cycle_min = input$duty
        )
      })
    })
  })
}
    
## To be copied in the UI
# mod_analysis_ui("analysis_1")
    
## To be copied in the server
# mod_analysis_server("analysis_1")
