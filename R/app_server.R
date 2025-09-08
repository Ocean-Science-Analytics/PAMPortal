#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  data <- mod_main_server("main_1") # Create data list to read into other servers
  mod_overview_server("overview_1", data)
  mod_spectro_server("spectro_1", data)
  mod_analysis_server("analysis_1", data)
  mod_soundscape_server("soundscape_1", data)
  
  observeEvent(input$help, {
    guide$init()$start()
  })
  Sys.sleep(5)
  waiter::waiter_hide()
  
  # Inject CSS for modal button hover
  insertUI(
    selector = "body",
    where = "beforeEnd",
    ui = tags$head(
      tags$style(HTML("
        #modal_ok {
          background-color: #001f3f;
          color: white;
          border: 1px solid black;
          padding: 5px 15px;
          transition: background-color 0.3s, color 0.3s, transform 0.2s;
        }
        #modal_ok:hover {
          background-color: #004080;   /* Darker blue */
          color: #ffcc00;               /* Optional text color */
          cursor: pointer;
          transform: scale(1.05);       /* Slightly enlarge button on hover */
        }
      "))
    )
  )
  
  # Show popup modal once app has loaded
  showModal(modalDialog(
    title = "Settings Notice",
    easyClose = TRUE,
    footer = actionButton(
      "modal_ok", 
      "Got it!"
    ),
    size = "m",
    tags$div(
      style = "font-size: 16px; line-height: 1.5;",
      HTML("1.) To be able to utilize the <u>Example Data</u> the application requires that your browser allows popups."),
      tags$b("Please make sure popups are enabled before continuing."), # Bold second line
      tags$br(), tags$br(),
      tags$hr(style = "border: 1px solid black; margin-top: 20px;"),
      tags$div(
        style = "margin-top: 10px;",  # some spacing below the line
        "2.) You can adjust the size of the screen by going to the Settings panel and adjusting the Zoom options."
      ),
      tags$br(),
      # Flex container for side-by-side images
      tags$div(
        style = "display: flex; justify-content: space-around; margin-top: 10px;",
        tags$div(
          style = "flex: 1; text-align: center;",
          tags$img(src = "www/zoom1.png", width = "170px")
        ),
        tags$div(
          style = "flex: 1; text-align: center;",
          tags$img(src = "www/zoom2.png", width = "170px")
        )
      )
    )
  ))
  observeEvent(input$modal_ok, {
    removeModal()
  })
}
