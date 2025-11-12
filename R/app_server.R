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
  mod_click_detector_server("click_detector_1", data)
  
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
          border: none;
          border-radius: 6px;
          padding: 8px 20px;
          font-weight: 500;
          transition: background-color 0.3s, color 0.3s, transform 0.2s;
        }
        #modal_ok:hover {
          background-color: #004080;
          color: #ffcc00;
          transform: translateY(-2px);
        }
        .modal-content {
          border-radius: 12px;
          box-shadow: 0 6px 20px rgba(0,0,0,0.25);
          border: none;  /* Remove harsh border */
        }
        .modal-body {
          font-size: 15px;
          line-height: 1.6;
          color: #333;
        }
        .modal-body li {
          margin-bottom: 12px;
        }
      "))
    )
  )
  
  # Show popup modal once app has loaded
  showModal(modalDialog(
    title = "Settings Notice",
    easyClose = TRUE,
    footer = actionButton("modal_ok", "Got it!"),
    size = "m",
    tags$div(
      class = "modal-body",
      HTML("
      <ol style='padding-left:20px;'>
        <li><b>Example Data</b>: In order to use the example data loaded into this app, the application requires that your browser allows popups. <b>Please make sure popups are enabled before continuing.</b></li>
        <li><b>Zoom Settings</b>: You can adjust the screen size by going to the Settings panel and adjusting the Zoom options.</li>
      </ol>
    "),
      tags$div(
        style = "display: flex; justify-content: space-around; margin-top: 20px;",
        tags$div(style = "flex: 1; text-align: center;", tags$img(src = "www/zoom1.png", width = "170px")),
        tags$div(style = "flex: 1; text-align: center;", tags$img(src = "www/zoom2.png", width = "170px"))
      )
    )
  ))
  observeEvent(input$modal_ok, {
    removeModal()
  })
}
