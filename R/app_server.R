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
  tags$style(HTML("
      #modal_ok:hover {
        background-color: #00688B !important;
        color: #ffcc00 !important;
        transform: translateY(-2px);
        transition: all 0.2s ease;
      }
      .modal-confirm-btn {
        transition: all 0.2s ease !important;
      }
      .modal-confirm-btn:hover {
        background-color: lightskyblue !important;
        transform: translateY(-2px) !important;
        box-shadow: 0 4px 8px rgba(0,0,0,0.2) !important;
      }
      /* Cancel/modal dismiss button */
      .btn-default:hover {
        background-color: #e0e0e0 !important;
        transform: translateY(-2px);
        transition: all 0.2s ease;
      }
      
      /* Example data confirm button */
      #main_1-confirm_example_load:hover {
        background-color: #a88a00 !important;
        color: white !important;
        transform: translateY(-2px);
        transition: all 0.2s ease;
      }
    "))
  
  # Show popup modal once app has loaded
  showModal(modalDialog(
    title = div(
      style = "display: flex; align-items: center; gap: 10px;",
      shiny::icon("circle-info", style = "color: #00688B; font-size: 1.3em;"),
      span("Before You Get Started", style = "font-weight: bold; font-size: 1.1em; color: #001f3f;")
    ),
    easyClose = TRUE,
    footer = div(
      style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
      span(
        style = "font-size: 0.82rem; color: #888;",
        shiny::icon("lock", style = "margin-right: 4px;"),
        "This message will not show again once dismissed."
      ),
      actionButton("modal_ok", "Got it!",
                   icon  = shiny::icon("check"),
                   class = "modal-confirm-btn",    
                   style = "background-color: #00688B; color: white; border: none;
                      border-radius: 6px; padding: 8px 20px; font-weight: 500;")
    ),
    size = "m",
    
    tagList(
      # Section 1: Popups
      div(
        style = "display: flex; gap: 12px; align-items: flex-start; 
               padding: 12px; border-radius: 8px; 
               background-color: #fff8e1; border-left: 4px solid #CDAD00;
               margin-bottom: 12px;",
        shiny::icon("triangle-exclamation", 
                    style = "color: #CDAD00; font-size: 1.2em; margin-top: 2px; flex-shrink: 0;"),
        div(
          tags$b("Enable Browser Popups"),
          tags$p(
            style = "margin: 4px 0 0 0; font-size: 0.9rem; color: #555;",
            "To use the example data, your browser must allow popups from this app. 
           Please enable popups before continuing."
          )
        )
      ),
      
      # Section 2: Zoom
      div(
        style = "display: flex; gap: 12px; align-items: flex-start;
               padding: 12px; border-radius: 8px;
               background-color: #e8f4fd; border-left: 4px solid #00688B;
               margin-bottom: 16px;",
        shiny::icon("magnifying-glass", 
                    style = "color: #00688B; font-size: 1.2em; margin-top: 2px; flex-shrink: 0;"),
        div(
          tags$b("Adjust Zoom for Best Experience"),
          tags$p(
            style = "margin: 4px 0 0 0; font-size: 0.9rem; color: #555;",
            "If the layout appears too large or small, adjust your browser zoom 
           level via the Settings menu (typically", tags$kbd("Ctrl"), "+", 
            tags$kbd("-"), "or", tags$kbd("Ctrl"), "+", tags$kbd("+"), ")."
          )
        )
      ),
      
      # Zoom screenshots
      div(
        style = "display: flex; gap: 12px; justify-content: center; 
               padding: 10px; background-color: #f8f8f8; 
               border-radius: 8px; border: 1px solid #ddd;",
        div(
          style = "text-align: center;",
          tags$p(style = "font-size: 0.78rem; color: #888; margin-bottom: 6px;", 
                 "Step 1: Open Settings"),
          tags$img(src = "www/zoom1.png", 
                   style = "width: 170px; border-radius: 6px; 
                          border: 1px solid #ddd; box-shadow: 0 2px 4px rgba(0,0,0,0.1);")
        ),
        div(
          style = "text-align: center;",
          tags$p(style = "font-size: 0.78rem; color: #888; margin-bottom: 6px;", 
                 "Step 2: Adjust Zoom"),
          tags$img(src = "www/zoom2.png", 
                   style = "width: 170px; border-radius: 6px; 
                          border: 1px solid #ddd; box-shadow: 0 2px 4px rgba(0,0,0,0.1);")
        )
      )
    )
  ))
  observeEvent(input$modal_ok, {
    removeModal()
  })
}
