#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinythemes
#' @import cicerone
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    
    waiter::waiter_show_on_load(
      color = "#001f3f",   # Dark navy (#001f3f) or olive green (#3E606F)
      html  = div(
        style = "
      display: flex;
      flex-direction: column;
      align-items: center;
      justify-content: center;
      height: 100vh;
      gap: 20px;
      font-family: 'Roboto Condensed', sans-serif;
    ",
        
        # Logo
        tags$img(
          src   = "www/white_square_OSA_med.jpg",
          style = "
        height: 100px;
        border-radius: 10px;
        box-shadow: 0 4px 20px rgba(0,0,0,0.4);
        margin-bottom: 10px;
      "
        ),
        
        # App name
        tags$h1(
          "PAMPortal",
          style = "
        color: white;
        font-size: 2.5rem;
        font-weight: bold;
        margin: 0;
        text-shadow: 2px 2px 8px rgba(0,0,0,0.4);
        letter-spacing: 2px;
      "
        ),
        
        # Subtitle
        tags$p(
          "Passive Acoustic Monitoring Data Portal",
          style = "
        color: #8DB6CD;
        font-size: 1rem;
        margin: 0;
        letter-spacing: 1px;
      "
        ),
        
        # Divider
        div(style = "
      width: 200px;
      height: 2px;
      background: linear-gradient(to right, transparent, #00688B, transparent);
      margin: 10px 0;
    "),
        
        # Spinner
        waiter::spin_wave(),
        
        # Loading text
        tags$p(
          "Initializing application...",
          style = "
        color: #aaa;
        font-size: 0.85rem;
        margin-top: 8px;
        letter-spacing: 0.5px;
      "
        ),
        
        # Footer credit
        div(
          style = "
        position: absolute;
        bottom: 30px;
        color: #555;
        font-size: 0.75rem;
        text-align: center;
      ",
          "© Ocean Science Analytics"
        )
      )
    ),
    
    golem_add_external_resources(),
    #add_beta_ribbon(),
    
    # Custom CSS for specific UI settings
    tags$head(
      tags$style(HTML("
        html, body, .container-fluid, .fluid-row {
          height: 100vh; /* Full viewport height */
          margin: 0;
          padding: 0;
        }
        .main-panel {
          display: flex;
          flex-direction: column;
          height: calc(100vh - 65px); /* Adjust for title bar */
        }
        .navset-card-underline {
          flex-grow: 1; /* Fills remaining space */
          display: flex;
          flex-direction: column;
        }
        .tab-content {
          flex-grow: 1;
          overflow-y: auto; /* Enables scrolling if needed */
        }
        .btn-lightgrey {
        background-color: #EEE5DE !important; /* Light grey background */
        color: black; /* Black text color */
        border: 1px solid black; /* Border color to match */
        }
        .btn-lightgrey:hover {
        background-color: #b0b0b0 !important; /* Darker grey on hover */
        transform: scale(1.05);
        }
        .header-logo {
          border-radius: 6px;
          box-shadow: 0 3px 8px rgba(0,0,0,0.3);
        }
        .pam-title {
          text-shadow: 2px 2px 4px rgba(0,0,0,0.35);
        }
        .bslib-sidebar {
        border-right: 1px solid rgba(0,0,0,1) !important;
        border-radius: 0 10px 10px 0;
        }
  
        /* Border + shadow around the top title bar */
        .app-header {
          border-bottom: 1px solid rgba(0,0,0,0.15);
          box-shadow: 0 2px 4px rgba(0,0,0,0.2);
        }
        .nav-link {
          color: #001f3f !important;
          font-weight: 500;
          letter-spacing: 0.04em;
          padding: 8px 16px !important;
          border-radius: 6px !important;
          transition: background-color 0.2s ease, color 0.2s ease;
        }
        
        /* Hover state */
        .nav-link:hover {
          background-color: rgba(0, 104, 139, 0.12) !important;
          color: #00688B !important;
        }
        
        /* Active/selected tab */
        .nav-link.active {
          background-color: #00688B !important;
          color: white !important;
          box-shadow: 0 2px 6px rgba(0, 104, 139, 0.35);
        }
      "))
    ),
    
    fluidPage(
      theme = bslib::bs_theme(
        version = 5,
        base_font = bslib::font_google("Roboto Condensed"),
        "nav-link-font-size" = "1.15rem !important"
      ),
      
      div(
        class = "app-header",
        style = "
          display: flex;
          align-items: center;
          justify-content: space-between;
          padding: 0 20px;
          background-color: #7AC5CD;
          height: 70px;
        ",
        
        # LEFT
        div(
          style = "display: flex; align-items: center; gap: 10px;",
          img(src = "www/white_square_OSA_med.jpg", height = 45,class = "header-logo"),
          h2("PAMPortal", class = "pam-title", style = "margin: 0; line-height: 1; color: white;")
        ),
        
        # CENTER
        div(
          style = "
            flex: 1;
            display: flex;
            justify-content: center;
            align-items: center;
          ",
          img(
            src = "www/header_scene.png",
            style = "height: 65px; width: auto; opacity: 0.9; filter: drop-shadow(0 0 6px rgba(255,255,255,0.6));"
          )
        ),
        
        # RIGHT
        actionButton(
          inputId = "help",
          label = "Guide",
          class = "btn btn-lightgrey"
        )
      ),
      
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = "380px", 
          bg = palette_main()$gray_light, # #A9D3DA
          mod_main_ui("main_1")
        ),
        fillable = TRUE,
        fill = TRUE,
        collapsible = TRUE,
        bslib::navset_card_underline(
          id = "nav",
          bslib::nav_panel(
            title = "SUMMARY",
            value = "summary",
            mod_overview_ui("overview_1")
          ),
          bslib::nav_panel(
            title = "SPECTROGRAM",
            value = "spectro",
            mod_spectro_ui("spectro_1")
          ),
          bslib::nav_panel(
            title = "DATA VISUALIZATION",
            value = "analysis",
            mod_analysis_ui("analysis_1")
          ),
          bslib::nav_panel(
            title = "SOUNDSCAPE",
            value = "soundscape",
            mod_soundscape_ui("soundscape_1")
          ),
          bslib::nav_panel(
            title = "CLICK DETECTOR SCREENSHOTS",
            value = "click_detector",
            mod_click_detector_ui("click_detector_1")
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "PAMPortal"
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    cicerone::use_cicerone(),
    waiter::use_waiter()
  )
}
