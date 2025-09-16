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
      html = tagList(
        tags$img(src = "www/white_square_OSA_med.jpg", height = "80px"),
        h2("Loading PAMPortal...", style = "color:white; margin-top: 10px;"),
        br(),
        waiter::spin_wave()
      ),
      color = "#3E606F"
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
        }
      "))
    ),
    
    fluidPage(
      theme = bslib::bs_theme(
        version = 5,
        base_font = bslib::font_google("Roboto Condensed"),
        "nav-link-font-size" = "1.25rem !important"
      ),
      
      # Title with image
      div(
        style = "display: flex; align-items: center; padding: 10px; background-color: #7AC5CD;",
        img(src = "www/white_square_OSA_med.jpg", height = 45, style = "margin-right: 10px;"),
        h2("PAMPortal", style = "margin: 0; color: white; flex-grow: 1;"),
        actionButton(inputId = "help", label = "Guide", class = "btn btn-lightgrey")  # Guide button
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
          )
        )
      )
      
      
      # Sidebar + Main Content (using fluidRow now)
      # fluidRow(
      #   column(
      #     width = 3,
      #     style = "padding-top: 10px;",
      #     mod_main_ui("main_1")
      #   ),
      #   column(
      #     width = 9,
      #     div(class = "main-panel",
      #         bslib::navset_card_underline(
      #           id = "nav",
      #           bslib::nav_panel(
      #             title = "SUMMARY",
      #             value = "summary",
      #             mod_overview_ui("overview_1")
      #           ),
      #           bslib::nav_panel(
      #             title = "SPECTROGRAM",
      #             value = "spectro",
      #             mod_spectro_ui("spectro_1")
      #           ),
      #           bslib::nav_panel(
      #             title = "DATA VISULIZATION",
      #             value = "analysis",
      #             mod_analysis_ui("analysis_1")
      #         )
      #       )
      #     )
      #   )
      # )
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
