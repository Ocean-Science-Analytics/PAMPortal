#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinythemes
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    
    bslib::page_navbar(
      title = span(img(src = "www/white_square_OSA_med.jpg", height = 45)),  #img(src = "www/edia_logo.png", height = 45)),
      theme = bslib::bs_theme(
        version = 5,
        base_font = bslib::font_google("Roboto Condensed"),
        "nav-link-font-size" = "1.25rem !important"
      ),
      bg = "#7AC5CD",
      id = "nav",
      bslib::nav_panel("Overview", mod_main_ui("main_1")),
      bslib::nav_panel("Spectrogram", mod_spectro_ui("spectro_1")),
      #bslib::nav_panel("Data Analysis", mod_main_ui("main_1"))
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
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
