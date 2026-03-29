#' soundscape UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import pixture
#' @import plotly
mod_soundscape_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(HTML(sprintf("
        #%s {
          background-color: #00688B;
          color: white;
          border: none;
          border-radius: 6px;
          padding: 8px 16px;
          transition: background-color 0.2s ease, transform 0.2s ease;
        }
        #%s:hover {
          background-color: #8DB6CD !important;
          color: white !important;
          transform: scale(1.05);
          cursor: pointer;
        }
      ", ns("render_plot"), ns("render_plot"))))
    ),
    
    tabsetPanel(
      
      # ── PSD Gallery Tab ───────────────────────────────────────────
      tabPanel(
        title = tagList(shiny::icon("images"), " PSD Gallery"),
        br(),
        uiOutput(ns("psd_tab_content"))
      ),
      
      # ── SPL Measurements Tab ──────────────────────────────────────
      tabPanel(
        title = tagList(shiny::icon("chart-bar"), " SPL Measurements"),
        br(),
        uiOutput(ns("spl_tab_content"))
      )
    )
  )
}
    
#' soundscape Server Functions
#'
#' @noRd 
mod_soundscape_server <- function(id, data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    no_data_message <- function(msg) {
      div(
        style = "
          margin-top: 30px;
          padding: 20px;
          border: 1px solid #ddd;
          border-radius: 8px;
          background-color: #f8f8f8;
          text-align: center;
          color: #888;
          font-size: 1.3rem;
        ",
        shiny::icon("circle-info", style = "margin-right: 8px;"),
        msg
      )
    }
    
    has_soundscape <- reactive({
      !is.null(data$soundscape_data()) && length(data$soundscape_data()) > 0
    })
    
    #########################################################################
    # PSD Gallery Tab
    #########################################################################
    output$psd_tab_content <- renderUI({
      if (!has_soundscape()) {
        no_data_message("No PSD images are available for this current dataset.")
      } else {
        div(
          style = "display: flex; flex-direction: column; gap: 12px;",
          
          # Control bar
          div(
            style = "
          display: flex; align-items: flex-end; gap: 16px;
          padding: 14px 18px;
          background-color: #f4f6f8;
          border-radius: 10px;
          border: 1px solid #e0e0e0;
          box-shadow: 0 2px 6px rgba(0,0,0,0.06);
        ",
            div(
              style = "min-width: 220px;",
              tags$label(
                style = "font-size: 0.75rem; font-weight: 600; color: #888;
                     text-transform: uppercase; letter-spacing: 0.05em;
                     display: block; margin-bottom: 4px;",
                shiny::icon("location-dot", style = "margin-right: 4px;"),
                "Select Site"
              ),
              selectInput(ns("site_select"), NULL,
                          choices = data$soundscape_data(),
                          width   = "100%")
            )
          ),
          
          # Gallery card
          div(
            style = "
          background-color: #ffffff;
          border-radius: 10px;
          border: 1px solid #e0e0e0;
          box-shadow: 0 2px 8px rgba(0,0,0,0.05);
          overflow: hidden;
        ",
            div(
              style = "
            padding: 14px 18px;
            border-bottom: 1px solid #e6e6e6;
            background-color: #fafafa;
            display: flex; align-items: center; gap: 10px;
          ",
              shiny::icon("wave-square", style = "color: #00688B; font-size: 1.1em;"),
              tags$span(
                style = "font-weight: 600; font-size: 1rem; color: #001f3f;",
                "Power Spectral Density Images"
              )
            ),
            div(
              style = "padding: 12px;",
              pixture::pixgalleryOutput(ns("gallery"))
            )
          )
        )
      }
    })
    
    output$site_ui <- renderUI({
      req(has_soundscape())
      selectInput(ns("site_select"), "Select Site:", choices = data$soundscape_data())
    })
    
    output$gallery <- pixture::renderPixgallery({
      req(has_soundscape(), input$site_select)
      
      base_path     <- data$selected_dir()
      soundscape_path <- file.path(base_path, "Soundscape")
      folders       <- list.dirs(path = soundscape_path, recursive = TRUE)
      matched       <- folders[grepl(paste0(input$site_select, ".*PSD_[Pp]lots$"), folders)]
      site_folder   <- matched[1]
      req(dir.exists(site_folder))
      
      png_files <- list.files(site_folder, pattern = "\\.png$", full.names = TRUE)
      req(length(png_files) > 0)
      
      alias <- paste0("psd_", gsub("[^a-zA-Z0-9]", "_", input$site_select))
      shiny::addResourcePath(alias, site_folder)
      relative_paths <- file.path(alias, basename(png_files))
      
      pixture::pixgallery(relative_paths, layout = "grid")
    })
    
    #########################################################################
    # SPL Measurements Tab
    #########################################################################
    output$spl_tab_content <- renderUI({
      if (!has_soundscape()) {
        no_data_message("No soundscape measurements are available in this dataset.")
      } else {
        div(
          style = "display: flex; flex-direction: column; gap: 12px;",
          
          # Control bar
          div(
            style = "
          display: flex; align-items: flex-end; gap: 16px;
          padding: 14px 18px;
          background-color: #f4f6f8;
          border-radius: 10px;
          border: 1px solid #e0e0e0;
          box-shadow: 0 2px 6px rgba(0,0,0,0.06);
          flex-wrap: wrap;
        ",
            div(
              style = "flex: 1; min-width: 280px;",
              tags$label(
                style = "font-size: 0.75rem; font-weight: 600; color: #888;
                     text-transform: uppercase; letter-spacing: 0.05em;
                     display: block; margin-bottom: 4px;",
                shiny::icon("wave-square", style = "margin-right: 4px;"),
                "Select Frequency Band(s)"
              ),
              selectInput(
                ns("band_select"), NULL,
                choices = c(
                  "50 to 1000 Hz", "<0.8 Hz", "0.8 Hz", "1 Hz", "1.3 Hz",
                  "1.6 Hz", "2 Hz", "2.5 Hz", "3.2 Hz", "4 Hz", "5 Hz",
                  "6.3 Hz", "7.9 Hz", "10 Hz", "12.6 Hz", "15.8 Hz", "20 Hz",
                  "25.1 Hz", "31.6 Hz", "39.8 Hz", "50.1 Hz", "63.1 Hz",
                  "79.4 Hz", "100 Hz", "125.9 Hz", "158.5 Hz", "199.5 Hz",
                  "251.2 Hz", "316.2 Hz", "398.1 Hz", "501.2 Hz", "631 Hz",
                  "794.3 Hz", "1000 Hz", "1258.9 Hz", "1584.9 Hz", "1995.3 Hz",
                  "2511.9 Hz", "3162.3 Hz", "3981.1 Hz", "5011.9 Hz", "6309.6 Hz",
                  "7943.3 Hz", "10000 Hz", "12589.3 Hz", "15848.9 Hz",
                  "19952.6 Hz", "25118.9 Hz", "31622.8 Hz", "39810.7 Hz"
                ),
                selected = "50 to 1000 Hz",
                multiple = TRUE,
                width    = "100%"
              )
            ),
            
            # Vertical divider
            div(style = "width: 1px; background-color: #ddd; height: 40px; align-self: center;"),
            
            div(
              style = "padding-bottom: 2px;",
              actionButton(
                ns("render_plot"),
                "Render Plot",
                icon = shiny::icon("play")
              )
            )
          ),
          
          # Info banner
          div(
            style = "
          display: flex; align-items: center; gap: 10px;
          padding: 10px 14px;
          background-color: #e8f4fd;
          border-left: 4px solid #00688B;
          border-radius: 6px;
          font-size: 0.85rem;
          color: #555;
        ",
            shiny::icon("circle-info", style = "color: #00688B; flex-shrink: 0;"),
            "Select one or more frequency bands above and click Render Plot. 
         A separate chart will be generated for each band."
          ),
          
          # Plots area
          div(
            style = "
          background-color: #ffffff;
          border-radius: 10px;
          border: 1px solid #e0e0e0;
          box-shadow: 0 2px 8px rgba(0,0,0,0.05);
          padding: 16px;
        ",
            uiOutput(ns("spl_plot_ui"))
          )
        )
      }
    })
    
    ###########################################################################
    # SPL Measurement Logic (unchanged)
    ###########################################################################
    observeEvent(input$render_plot, {
      req(has_soundscape(), input$band_select)
      showNotification("Loading SPL Plot...", type = "message")
      
      sites         <- unique(data$soundscape_data())
      selected_band <- input$band_select
      base_path     <- data$selected_dir()
      
      if ("50 to 1000 Hz" %in% selected_band) {
        bandwidths <- sub(" Hz", "", setdiff(selected_band, "50 to 1000 Hz"))
      } else {
        bandwidths <- sub(" Hz", "", selected_band)
      }
      
      df      <- all_data(sites, bandwidths, base_path)
      col_pal <- c("#4A90A4", "#DB9A8E", "#849F99", "#9D9B90")
      font    <- "Roboto"
      
      output$spl_plot_ui <- renderUI({
        plot_output_list <- lapply(selected_band, function(band) {
          plotlyOutput(ns(paste0("plot_", gsub("\\s|\\.", "_", band))), height = "400px")
        })
        tagList(plot_output_list)
      })
      
      for (my_band in selected_band) {
        local({
          band <- my_band
          output[[paste0("plot_", gsub("\\s|\\.", "_", band))]] <- renderPlotly({
            band_df <- df[df$band_type == band, ]
            plot_ly(
              data   = band_df,
              x      = ~month,
              y      = ~freq,
              color  = ~site,
              colors = col_pal,
              type   = "box"
            ) %>%
              layout(
                title   = list(text = band, x = 0.1),
                yaxis   = list(title = "Sound Pressure Level (dB re 1 µPa)", showline = TRUE, linecolor = "black", linewidth = 1.5),
                xaxis   = list(title = "Month", showline = TRUE, linecolor = "black", linewidth = 1.5),
                boxmode = "group",
                legend  = list(y = 0.5, yanchor = "middle", font = list(size = 12)),
                font    = list(family = font, size = 13),
                colorway = col_pal
              )
          })
        })
      }
    })
  })
}
    
## To be copied in the UI
# mod_soundscape_ui("soundscape_1")
    
## To be copied in the server
# mod_soundscape_server("soundscape_1")
