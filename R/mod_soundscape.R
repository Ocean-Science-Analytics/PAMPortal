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
        #%s:hover {
          background-color: #8DB6CD !important;
          color: white !important;
          border-color: black !important;
          transform: scale(1.05);
          cursor: pointer;
        }
      ", ns("render_plot"))))
    ),
    
    tabsetPanel(
      tabPanel(
        "PSD Gallery",
        uiOutput(ns("site_ui")),
        pixture::pixgalleryOutput(ns("gallery"))
      ),
      tabPanel("SPL Measurements",
              br(),
              div(style = "border: 2px solid black; border-radius: 8px; padding: 15px; margin-bottom: 10px; box-shadow: 0 8px 10px rgba(0,0,0.08,0.4);",
               fluidRow(
                 column(
                   width = 3,
                   selectInput(ns("band_select"),
                               label = "Select Frequency Band",
                               choices = c("50 to 1000 Hz", "<0.8 Hz", "0.8 Hz","1 Hz","1.3 Hz","1.6 Hz","2 Hz","2.5 Hz","3.2 Hz",    
                                            "4 Hz","5 Hz","6.3 Hz","7.9 Hz","10 Hz","12.6 Hz","15.8 Hz","20 Hz","25.1 Hz","31.6 Hz","39.8 Hz",    
                                            "50.1 Hz","63.1 Hz","79.4 Hz","100 Hz","125.9 Hz", "158.5 Hz","199.5 Hz","251.2 Hz","316.2 Hz","398.1 Hz","501.2 Hz",   
                                            "631 Hz","794.3 Hz","1000 Hz","1258.9 Hz","1584.9 Hz","1995.3 Hz","2511.9 Hz","3162.3 Hz","3981.1 Hz","5011.9 Hz","6309.6 Hz",
                                            "7943.3 Hz","10000 Hz","12589.3 Hz", "15848.9 Hz", "19952.6 Hz", "25118.9 Hz", "31622.8 Hz", "39810.7 Hz"),
                               selected = "50 to 1000 Hz",
                               multiple = TRUE
                   )
                 ),
                 column(
                   width = 3,
                   br(),  # adds vertical alignment
                   actionButton(
                     ns("render_plot"),
                     "Render Plot",
                     icon = shiny::icon("file-lines"),
                     style = "background-color: #00688B; color: white; border: none;"
                   )
                 )
               ),
               br(),br(),br(),
               #plotlyOutput(ns("spl_plot"), height = "500px")
               uiOutput(ns("spl_plot_ui"))
              )
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
    
    observeEvent(data$soundscape_data(), {
      sites <- data$soundscape_data()
      if (!is.null(sites) && length(sites) > 0) {
        updateSelectInput(session, "site_select", choices = sites)
      }
    })
    
    ##### Change this back so that the selectInput is back in the UI !!!!!!!!!!!!!!!!!!!!
    output$site_ui <- renderUI({
      req(data$soundscape_data())
      selectInput(ns("site_select"), "Select Site:", choices = data$soundscape_data())
    })
    
    output$gallery <- pixture::renderPixgallery({
      req(input$site_select)
      
      # Construct path to the "PSD_plots" folder for the selected site
      base_path <- data$selected_dir()  # this assumes `selected_dir()` is globally available
      site_folder <- file.path(base_path, "Soundscape", input$site_select, "PSD_Plots")
      req(dir.exists(site_folder))
      
      # Get all .png files in the folder
      png_files <- list.files(site_folder, pattern = "\\.png$", full.names = TRUE)
      req(length(png_files) > 0)
      
      # Create a unique resource alias for this folder (in case multiple sites are selected over time)
      alias <- paste0("psd_", gsub("[^a-zA-Z0-9]", "_", input$site_select))
      shiny::addResourcePath(alias, site_folder)
      
      # Create relative paths for the browser
      relative_paths <- file.path(alias, basename(png_files))
      
      # Render gallery
      pixture::pixgallery(
        relative_paths,
        layout = "grid"
      )
    })
    
    
    ###########################################################################
    # SPL Measurment Logic
    ###########################################################################
    
    observeEvent(input$render_plot, {
      req(data$soundscape_data(), input$band_select)
      showNotification("Loading SPL Plot...", type = "message")
      
      sites <- unique(data$soundscape_data())
      selected_band <- input$band_select
      base_path <- data$selected_dir()
      #browser()
      if ("50 to 1000 Hz" %in% selected_band) {
        bandwidths <- sub(" Hz", "", setdiff(selected_band, "50 to 1000 Hz"))
      } else {
        bandwidths <- sub(" Hz", "", selected_band)
      }
      
      df <- all_data(sites, bandwidths, base_path)
      
      col_pal <- c("#4A90A4", "#DB9A8E", "#849F99", "#9D9B90")
      background = alpha('#F2F2F2', 0.25)
      text = '#55636f'
      font = "Roboto"
      
      # Dynamically create UI outputs for each plot
      output$spl_plot_ui <- renderUI({
        plot_output_list <- lapply(selected_band, function(band) {
          plotlyOutput(ns(paste0("plot_", gsub("\\s|\\.", "_", band))), height = "400px")
        })
        tagList(plot_output_list)
      })
      
      # Render a separate Plotly plot for each band
      for (my_band in selected_band) {
        local({
          band <- my_band
          output[[paste0("plot_", gsub("\\s|\\.", "_", band))]] <- renderPlotly({
            band_df <- df[df$band_type == band, ]
            
            plot_ly(
              data = band_df,
              x = ~month,
              y = ~freq,
              color = ~site,
              colors = col_pal,
              type = "box"
            ) %>%
              layout(
                title = list(text = band, x = 0.1),
                yaxis = list(
                  title = "Sound Pressure Level (dB re 1 µPa)",
                  showline = TRUE,
                  linecolor = "black",
                  linewidth = 1.5
                ),
                xaxis = list(
                  title = "Month",
                  showline = TRUE,
                  linecolor = "black",
                  linewidth = 1.5
                ),
                boxmode = "group",
                legend = list(y = 0.5, yanchor = "middle", font = list(size = 12)),
                font = list(family = font, size = 13),
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
