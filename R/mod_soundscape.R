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
          background-color: #3399CC !important;
          color: white !important;
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
               fluidRow(
                 column(
                   width = 4,
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
                   width = 4,
                   br(),  # adds vertical alignment
                   actionButton(
                     ns("render_plot"),
                     "Render Plots",
                     style = "background-color: #00688B; color: white; border: none;"
                   )
                 )
               ),
               br(),
               #plotlyOutput(ns("spl_plot"), height = "500px")
               uiOutput(ns("spl_plot_ui"))
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
      site_folder <- file.path(base_path, "Soundscape", input$site_select, "PSD_plots")
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
    
    # Helper: load & format SPL data
    all_data <- function(location_list, bandwidth_list) {
      base_path <- data$selected_dir()
      soundscape_path <- file.path(base_path, "Soundscape")
      folders <- list.dirs(path = soundscape_path, recursive = TRUE)
      band_cols <- paste0("band_", bandwidth_list)
      rows <- list()
      
      for (loc in location_list) {
        matched <- folders[grepl(paste0(loc, ".*SPL_measurements[/\\]csv$"), folders)]
        
        for (file in list.files(matched, full.names = TRUE)) {
          data <- read.csv(file, header = TRUE, check.names = FALSE)
          colnames(data)[2:3] <- c("<0.8", "0.8")
          d <- strsplit(colnames(data)[1], "\\s+")[[1]][1]
          
          cols <- colnames(data)
          cols_numeric <- suppressWarnings(as.numeric(cols))
          filtered <- cols[!is.na(cols_numeric) & cols_numeric >= 50 & cols_numeric <= 1000]
          pressure_sq <- 10^(data[, filtered] / 10)
          pressure_sq_avg <- mean(as.matrix(pressure_sq))
          avg_band <- 10 * log10(pressure_sq_avg)
          
          row <- list(date = d, site = loc, band_50to1000 = avg_band)
          
          for (i in seq_along(bandwidth_list)) {
            bw_raw <- bandwidth_list[i]
            bw_clean <- band_cols[i]
            if (bw_raw %in% colnames(data)) {
              row[[bw_clean]] <- mean(data[[bw_raw]], na.rm = TRUE)
            }
          }
          
          rows[[length(rows) + 1]] <- row
        }
      }
      
      df <- dplyr::bind_rows(lapply(rows, as.data.frame))
      df$date <- as.Date(df$date, format = "%d-%b-%Y")
      df$month <- factor(format(df$date, "%b"), levels = month.abb)
      df$site <- factor(df$site, levels = location_list)  
      df <- tidyr::pivot_longer(df,
                                cols = starts_with("band_"),
                                names_to = "band_type",
                                values_to = "freq"
      )
      
      band_levels <- c("band_50to1000", band_cols)
      band_labels <- c("50 to 1000 Hz", paste0(bandwidth_list, " Hz"))
      df$band_type <- factor(df$band_type, levels = band_levels, labels = band_labels)
      
      return(df)
    }
    
    observeEvent(input$render_plot, {
      req(data$soundscape_data(), input$band_select)
      
      sites <- unique(data$soundscape_data())
      selected_band <- input$band_select
      
      if ("50 to 1000 Hz" %in% selected_band) {
        bandwidths <- sub(" Hz", "", setdiff(selected_band, "50 to 1000 Hz"))
      } else {
        bandwidths <- sub(" Hz", "", selected_band)
      }
      
      df <- all_data(sites, bandwidths)
      
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
    
    # observeEvent(input$render_plot, {
    #   output$spl_plot_ui <- renderUI({
    #     req(input$band_select)
    #     bands <- input$band_select
    #     plot_output_list <- lapply(seq_along(bands), function(i) {
    #       plotlyOutput(ns(paste0("plotly_band_", i)))
    #     })
    #     tagList(plot_output_list)
    #   })
    #   
    #   req(data$soundscape_data(), input$band_select)
    #   sites <- unique(data$soundscape_data())
    #   selected_band <- input$band_select
    #   
    #   bandwidths <- setdiff(selected_band, "50 to 1000 Hz")
    #   bandwidths <- sub(" Hz", "", bandwidths)
    #   
    #   df <- all_data(sites, bandwidths)
    #   filtered_df <- df[df$band_type %in% selected_band, ]
    #   split_data <- split(filtered_df, filtered_df$band_type)
    #   
    #   for (i in seq_along(selected_band)) {
    #     band <- selected_band[i]
    #     local({
    #       my_i <- i
    #       my_band <- band
    #       band_df <- split_data[[my_band]]
    #       band_df$x_group <- interaction(band_df$month, band_df$site, drop = TRUE)
    #       
    #       output[[paste0("plotly_band_", my_i)]] <- plotly::renderPlotly({
    #         p <- ggplot(band_df, aes(x = x_group, y = freq, fill = site)) +
    #           geom_boxplot() +
    #           labs(
    #             title = my_band,
    #             x = "Month",
    #             y = "Sound Pressure Level (dB re 1 µPa)",
    #             fill = "Site"
    #           ) +
    #           scale_fill_manual(values = alpha(col_pal, 0.5)) +
    #           theme_minimal() +
    #           theme(
    #             plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    #             axis.title = element_text(size = 14, face = "bold"),
    #             axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    #             legend.title = element_text(size = 13),
    #             legend.text = element_text(size = 12),
    #             axis.line = element_line(color = "black", linewidth = 0.8),
    #             text = element_text(family = font)
    #           )
    #         ggplotly(p)
    #       })
    #     })
    #   }
    # })
    
    # output$spl_plot <- renderPlot({
    #   req(data$soundscape_data(), input$band_select)
    #   
    #   # Use all unique sites from the soundscape data
    #   sites <- unique(data$soundscape_data())
    #   selected_bands <- input$band_select
    #   
    #   # Parse selected band names
    #   if ("50 to 1000 Hz" %in% selected_bands) {
    #     bandwidths <- sub(" Hz", "", setdiff(selected_bands, "50 to 1000 Hz"))
    #     include_agg_band <- TRUE
    #   } else {
    #     bandwidths <- sub(" Hz", "", selected_bands)
    #     include_agg_band <- FALSE
    #   }
    #   
    #   df <- all_data(sites, bandwidths)
    #   filtered_df <- df[df$band_type %in% selected_bands, ]
    #   
    #   ggplot(filtered_df, aes(x = month, y = freq, fill = site, color = site)) +
    #     geom_boxplot() +
    #     facet_wrap(~ band_type, scales = "free_y", ncol = 1) +
    #     labs(
    #       x = "Month",
    #       y = "Sound Pressure Level (dB re 1 µPa)",
    #       fill = "Site",
    #       color = "Site"
    #     ) +
    #     scale_fill_manual(values = alpha(col_pal, 0.5)) +
    #     scale_color_manual(values = col_pal) +
    #     theme_minimal() +
    #     theme(
    #       strip.text = element_text(hjust = 0, size = 14),
    #       panel.grid.minor = element_blank(),
    #       text = element_text(family = font, size = 14),           # base font size
    #       axis.title = element_text(size = 16, face = "bold"),     # axis titles
    #       axis.text = element_text(size = 13),                     # axis tick labels
    #       legend.title = element_text(size = 14, face = "bold"),   # legend title
    #       legend.text = element_text(size = 13),                   # legend items
    #       axis.line = element_line(color = "black", linewidth = 0.8) # axis lines
    #     )
    # })
  })
}
    
## To be copied in the UI
# mod_soundscape_ui("soundscape_1")
    
## To be copied in the server
# mod_soundscape_server("soundscape_1")
