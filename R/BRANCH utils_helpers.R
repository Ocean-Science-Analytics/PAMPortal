#' helpers 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
#' 
#' @title Add red "beta" ribbon to the bottom left of the UI
#'
#' @description To be used within Shiny UI
#'
#' @noRd
#' @return html
#'
#' @examples
#' htmltools::html_print(add_beta_ribbon())
add_beta_ribbon <- function(){
  
  # inline CSS
  style <- "
    width: 200px;
    background: #b81b11;
    position: fixed;
    bottom: 25px;
    left: -50px;
    text-align: center;
    line-height: 50px;
    letter-spacing: 3px;
    font-size: 1.5em;
    font-weight: 700;
    color: #f0f0f0;
      -webkit-transform: rotate(45deg);
      -moz-transform: rotate(45deg);
    transform: rotate(45deg);
      -webkit-box-shadow: 0 0 15px rgba(0,0,0,0.5);
      -moz-box-shadow: 0 0 15px rgba(0,0,0,0.5);
    box-shadow: 0 0 15px rgba(0,0,0,0.5);
    z-index: 9999;
    "
  
  beta_div <- htmltools::tags$div(
    class = 'corner-ribbon',
    style = style,
    'BETA'
  )
  
  return(beta_div)
}

#' Color palettes
#' 
#' PAMPortal color palette. Primarily used for static UI elements.
#' 
#' @param .data A raster or numeric vector providing the data domain for the color scale.
#'
#' @return For `palette_main()`, a named list of hex colors.  
#' @export
#'
#' @examples
#' palette_main()$green
palette_main <- function(){
  list(
    slate_blue = "#3e606f",# Mainly for loading page 
    deep_cyan = "#00688B", # Mainly for buttons and triggers
    aquamarine = "#7AC5CD", # Mainly for title bar of app
    gray = "#b0b0b0", # Background of buttons and titles
    gray_light = "#F0F0F0", # Background of sidebar
    seashell = "#EEE5DE" # Background of Guide button 
  )
}

#' secondary color palette used for data viz elements.
palette_secondary <- c("#67A9C4", "#15686A", "#CC8266", "#F4C542", 
                       "#8ECFB7", "#A88AC4", "#E69F00", "#9ED6E8",
                       "#D85F54", "#C18C57", "#2E6F9E", "#D7A592")


background = '#F2F2F2'
text = '#55636f'
font = "Roboto"

font_sizes <- c(
  "title" = 20,
  "ticks" = 14,
  "facets" = 16,
  "axis labels" = 18,
  "legend title" = 14,
  "legend text" = 12,
  "text" = 12
)

library(ggplot2)
library(tidyverse)
library(tidyr)
library(dplyr)
library(lubridate)

theme_set(
  theme_minimal(base_size = 12) +
    theme(
      text = element_text(color = text, 
                          size = font_sizes['text']),
      axis.title = element_text(color = text, 
                                size = font_sizes['axis labels']),
      axis.text.x = element_text(color = text, margin = margin(t = 5), 
                                 size = font_sizes['axis labels']),
      axis.text.y = element_text(color = text, margin = margin(r = 5),
                                 size = font_sizes['axis labels']),
      strip.text = element_text(color = text, hjust = 0, 
                                size = font_sizes['facets']), 
      legend.text = element_text(color = text, 
                                 size = font_sizes['legend text']),
      legend.title = element_text(color = text, 
                                  size = font_sizes['legend title']),
      plot.title = element_text(color = text, size = font_sizes['title'], 
                                hjust = 0.5, margin = margin(b=10))
    ))

#' Process Zip File
#' 
#' @description Processes the project zip files for the app
process_zip <- function(zip_path) {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  unzip(zip_path, exdir = temp_dir)
  
  top_level_dirs <- list.dirs(temp_dir, recursive = FALSE, full.names = TRUE)
  
  if (length(top_level_dirs) != 1) {
    stop("Multiple folders found at root of ZIP. Please ensure the ZIP contains a single project folder.")
  }
  
  root_path <- top_level_dirs[[1]]
  
  rds_folder <- file.path(root_path, "RDS")
  acoustic_dir <- file.path(root_path, "Audio")
  soundscape_dir <- file.path(root_path, "Soundscape")
  click_detector_dir <- file.path(root_path, "Click_Detector_Screenshots")
  
  ## ---- RDS LOADING ---- ##
  rds_names <- NULL
  rds_data <- NULL
  if (dir.exists(rds_folder)) {
    rds_paths <- list.files(rds_folder, pattern = "\\.rds$", full.names = TRUE, ignore.case = TRUE)
    if (length(rds_paths) > 0) {
      rds_names <- tools::file_path_sans_ext(basename(rds_paths))
      rds_data <- setNames(lapply(rds_paths, readRDS), rds_names)
    }
  }
  
  ## ---- ACOUSTIC LOADING ---- ##
  acoustic_names <- NULL
  acoustic_tree <- NULL
  
  if (dir.exists(acoustic_dir)) {
    event_paths <- list.dirs(acoustic_dir, recursive = FALSE, full.names = TRUE)
    acoustic_names <- sub("_Clips$", "", basename(event_paths))
    
    build_nested_list <- function(base_dir) {
      event_folders <- list.dirs(base_dir, recursive = FALSE, full.names = TRUE)
      structure_list <- list()
      
      for (event_path in event_folders) {
        event_name <- basename(event_path)
        species_paths <- list.dirs(event_path, recursive = FALSE, full.names = TRUE)
        species_list <- list()
        
        for (species_path in species_paths) {
          species_name <- basename(species_path)
          detection_paths <- list.dirs(species_path, recursive = FALSE, full.names = TRUE)
          detection_list <- list()
          
          for (detection_path in detection_paths) {
            detection_name <- basename(detection_path)
            wavs <- list.files(detection_path, pattern = "\\.wav$", full.names = TRUE)
            if (length(wavs) > 0) {
              detection_list[[detection_name]] <- wavs
            }
          }
          
          if (length(detection_list) > 0) {
            species_list[[species_name]] <- detection_list
          }
        }
        
        if (length(species_list) > 0) {
          structure_list[[event_name]] <- species_list
        }
      }
      
      return(structure_list)
    }
    
    acoustic_tree <- build_nested_list(acoustic_dir)
  }
  
  ## ---- SOUNDSCAPE LOADING ---- ##
  soundscape <- NULL
  if (dir.exists(soundscape_dir)) {
    site_folders <- list.dirs(soundscape_dir, recursive = FALSE, full.names = FALSE)
    if (length(site_folders) > 0) {
      soundscape <- site_folders
    }
  }
  
  ## ---- CLICK DETECTOR SCREENSHOTS LOADING ---- ##
  click_detector <- NULL
  if (dir.exists(click_detector_dir)) {
    site_folders <- list.dirs(click_detector_dir, recursive = FALSE, full.names = TRUE)
    if (length(site_folders) > 0) {
      click_detector <- site_folders
    }
  }
  
  list(
    root_path = root_path,
    rds_names = rds_names,
    rds_data = rds_data,
    acoustic_names = acoustic_names,
    acoustic_tree = acoustic_tree,
    soundscape = soundscape,
    click_detector = click_detector
  )
}


#' Process Acoustic Data
#' 
#' @description Processes the acoustic data from a specific PAMPal rds file.
#'
#' @examples
#' process_acoustic_data(acou_data)

process_acoustic_data <- function(acou_data) {
  # Initialize an empty list to store results
  event_list <- list()
  
  # Loop through each event in acou_data
  for (event_name in names(acou_data@events)) {
    event <- acou_data@events[[event_name]]
    
    # Get species directly from event@species
    species <- event@species
    # Check if the event contains detectors
    if (!is.null(event@detectors)) {
      # Loop through each detector within the event
      for (detector_name in names(event@detectors)) {
        detector_data <- event@detectors[[detector_name]]
        
        if ("UTC" %in% colnames(detector_data)) {
          first_utc <- format(as.POSIXct(detector_data$UTC[1], tz = "UTC"), "%Y-%m-%d %H:%M")
          last_utc  <- format(as.POSIXct(tail(detector_data$UTC, 1), tz = "UTC"), "%Y-%m-%d %H:%M")
        } else {
          first_utc <- NA
          last_utc  <- NA
        }
        
        # Create a data frame entry for this detector
        event_list[[length(event_list) + 1]] <- data.frame(
          Event    = event_name,
          Detector = detector_name,
          Species  = as.character(species),
          Start = first_utc,
          Finish  = last_utc,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  # Combine into a single data frame
  if (length(event_list) > 0) {
    final_df <- dplyr::bind_rows(event_list)
  } else {
    final_df <- data.frame(Event = character(0), Detector = character(0), Species = character(0), Time = character(0))
  }
  return(final_df)
}



#' Get data
#' 
#' @description Retrieves and concatenates all data from the RDS for a 
#' particular time frame (if specified) into a dataframe.
#'
#' @examples
#' get_data(location, base_path, months_of_interest = c(6,7))

get_data <- function(location, base_path, 
                     months_of_interest = c('All')) {
  
  rds_path <- file.path(base_path, "RDS", paste0(location, ".rds"))
  rds <- readRDS(rds_path)
  
  detectors <- unique(unlist(lapply(rds@events, function(event) names(event@detectors))))
  
  species_list <- lapply(rds@events, function(event) {
    dfs <- lapply(detectors, function(detector) {
      data <- event[[detector]]
      if (is.null(data)) return(NULL)
      
      utc <- as.POSIXct(data$UTC, tz = "UTC")
      df <- tibble(
        UTC     = data$UTC,
        species  = event@species$id,
        callType = detector,
        duration = data$duration
      )
    })
    
    dfs <- dfs[!sapply(dfs, is.null)]
    if (length(dfs) > 0) bind_rows(dfs) else NULL
    
  })
  
  species_list <- species_list[!sapply(species_list, is.null)]
  species_df <- bind_rows(species_list)
  
  species_df <- species_df %>%
    mutate(callType = sub("_.*", "", callType),
           duration = if_else(callType == "Click", duration / 1e6, duration))
  
    
  return(species_df)
}

#' Get metadata
#' 
#' @description Retrieves specific metadata variables 
#' (Latitude, Longitude, DC_length, DC_per_hour, Original_TZ) 
#' from csv files in the PAMportal folder.
#'
#' @examples
#' get_metadata(location, base_path, variable = "Latitude")
#' 
get_metadata <- function(location, base_path, variable) {
  csv_path <- paste0(base_path, '\\Metadata.csv')
  df <- read.csv(csv_path)
  print(df[(df$Site==location), variable])
  
  #if (variable %in% c('dc', 'dc_per_hour', 'tz')) {
   # csv_path <- paste0(base_path,'\\Duty_Cycles.csv')
    #df <- read.csv(csv_path)
    #return(df[(df$location==location),variable])
    
  #} else if (variable %in% c('latitude', 'longitude')) {
   # csv_path <- paste0(base_path,'\\Spatial_Data.csv')
    #df <- read.csv(csv_path)
    
    #print(df)
    #return(df[(df$Site==location),variable])
  #}
}


#' Time zone conversion
#' 
#' @description If metadata reflects current time zone in UTC, 
#' converts an input dataframe to local time zone with day/hour/minute columns
#' for future use.
#'
#' @examples
#' get_metadata(location, base_path, variable = "Latitude")
#' 
timezone_conversion <- function(df, latitude, longitude) {
  tz <- suppressWarnings(lutz::tz—l)
}


get_metadata(wake08_2017_location, wake08_base_path, "Latitude")















#' Spectrogram Card
#' 
#' @description Generates UI cards for spectrogram module
#' 
card_spectro <- function(ns, id, index) {
  tagList(
    div(
      style = "display: flex; flex-direction: row; border: 1px solid #ccc; border-radius: 8px;
      margin-bottom: 20px; padding: 15px; height: 700px; background-color: #f9f9f9; box-shadow: 0 8px 10px rgba(0,0,0.08,0.4);",
      
      # Left panel with inputs
      div(
        style = "flex: 1; display: flex; flex-direction: column; gap: 2px; margin-right: 4px;",
        h4(paste("Spectrogram", index)),
        div(style = "border: 2px solid black; border-radius: 6px; padding: 5px; margin-bottom: 10px;",
            selectInput(ns(paste0("location_", index)), "1. Location", choices = NULL),
            selectInput(ns(paste0("species_", index)), "2. Species", choices = NULL),
            selectInput(ns(paste0("folder_", index)), "3. Folder", choices = NULL),
            selectInput(ns(paste0("file_", index)), "4. WAV File", choices = NULL)
        ),
        numericInput(ns(paste0("wl_", index)), "Window Length (wl)", value = 1024, min = 256, step = 256),
        actionButton(ns(paste0("render_", index)), "Render Spectrogram", icon = shiny::icon("file-audio"),
                     class = "custom-btn"
                     #style = "background-color: #00688B; color: white; border: none; width: 300px;"
                     )
      ),
      
      # # Right panel with spectrogram plot
      # div(
      #   style = "flex: 2; height: 100%;",
      #   uiOutput(ns(paste0("audio_", index)), style = "height: 10%;"),
      #   uiOutput(ns(paste0("plot_ui_", index)), style = "height: 90%;"),
      #   
      #   div(
      #     style = "height: 20%; margin-top: 5px; padding: 8px; border-top: 1px solid #ddd;
      #     font-size: 16px; background-color: #fff; min-height: 120px;",
      #     # strong("Event: "),
      #     # uiOutput(ns(paste0("event_name_", index))),   # NEW line
      #     # tags$br(),
      #     strong("Description:"),
      #     uiOutput(ns(paste0("description_", index))),
      #     tags$br(),
      #     strong("Analysis Comments:"),
      #     uiOutput(ns(paste0("analysis_", index)))
      #   )
      # )
      # Right panel with spectrogram plot
      div(
        style = "
          flex: 2; 
          display: flex; 
          flex-direction: column; 
          gap: 10px; 
          height: 100%;
        ",
        
        # Audio player
        uiOutput(ns(paste0("audio_", index))),
        
        # Spectrogram plot — flexible, fills remaining space
        div(
          style = "flex: 1; border: 1px solid #ccc;",
          uiOutput(ns(paste0("plot_ui_", index)))
        ),
        
        # Description / Analysis Comments
        div(
          style = "
            padding: 8px; 
            border-top: 1px solid #ddd; 
            font-size: 16px; 
            background-color: #fff;
            box-shadow: 0 8px 10px rgba(0,0,0.08,0.1);
            min-height: 120px;
          ",
          strong("Description:"),
          uiOutput(ns(paste0("description_", index))),
          tags$br(),
          strong("Analysis Comments:"),
          uiOutput(ns(paste0("analysis_", index)))
        )
      )
    )
  )
}


#' Spectrogram Plot
#' 
#' @description Function for generating a spectrogram plot
#'
spectrogram_plotly <- function(wave,
                               floor = -50,
                               background = "#001f3f",  # very dark blue
                               foreground = "white",
                               hover_bgcolor = "white",
                               hover_fontcolor = "black",
                               overlap = 50,
                               zero_padding = 2,
                               wl = NULL) {
  
  #wl <- round(wave@samp.rate * sqrt(seewave::duration(wave)) * 20e-4)
  if (is.null(wl)) {
    wl <- 1024
  }
  if (wl %% 2 != 0) wl <- wl + 1
  
  spect <- wave |>
    seewave::spectro(
      wl = wl,
      ovlp = overlap,
      zp = zero_padding,
      plot = FALSE
    )
  
  colnames(spect$amp) <- spect$time
  rownames(spect$amp) <- spect$freq
  
  library(tidyr)
  
  spect_df <- spect$amp |>
    as_tibble(rownames = "freq") |>
    pivot_longer(-freq, names_to = "time", values_to = "amp") |>
    mutate(
      freq = as.numeric(freq),
      time = as.numeric(time)
    )
  
  spect_df_floor <- spect_df |>
    mutate(amp_floor = ifelse(amp < floor, floor, amp))
  
  spect_plot <- plot_ly(
    data = spect_df_floor,
    x = ~time,
    y = ~freq,
    z = ~amp_floor,
    type = "heatmap",
    colorscale = "Jet",  # rainbow style
    zmin = floor,
    zmax = max(spect_df$amp),
    colorbar = list(
      title = "Amplitude (dB)",
      titleside = "right",
      tickfont = list(color = foreground),
      titlefont = list(color = foreground)
    ),
    hovertemplate = paste(
      "Time: %{x:.3f} s<br>",
      "Freq: %{y:.1f} kHz<br>",
      "Amp: %{z:.1f} dB<extra></extra>"
    )
  ) |>
    layout(
      xaxis = list(
        title = "Time (s)",
        titlefont = list(size = 14, color = foreground),
        tickfont = list(size = 12, color = foreground),
        tickcolor = foreground,
        linecolor = foreground,
        mirror = TRUE
      ),
      yaxis = list(
        title = "Frequency (kHz)",
        titlefont = list(size = 14, color = foreground),
        tickfont = list(size = 12, color = foreground),
        tickcolor = foreground,
        linecolor = foreground,
        mirror = TRUE
      ),
      # shapes = list(list(
      #   type = "line",
      #   x0 = 0, x1 = 0,
      #   y0 = 0, y1 = max(spect_df$freq, na.rm = TRUE),
      #   line = list(color = "red", width = 2),
      #   name = "playhead",
      #   layer = "above"
      # )),
      paper_bgcolor = background,
      plot_bgcolor = background,
      margin = list(t = 25, r = 25, b = 55, l = 35),
      showlegend = FALSE
    ) |>
    style(
      hoverlabel = list(
        bgcolor = hover_bgcolor,
        font = list(color = hover_fontcolor)
      )
    )
  
  return(spect_plot)
}


#' SPL Measurment Plot
#' 
#' @description Function for gathering SPL measurment data
#'
all_data <- function(location_list, bandwidth_list, base_path) {
  #base_path <- data$selected_dir()
  soundscape_path <- file.path(base_path, "Soundscape")
  print(soundscape_path)
  folders <- list.dirs(path = soundscape_path, recursive = TRUE)
  print(folders)
  band_cols <- paste0("band_", bandwidth_list)
  print(band_cols)
  rows <- list()

  for (loc in location_list) {
    #matched <- folders[grepl(paste0(loc, ".*SPL_measurements[/\\]csv$"), folders)]
    matched <- folders[grepl(paste0(loc, ".*SPL_[Mm]easurements[/\\]csv$"), folders)]

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
  str(df)
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

