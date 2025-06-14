#' helpers 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd


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



card_spectro <- function(ns, id, num) {
  ns_id <- function(x) ns(paste0(id, "-", x))
  
  bslib::card(
    div(
      style = "border: 2px solid black; padding: 10px; border-radius: 5px; margin-bottom: 15px; background-color: #FFFAFA;",
      
      h4(paste("Spectrogram", num)),
      
      # Row 1: Event + Species
      fluidRow(
        column(6, selectInput(ns_id("event"), "1.) Event", choices = NULL)),
        column(6, selectInput(ns_id("species"), "2.) Species", choices = NULL))
      ),
      
      # Row 2: Detection + File
      fluidRow(
        column(6, selectInput(ns_id("detection"), "3.) Detection ID", choices = NULL)),
        column(6, selectInput(ns_id("file"), "4.) File", choices = NULL))
      ),
      
      # Row 3: Time Range + Audio playback
      fluidRow(
        column(5, sliderInput(ns_id("time_range"), "Time (s)", min = 0, max = 40, value = c(0, 5))),
        column(7,
               tags$audio(
                 controls = NA,
                 id = ns_id("audio_player"),
                 src = "",  # dynamically updated in server
                 type = "audio/wav"
               )
        )
      ),
      
      # Spectrogram Plot (with spinner)
      shinycssloaders::withSpinner(
        plotOutput(ns_id("spectrogram"), height = "250px"),
        type = 4,
        color = "#00688B",
        caption = "Loading Spectrogram..."
      )
    )
  )
}


#' Process Acoustic Data
#' 
#' @description Process the acoustic data from a specific PAMPal rds file.
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

        first_utc <- if ("UTC" %in% colnames(detector_data)) {
          format(as.POSIXct(detector_data$UTC[1], tz = "UTC"), "%Y-%m-%d %H:%M")
        } else {
          NA
        }

        # Create a data frame entry for this detector
        event_list[[length(event_list) + 1]] <- data.frame(
          Event = event_name,
          Detector = detector_name,
          Species = as.character(species),
          Time = first_utc,
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


#' Pull Events for Effort/Detection Figure
#' 
#' @description This code pulls all events for a specific location RDS file
#'
#' @examples
#' example <- pull_events(location=input$location)
pull_events <- function(location, base_path) {
  rds_path <- file.path(base_path, "RDS", paste0(location, ".rds"))
  rds <- readRDS(rds_path)
  species_df <- data.frame()
  detectors <- unique(unlist(lapply(rds@events, function(event) names(event@detectors))))
  for (event in rds@events) {
    for (detector in detectors) {
      data <- event[[detector]]
      if (!is.null(data)) {
        df <- tibble(datetime = data$UTC,
                     day = as.Date(data$UTC),
                     hour = hour(data$UTC),
                     minute = minute(data$UTC),
                     species = event@species$id)
        
        unique_minutes <- df %>%
          distinct(day, hour, minute, species)
        
        species_df <- rbind(species_df, unique_minutes)
      }
    }
  }
  species_df <- species_df %>% 
    group_by(day, hour, minute) %>%
    mutate(species = if (n() > 1) "Multiple Species" else species) %>%
    distinct(day, hour, minute, .keep_all = TRUE)
  return(species_df[order(species_df$day, species_df$hour, species_df$minute), ])
}


#' sp annotation
#' 
#' @description This function annotates date/time information for the effort/detection figure
#'
sp_annotations <- function(location, base_path, duty_cycle_min=60) {
  library(lubridate)
  library(stringr)
  library(ggplot2)
  library("ggdist")
  library(tidyr)
  library(tidyverse)
  #**may need to specify full path name here too
  files <- read.csv(file.path(base_path, "Audio", paste0(location, "_file_list.csv")))
  parsed <- parse_date_time(str_remove(files$Filename, "\\.wav$"), orders = "ymd-HMS")
  day_hour_present <- data.frame(day = as.Date(parsed), hour = hour(parsed))
  full_grid <- expand_grid(
    day = unique(day_hour_present$day),
    hour = 0:23, minute = 0:59)
  
  species_df <- pull_events(location, base_path)
  
  annotated <- full_grid %>%
    left_join(day_hour_present %>% 
                mutate(effort = TRUE), by = c("day", "hour")) %>%
    mutate(effort = replace_na(effort, FALSE)) %>%
    left_join(species_df, by = c("day", "hour", "minute")) %>%
    mutate(presence = case_when(
      minute < duty_cycle_min & !is.na(species) ~ species,
      minute < duty_cycle_min & effort          ~ "No Events",
      TRUE                         ~ "Not Sampled")) %>%
    mutate(time_str = sprintf("%02d:%02d", hour, minute),
           time_hms = hms::as_hms(sprintf("%02d:%02d:00", hour, minute)),
           day = factor(day, levels = rev(sort(unique(day))))) %>%
    select(day, time_str, time_hms, presence)
  
  spatial <- read.csv(file.path(base_path, "Spatial_Data.csv"))
  spatial$Site <- sub(".*?_", "", spatial$Site)
  
  latitude <- spatial[spatial$Site==location,'Latitude']
  longitude <- spatial[spatial$Site==location,'Longitude']
  tz <- suppressWarnings(
    lutz::tz_lookup_coords(lat = latitude,
                           lon = longitude,
                           method = "fast"))
  
  unique_days <- unique(as.Date(levels(annotated$day)))
  sun_times <- getSunlightTimes(
    date = unique_days,
    lat = latitude,
    lon = longitude,
    tz = tz,
    keep = c("sunrise", "sunset")
  ) %>% mutate(day = as.factor(as.Date(date))) %>%
    select(day, sunrise, sunset)
  
  annotated <- annotated %>%
    left_join(sun_times, by = "day") %>%
    mutate(
      day_date = as.Date(as.character(day)),
      sunrise_hms = hms::as_hms(format(sunrise, "%H:%M:%S")),
      sunset_hms  = hms::as_hms(format(sunset, "%H:%M:%S")),
      
      daylight = case_when(
        (is.na(sunrise) | is.na(sunset)) & lubridate::month(day_date) %in% 4:9 ~
          TRUE, 
        
        sunset_hms > sunrise_hms ~ 
          time_hms >= sunrise_hms & time_hms < sunset_hms,
        
        sunset_hms < sunrise_hms ~ 
          time_hms >= sunrise_hms | time_hms < sunset_hms, 
        
        TRUE ~ NA
      )) %>%
    select(day, time_str, presence, daylight)
  
  return(annotated)
}


#' Effort Plot
#' 
#' @description This function creates the effort/detection plot for the data visualization tab
#'
effort_plot <- function(location, base_path, see_duty_cycle = FALSE, duty_cycle_min = 60) {
  
  col_pal <- c("#4A90A4", "#DB9A8E", "#849F99", "#9D9B90")
  background = alpha('#F2F2F2', 0.25)
  text = '#55636f'
  font = "Roboto"
  
  annotated <- sp_annotations(location, base_path, duty_cycle_min)
  species_colors <- setdiff(unique(annotated$presence), 
                            c("Not Sampled", "No Events"))
  species_palette <- setNames(col_pal[seq_along(species_colors)], species_colors)
  
  hour_labels <- sprintf("%02d:00", 0:23)
  y_breaks <- levels(annotated$day)[seq(1, length(levels(annotated$day)), by = 7)]
  
  if (see_duty_cycle==FALSE) {
    annotated <- annotated[annotated$presence != 'Not Sampled', ]
  }
  
  loc_str <- str_replace(location, "(?<=.)(?=[A-Z])", " ")
  
  p <- ggplot(annotated, aes(x = time_str, y = day)) +
    
    geom_tile(data = subset(annotated, subset = (daylight==FALSE)),
              fill="#2e4482", alpha = 0.25) +
    
    geom_tile(aes(fill = presence)) +
    
    scale_fill_manual(values = c("No Events" = background, 
                                 "Not Sampled" = alpha(text,0.25), 
                                 species_palette),
                      breaks = setdiff(unique(annotated$presence), 
                                       c("No Events", "Not Sampled"))) +
    scale_x_discrete(breaks = hour_labels, position = "top") +
    scale_y_discrete(breaks = y_breaks) +
    
    theme(
      axis.text.x = element_text(angle = 45, hjust = 0, size = 12, family = font, color = text),
      axis.text.y = element_text(size = 12, family = font, color = text),
      
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_blank(),
      
      plot.title = element_text(hjust = 0.5, margin = margin(b=20),
                                color=text, family=font, size=18),
      
      legend.text = element_text(color=text, 
                                 family=font, 
                                 margin = margin(r=15, l=5)),
      legend.title = element_text(color=text, 
                                  family=font),
      legend.position = "bottom",
      legend.direction = "horizontal"
    ) + 
    
    labs(x = NULL, y = NULL, 
         title=paste(loc_str, "Event Detections"),
         fill="")
  
  return(p) 
}


#' Concat Whistes
#' 
#' @description Concat whistles for distribution plot in data visualization
#'
concat_whistles <- function(base_path, location_list) {
  dfs <- list()
  
  for (location in location_list) {
    rds <- readRDS(paste(base_path, "\\RDS\\", location, ".rds", sep=""))
    for (event in names(rds@events)) {
      data <- rds@events[[event]][["Whistle_and_Moan_Detector"]]
      if (!is.null(data) && is.data.frame(data)) {
        data$eventName <- event
        data$eventLabel <- rds@events[[event]]@species$id
        data$location <- location
        dfs <- append(dfs, list(data))
      }
    }
  }
  
  df <- do.call(rbind, dfs)
  return(df)
}


#' Distribution Plot
#' 
#' @description Generates the distribution plot for data visualization
#'
distribution_plot <- function(base_path, location_list, event_list, variable, species_list) {
  
  df <- concat_whistles(base_path, location_list)
  
  if (!identical(event_list, c("All"))) {
    df <- df %>%
      filter(eventName %in% event_list)}
  if (!identical(species_list, c("All"))) {
    df <- df %>%
      filter(eventLabel %in% species_list)}
  
  space_caps <- function(string) {
    gsub("(?<=[a-z])(?=[A-Z])", " ", string, perl = TRUE)
  }
  
  df <- df %>% 
    select(location, eventName, eventLabel, all_of(variable)) %>%
    mutate(eventName = sub(".*\\.", "", eventName)) %>%
    mutate(eventName = factor(eventName, 
                              levels = unique(eventName[order(
                                as.numeric(sub("DGL", "", eventName)))])))
  
  col_pal <- c("#4A90A4", "#DB9A8E", "#2B7A78", "#D6C4A2", "#1C3D57",
               "#7F8B89", "#F4C542")
  
  background = '#F2F2F2'
  text = '#55636f'
  font = "Roboto"
  
  
  p <- ggplot(df, aes(x = eventName, y = .data[[variable]])) +
    geom_violin(aes(fill=eventLabel, color=eventLabel),
                position = position_dodge(width=1),
                width=1, alpha = 0.75,
                linewidth = 0.4, color = text) +
    
    geom_boxplot(aes(group = interaction(eventName, eventLabel)),
                 width = 0.075, position = position_dodge(width = 1),
                 fill = 'white', alpha = 0.75,
                 linewidth = 0.4, color = text) +
    
    facet_wrap(~ location, 
               labeller = labeller(location = space_caps),
               ncol = 1,
               scales = "free_x") + 
    
    labs(x = "", y = var_names[[variable]], fill = "") +
    scale_fill_manual(values = col_pal) +
    
    theme_minimal(base_size=14) +
    theme(
      axis.text.x = element_text(angle=45, hjust=1),
      text = element_text(family = font, color = text),
      strip.text = element_text(hjust = 0, family=font, 
                                color=text, face='bold'),
      
      axis.title.y = element_text(margin = margin(r = 10)),
      
      panel.spacing = unit(1, "lines"),
      
      panel.background = element_rect(fill = background, color = NA),
      plot.background = element_rect(fill = background, color = NA),
      legend.background = element_rect(fill = background, color = NA),
      
      legend.key = element_rect(fill = background, color = NA),
      legend.position = "bottom",
      legend.text = element_text(margin = margin(r=25, l=5))
    )
  
  return(p)
}


#' Spectrogram Card
#' 
#' @description Generates UI cards for spectrogram module
#' 
card_spectro <- function(ns, id, index) {
  tagList(
    div(
      style = "display: flex; flex-direction: row; border: 1px solid #ccc; border-radius: 8px; margin-bottom: 20px; padding: 15px; height: 600px; background-color: #f9f9f9;",
      
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
        actionButton(ns(paste0("render_", index)), "Render Spectrogram", class = "btn btn-primary", style = "width: 300px;")
      ),
      
      # Right panel with spectrogram plot
      div(
        style = "flex: 2; height: 100%;",
        #uiOutput(ns(paste0("audio_", index)), style = "height: 15%;"),
        uiOutput(ns(paste0("plot_ui_", index)), style = "height: 10%;")
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

