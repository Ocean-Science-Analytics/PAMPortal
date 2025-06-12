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
sp_annotations <- function(location, base_path, project_name, duty_cycle_min=60) {
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
#' @description This function creates the effort/detection plot for the data visulization tab
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