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

#' secondary color palette used for data viz elements.
palette_secondary <- c("#4A90A4", "#DB9A8E", "#2B7A78", "#D6C4A2", "#1C3D57",
             "#F4C542", "#E69F00", "#7F8B89", "#00CED1", "#CC79A7")


background = '#F2F2F2'
text = '#55636f'
font = "Roboto"

font_sizes <- c(
  "title" = 20,
  "ticks" = 14,
  "facets" = 16,
  "axis labels" = 18,
  "legend title" = 14,
  "legend text" = 12
)


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
  
  list(
    root_path = root_path,
    rds_names = rds_names,
    rds_data = rds_data,
    acoustic_names = acoustic_names,
    acoustic_tree = acoustic_tree,
    soundscape = soundscape
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
  #parsed <- parse_date_time(str_remove(files$Filename, "\\.wav$"), orders = "ymd-HMS")
  if ("UTC_DateTime" %in% colnames(files)) {
    parsed <- parse_date_time(files$UTC_DateTime, orders = "ymd-HMS")
  } else {
    parsed <- parse_date_time(str_remove(files$Filename, "\\.wav$"), orders = "ymd-HMS")
  }
  day_hour_present <- data.frame(day = as.Date(parsed), hour = hour(parsed))
  full_grid <- expand_grid(
    day = unique(day_hour_present$day),
    hour = 0:23, minute = 0:59)
  
  species_df <- pull_events(location, base_path)
  #browser()
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
  #browser()
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
  
  annotated <- sp_annotations(location, base_path, duty_cycle_min)
  species_colors <- setdiff(unique(annotated$presence), 
                            c("Not Sampled", "No Events"))
  species_palette <- setNames(palette_secondary[seq_along(species_colors)], species_colors)
  
  annotated$presence <- factor(annotated$presence, 
                               levels = c("No Events", "Not Sampled", species_colors,
                                          "Night time", "No effort"))

  hour_labels <- sprintf("%02d:00", 0:23)
  y_breaks <- levels(annotated$day)[seq(1, length(levels(annotated$day)), by = 7)]

  if (see_duty_cycle==FALSE) {
    annotated <- annotated[annotated$presence != 'Not Sampled', ]
  }
  
  p <- ggplot(annotated, aes(x = time_str, y = day)) +
    
    geom_tile(aes(fill = presence)) +
    
    # Background tiles for night (adds legend entry with dummy fill)
    geom_tile(data = subset(annotated, daylight == FALSE),
              aes(fill = "Night"), alpha = 0.4) +
    
    # No-effort tiles (adds legend entry with dummy fill)
    geom_tile(data = subset(annotated, presence == "Not Sampled"),
              aes(fill = "No effort"), alpha = 0.5) +
    
    
    
    scale_fill_manual(
      values = c("No Events" = background, 
                 "Not Sampled" = alpha(text, 0.25),
                 species_palette,
                 "spacer" = NA, 
                 "Night" = "#2e4482",  
                 "No events" = "#9D9B90",
                 "No effort" = "#F2F2F2"),
      breaks = c(species_colors, "Night", "No effort", "No events"),
      labels = c(species_colors, "Night", "No effort", "No events")
    ) +
    
    scale_x_discrete(breaks = hour_labels, position = "top") +
    scale_y_discrete(
      breaks = levels(annotated$day),  # Tick mark at every day
      labels = function(x) ifelse(x %in% y_breaks, x, "")  # Label only every 7th
    )+
    
    theme(
      axis.text.x = element_text(angle = 45, hjust = 0, size = font_sizes['ticks'], family = font, color = text),
      axis.text.y = element_text(family = font, color = text, size = font_sizes['ticks']),
      
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_blank(),
      
      plot.title = element_text(hjust = 0.5, margin = margin(b=20),
                                color=text, family=font, size=font_sizes['title']),
      
      legend.text = element_text(color=text, 
                                 family=font,
                                 size = font_sizes['legend text'],
                                 margin = margin(r=15, l=5)),
      legend.title = element_text(color=text, 
                                  family=font,
                                  size = font_sizes['legend title']),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.justification = "center",
      legend.box = "horizontal",
      legend.box.just = "center",
      legend.margin = margin(t = 10),
      plot.margin = margin(20, 20, 20, 20)
    ) +
    
    guides(
      fill = guide_legend(
        nrow = 2,
        byrow = TRUE,
        override.aes = list(colour = "black", size = 0.2)  # Outline around each swatch
      )
    ) +
    
    labs(x = NULL, y = NULL, 
         title=paste(location, "Event Detections"),
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
    rds <- readRDS(file.path(base_path, "RDS", paste0(location, ".rds"))) # rds <- readRDS(paste(base_path, "\\RDS\\", location, ".rds", sep=""))
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
    scale_fill_manual(values = palette_secondary) +
    
    theme_minimal(base_size=14) +
    theme(
      axis.text.x = element_text(angle=45, hjust=1, size = font_sizes['ticks']),
      text = element_text(family = font, color = text),
      strip.text = element_text(hjust = 0, family=font, 
                                color=text, face='bold', size = font_sizes['facets']),
      
      axis.title.y = element_text(margin = margin(r = 10), size = font_sizes['axis labels']),
      
      panel.spacing = unit(1, "lines"),
      
      panel.background = element_rect(fill = background, color = NA),
      plot.background = element_rect(fill = background, color = NA),
      legend.background = element_rect(fill = background, color = NA),
      
      legend.key = element_rect(fill = background, color = NA),
      legend.position = "bottom",
      legend.text = element_text(margin = margin(r=25, l=5), size = font_sizes['legend title'])
    )
  
  return(p)
}


#' Occurrence plot events
#' 
#' @description Gathers acoustic event data for the Occurrence plot
#' 
occr_events <- function(location, base_path, species_list = c('All')) {
  library(lubridate)
  
  #rds <- readRDS(paste(base_path, "\\RDS\\", location, ".rds", sep=""))
  rds <- readRDS(file.path(base_path, "RDS", paste0(location, ".rds")))
  species_df <- data.frame()
  detectors <- unique(unlist(lapply(rds@events, 
                                    function(event) names(event@detectors))))
  for (event in rds@events) {
    for (detector in detectors) {
      data <- event[[detector]]
      if (!is.null(data)) {
        df <- tibble(day = as.Date(data$UTC),
                     hour = hour(data$UTC),
                     duration = data$duration,
                     species = event@species$id,
                     detector = detector)
        
        df <- df %>%
          mutate(duration = if_else(detector != "Whistle_and_Moan_Detector", duration / 1000, duration))
        
        event_summary <- df %>%
          group_by(day, hour, species) %>%
          summarise(total_duration = sum(duration), .groups = 'drop')
        
        species_df <- rbind(species_df, event_summary)
      }
    }
  }
  
  all_days <- seq(min(species_df$day), 
                  max(species_df$day), by="day")
  
  all_species <- unique(species_df$species)
  
  all_hours <- seq(0,24)
  
  full_grid <- expand_grid(
    species = all_species,
    day = all_days,
    hour = all_hours
  )
  
  merged <- full_grid %>%
    left_join(species_df, by = c("day", "hour", "species")) %>%
    mutate(datetime = as.POSIXct(day) + hours(hour))
  
  if (!('All' %in% species_list)) {
    merged <- merged %>%
      filter(species %in% species_list)
  }
  
  spatial <- read.csv(file.path(base_path, "Spatial_Data.csv"))
  spatial$Site <- sub(".*?_", "", spatial$Site)
  
  latitude <- spatial[spatial$Site==location,'Latitude']
  longitude <- spatial[spatial$Site==location,'Longitude']
  tz <- suppressWarnings(
    lutz::tz_lookup_coords(lat = latitude,
                           lon = longitude,
                           method = "fast"))
  
  sun_times <- getSunlightTimes(
    date = unique(merged$day),
    lat = latitude,
    lon = longitude,
    tz = tz,
    keep = c("sunrise", "sunset")) %>% 
    mutate(day = as.Date(date)) %>%
    select(day, sunrise, sunset)
  
  if (latitude >= 0) {
    merged <- merged %>%
      left_join(sun_times, by = "day", relationship = "many-to-many") %>%
      mutate(
        #day_date = as.Date(as.character(day)),
        time_hms = hms::as_hms(sprintf("%02d:00:00", hour)),
        sunrise_hms = hms::as_hms(format(sunrise, "%H:%M:%S")),
        sunset_hms  = hms::as_hms(format(sunset, "%H:%M:%S")),
        
        daylight = case_when(
          (is.na(sunrise) | is.na(sunset)) & lubridate::month(day) %in% 4:9 ~
            TRUE,
          
          (is.na(sunrise) | is.na(sunset)) & lubridate::month(day) %in% c(10, 11, 12, 1, 2, 3) ~ FALSE,
          
          sunset_hms > sunrise_hms ~ 
            time_hms >= sunrise_hms & time_hms < sunset_hms,
          sunset_hms < sunrise_hms ~ 
            time_hms >= sunrise_hms | time_hms < sunset_hms
        )) %>%
      mutate(time_hour = substr(time_hms, 1,5))%>%
      select(species, day, time_hour, total_duration, daylight)
  } else {
    merged <- merged %>%
      left_join(sun_times, by = "day", relationship = "many-to-many") %>%
      mutate(
        time_hms = hms::as_hms(sprintf("%02d:00:00", hour)),
        sunrise_hms = hms::as_hms(format(sunrise, "%H:%M:%S")),
        sunset_hms  = hms::as_hms(format(sunset, "%H:%M:%S")),
        
        daylight = case_when(
          (is.na(sunrise) | is.na(sunset)) & lubridate::month(day) %in% 4:9 ~
            FALSE,
          
          (is.na(sunrise) | is.na(sunset)) & lubridate::month(day) %in% c(10, 11, 12, 1, 2, 3) ~ TRUE,
          
          sunset_hms > sunrise_hms ~ 
            time_hms >= sunrise_hms & time_hms < sunset_hms,
          sunset_hms < sunrise_hms ~ 
            time_hms >= sunrise_hms | time_hms < sunset_hms
        )) %>%
      mutate(time_hour = substr(time_hms, 1,5))%>%
      select(species, day, time_hour, total_duration, daylight)
  }

  
  return(merged)
}


#' Occurrence plot
#' 
#' @description Creates the Occurrence plot
#' 
occurrence_plot <- function(location, base_path, species_list = c('All')) {
  library(stringr)
  library(scales)
  df <- occr_events(location, base_path, species_list) %>%
    mutate(hour = as.numeric(substr(time_hour, 1, 2)))
  
  all_days <- seq(min(df$day), max(df$day), by = "1 day")
  week <- seq(min(df$day), max(df$day), by = "7 days")
  shadow <- df %>% filter(!daylight)
  
  breaks <- sprintf("%02d:00", seq(0, 24, by = 6))

  
  p <- ggplot(df, aes(x = day, y = hour, fill = total_duration)) +
    geom_tile(data = shadow, fill='black', alpha = 0.25) +
    geom_tile() +
    facet_wrap(~species, ncol=1) +
    scale_fill_gradientn(
      colors = c(palette_secondary[9], palette_secondary[1], palette_secondary[5]),
      na.value = alpha('#F2F2F2', 0.5),
      name = 'Total Detection Duration\n(seconds)',
      guide = guide_colorbar(
        title.position = "right",   # put title to the left of colorbar
        title.hjust = 0.5,
        barheight = unit(6, 'cm')
      )) +
    
    scale_y_continuous(breaks=breaks_extended(), expand=c(0,0)) +
    scale_x_date(breaks = week,
                 minor_breaks = all_days,
                 date_labels = "%b %d",
                 expand=c(0,0)) +
    
    theme_minimal() + 
    labs(y = 'Hour of Day', x = '', 
         title = location) +
    
    theme(
      plot.background = element_rect(fill = "#F2F2F2", color = NA),  # overall background
      panel.background = element_rect(fill = "#F2F2F2", color = NA),
      legend.position = "right",
      legend.title = element_text(angle = 270, vjust = 0.5,
                                  color=text, family=font, 
                                  size = font_sizes['legend title']),
      legend.text = element_text(color=text, 
                                 family=font, 
                                 margin = margin(r=4), 
                                 size = font_sizes['legend text']),
      
      strip.text = element_text(hjust = 0, family=font, color=text,
                                size=font_sizes['facets']),
      axis.text.x = element_text(angle = 45, hjust = 1, size = font_sizes['ticks']),
      axis.text.y = element_text(size = font_sizes['ticks']),
      axis.title.y = element_text(family=font, color=text, margin=margin(r=10), size = font_sizes['axis labels']),
      
      plot.title = element_text(hjust = 0.5, margin = margin(b=10),
                                color=text, family=font, size=font_sizes['title']),
      
      panel.grid.major.x = element_line(color = "gray60", size = 0.4),
      panel.grid.minor.x = element_line(color = "gray80", size = 0.4),
      panel.grid.major.y = element_line(color = "gray60", size = 0.4),
      panel.grid.minor.y = element_blank()
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
        actionButton(ns(paste0("render_", index)), "Render Spectrogram", icon = shiny::icon("file-audio"),
                     class = "custom-btn"
                     #style = "background-color: #00688B; color: white; border: none; width: 300px;"
                     )
      ),
      
      # Right panel with spectrogram plot
      div(
        style = "flex: 2; height: 100%;",
        uiOutput(ns(paste0("audio_", index)), style = "height: 10%;"),
        uiOutput(ns(paste0("plot_ui_", index)), style = "height: 90%;")
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

