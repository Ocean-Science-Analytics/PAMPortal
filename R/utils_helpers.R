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
palette_secondary <- c("#4A90A4", "#DB9A8E", "#198F7D", "#D6C4A2", "#1C3D57",
             "#F4C542", "#E69F00", "#00CED1", "#CC79A7", "#35AD6B", "#724887")


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


#' Pull Events for Effort/Detection Figure
#' 
#' @description This code pulls all events for a specific location RDS file.  Output will be in original time zone (UTC or local)
#'

pull_events <- function(location, base_path) {
  rds_path <- file.path(base_path, "RDS", paste0(location, ".rds"))
  rds <- readRDS(rds_path)
  
  detectors <- unique(unlist(lapply(rds@events, function(event) names(event@detectors))))
  
  # Collect results in a list of data frames
  species_list <- lapply(rds@events, function(event) {
    dfs <- lapply(detectors, function(detector) {
      data <- event[[detector]]
      if (is.null(data)) return(NULL)
      
      utc <- as.POSIXct(data$UTC, tz = "UTC")
      df <- tibble(
        date     = as.Date(utc),
        hour     = lubridate::hour(utc),
        minute   = lubridate::minute(utc),
        species  = event@species$id
      )

      df %>% distinct(date, hour, minute, species)
    })

    dfs <- dfs[!sapply(dfs, is.null)]
    if (length(dfs) > 0) bind_rows(dfs) else NULL
  })
  
  species_list <- species_list[!sapply(species_list, is.null)]
  
  species_df <- bind_rows(species_list) %>%
    group_by(date, hour, minute) %>%
    mutate(species = if (n() > 1) "Multiple Species" else species) %>%
    distinct(date, hour, minute, .keep_all = TRUE) %>%
    arrange(date, hour, minute) %>%
    ungroup() %>%
    mutate(datetime = make_datetime(
      year = year(date), month = month(date),
      day = day(date), hour = hour, min = minute
    )) %>%
    select(datetime, species)
  
  return(species_df)
}

### pull_events("HYDBBA106", "C:\\Users\\mtoll\\OneDrive\\Ocean Science Analytics\\PAMportal\\OSA_OOI_Demo")




#' effort table
#' 
#' @description This function aggregates data on which time periods were analyzed.  Output will be in local time zone.
#' 

effort_table <- function(location, base_path) {
  library(lubridate)
  library(dplyr)
  library(stringr)
  library(lutz)
  
  #pull spatial / time zone data
  spatial <- read.csv(file.path(base_path, "Spatial_Data.csv"))
  spatial$Site <- sub(".*?_", "", spatial$Site)
  
  latitude <- spatial[spatial$Site==location,'Latitude']
  longitude <- spatial[spatial$Site==location,'Longitude']
  
  tz <- suppressWarnings(
    lutz::tz_lookup_coords(lat = latitude,
                           lon = longitude,
                           method = "fast"))
  
  
  # read audio file list
  files <- read.csv(file.path(base_path, "Audio", paste0(location, "_file_list.csv")))
  parsed <- if ("UTC_DateTime" %in% colnames(files)) {
    parse_date_time(files$UTC_DateTime, orders = "ymd-HMS")
  } else {
    parse_date_time(str_remove(files$Filename, "\\.wav$"), orders = "ymd-HMS")
  }
  files_present <- as.POSIXct(parsed)
  
  # read duty cycle info
  char_df <- read.csv(file.path(base_path, "Duty_Cycles.csv"))
  row_info <- char_df[char_df$location == location, ]
  file_length <- row_info$file_length
  time_zone <- row_info$tz
  duty_cycle  <- row_info$dc

  # create full minute grid
  start_day <- min(files_present)
  end_day   <- max(files_present)
  full_grid <- tibble(datetime = seq(start_day, end_day, by = "1 min"))
  
  # create effort blocks vectorized
  start_times <- files_present
  end_times   <- files_present + minutes(file_length - 1)
  
  # vectorized effort check
  full_grid <- full_grid %>%
    mutate(
      effort = sapply(datetime, function(x) any(x >= start_times & x <= end_times)),
      minute = minute(datetime),
      effort = if_else(minute > duty_cycle, FALSE, effort)
    ) %>%
    select(datetime, effort)
  
  #change time zones if in utc
  if (time_zone == "utc") {
    full_grid <- full_grid %>%
      mutate(datetime = with_tz(datetime, tzone = tz))
  }
  
  #pad first/last day to midnight
  all_times <- tibble(datetime = seq(
    floor_date(min(full_grid$datetime), "day"),
    ceiling_date(max(full_grid$datetime), "day") - minutes(1),
    by = "1 min"
  ))
  
  effort <- all_times %>%
    left_join(full_grid, by = "datetime") %>%
    mutate(effort = replace_na(effort, FALSE))
  
  return(effort)
}

## effort_table("HYDBBA106", "C:\\Users\\mtoll\\OneDrive\\Ocean Science Analytics\\PAMportal\\OSA_OOI_Demo")


#' species annotations
#' 
#' @description This function merges event records with effort, changes time zone to local, and pads 
#' 
#' 
sp_annotations <- function(location, base_path) {
  library(suncalc)
  
  #pull spatial / time zone data
  spatial <- read.csv(file.path(base_path, "Spatial_Data.csv"))
  spatial$Site <- sub(".*?_", "", spatial$Site)
  
  latitude <- spatial[spatial$Site==location,'Latitude']
  longitude <- spatial[spatial$Site==location,'Longitude']
  
  tz <- suppressWarnings(
    lutz::tz_lookup_coords(lat = latitude,
                           lon = longitude,
                           method = "fast"))
  
  char_df <- read.csv(file.path(base_path, "Duty_Cycles.csv"))
  row_info <- char_df[char_df$location == location, ]
  time_zone <- row_info$tz
  
  events <- pull_events(location, base_path)
  
  #convert species table to local time if in utc
  if (time_zone == "utc") {
    events <- events %>%
      mutate(datetime = with_tz(datetime, tzone = tz))
  }
  
  #merge events and effort dataframes
  effort <- effort_table(location, base_path)
  
  annotated <- effort %>%
    left_join(events, by = "datetime") %>%
    mutate(
      date = as.Date(datetime, tz = tz),
      hour = hour(datetime), 
      minute = minute(datetime)
    )

  #get sunrise/sunset times
  unique_days <- unique(as.Date(annotated$datetime, tz = tz))
  sun_times <- getSunlightTimes(
    date = unique_days,
    lat = latitude, lon = longitude,
    tz = tz, keep = c("sunrise", "sunset")) %>%
    select(date, sunrise, sunset)
  
  #merge with data table and calculate daylight
  annotated <- annotated %>%
    left_join(sun_times, by = c("date")) %>%
    mutate(
      daylight = case_when(
        !is.na(sunrise) & !is.na(sunset) & datetime >= sunrise & datetime <= sunset ~ TRUE,
        !is.na(sunrise) & !is.na(sunset) ~ FALSE,
        
        ((is.na(sunrise) | is.na(sunset)) & latitude > 0 & lubridate::month(date) %in% 4:9) ~ TRUE,  # polar day - Northern Hemisphere (Apr–Sep)
        ((is.na(sunrise) | is.na(sunset)) & latitude > 0 & lubridate::month(date) %in% c(10:12, 1:3)) ~ FALSE, # polar night - Northern Hemisphere (Oct–Mar)
        ((is.na(sunrise) | is.na(sunset)) & latitude < 0 & lubridate::month(date) %in% c(10:12, 1:2)) ~ TRUE,  # polar day - Southern Hemisphere (Oct–Feb)
        ((is.na(sunrise) | is.na(sunset)) & latitude < 0 & lubridate::month(date) %in% 4:9) ~ FALSE         # polar night - Southern Hemisphere (Apr–Sep)
      ),
      hour = hour(datetime),
      minute = minute(datetime),
      annotation = case_when(
        !effort ~ "No effort",
        is.na(species) & daylight & effort ~ "No detections (day)",
        is.na(species) & !daylight & effort ~ "No detections (night)",
        TRUE ~ as.character(species)
      )
    ) %>%
    select(datetime, date, hour, minute, effort, species, daylight, annotation)
  
  return(annotated)
  
}

### sp_annotations("HYDBBA106", "C:\\Users\\mtoll\\OneDrive\\Ocean Science Analytics\\PAMportal\\OSA_OOI_Demo")


effort_plot <- function(location, base_path, see_duty_cycle = TRUE) {
  library(scales)
  
  df <- sp_annotations(location, base_path) %>%
    mutate(time_of_day = hour * 60 + minute)
  
  #define color palette
  fixed_colors <- c(
    "No effort" = text,
    "No detections (day)" = background,   # light gray
    "No detections (night)" = "#AEC2CF"  # darker gray
  )
  
  species <- setdiff(unique(df$annotation), names(fixed_colors))
  sp_colors <- setNames(palette_secondary[seq_along(species)], species)
  color_map <- c(sp_colors, fixed_colors)
  df$annotation <- factor(df$annotation, levels = c(species, names(fixed_colors)))
  
  #remove duty cycle periods if specified
  if (see_duty_cycle == FALSE) {
    char_df <- read.csv(file.path(base_path, "Duty_Cycles.csv"))
    duty_cycle  <- char_df[char_df$location == location, "dc"]
    df <- df %>%
      filter(minute < duty_cycle)
  }
  
  x_brks <- seq(min(df$date),max(df$date),
              length.out = 10)
  
  p <- ggplot(df, aes(x = date, y = time_of_day)) +
    geom_tile(aes(fill = annotation)) +
    scale_fill_manual(values = color_map) +

    scale_y_continuous(
      breaks = seq(0, 24*60, by = 180),
      limits = c(0, 24*60),
      labels = function(x) sprintf("%02d:00", x %/% 60),
      expand = c(0,0)
    ) +
    
    scale_x_date(breaks = x_brks,
                 labels = label_date_short(),
                 expand = c(0,0)) +
    
    ggtitle(paste0(location, " Effort and All Detections")) +
    ylab("Time of Day") +
    
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      
      axis.line = element_line(color = "black"),
      axis.ticks = element_line(color = "black"),
      
      axis.title.y = element_text(size = font_sizes['axis labels'], color = text),
      axis.title.x = element_blank(),
      axis.text = element_text(size = font_sizes['ticks'], color = text),
      
      plot.title = element_text(size = font_sizes['title'], color = text, hjust = 0.5,
                                margin = margin(b=15)),
      
      legend.title = element_blank(),
      legend.text = element_text(size = font_sizes['legend text'], color = text),
      legend.position = "bottom",
      legend.justification = "center"
      )
  
  return(p)
}
  
### effort_plot("HYDBBA106", "C:\\Users\\mtoll\\OneDrive\\Ocean Science Analytics\\PAMportal\\OSA_OOI_Demo", see_duty_cycle = FALSE)


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
      axis.line = element_line(color = "black"),
      axis.ticks = element_line(color = "black"),
      
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
  library(suncalc)
  library(lutz)
  
  
  char_df <- read.csv(file.path(base_path, "Duty_Cycles.csv"))
  row_info <- char_df[char_df$location == location, ]
  time_zone <- row_info$tz
  
  spatial <- read.csv(file.path(base_path, "Spatial_Data.csv"))
  spatial$Site <- sub(".*?_", "", spatial$Site)
  
  latitude <- spatial[spatial$Site==location,'Latitude']
  longitude <- spatial[spatial$Site==location,'Longitude']
  tz <- suppressWarnings(
    lutz::tz_lookup_coords(lat = latitude,
                           lon = longitude,
                           method = "fast"))
  
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
  
  species_df <- species_df %>%
    mutate(datetime = as.POSIXct(day) + hours(hour)) %>%
    mutate(total_duration = pmin(total_duration, 3600))
  
  if (time_zone == "utc") {
    species_df <- species_df %>%
      mutate(datetime = with_tz(datetime, tzone = tz)) %>%
      mutate(hour = hour(datetime),
             day = as.Date(datetime))
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
    left_join(species_df, by = c("day", "hour", "species"))
    
  
  if (!('All' %in% species_list)) {
    merged <- merged %>%
      filter(species %in% species_list)
  }
  
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
      
      strip.text = element_text(hjust = 0, 
                                family=font, 
                                color=text,
                                size=font_sizes['facets'],
                                face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = font_sizes['ticks']),
      axis.text.y = element_text(size = font_sizes['ticks']),
      axis.title.y = element_text(family=font, color=text, margin=margin(r=10), size = font_sizes['axis labels']),
      axis.line = element_line(color = "black"),
      axis.ticks = element_line(color = "black"),
      
      
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
      style = "display: flex; flex-direction: row; border: 1px solid #ccc; border-radius: 8px; margin-bottom: 20px; padding: 15px; height: 700px; background-color: #f9f9f9;",
      
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
        uiOutput(ns(paste0("plot_ui_", index)), style = "height: 90%;"),
        
        div(
          style = "height: 20%; margin-top: 5px; padding: 8px; border-top: 1px solid #ddd; font-size: 16px; background-color: #fff;",
          # strong("Event: "), 
          # uiOutput(ns(paste0("event_name_", index))),   # NEW line
          # tags$br(),
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

