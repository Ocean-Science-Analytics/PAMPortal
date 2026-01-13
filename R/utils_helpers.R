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

#' secondary color palettes used for data viz elements.
palette_secondary <- c(
  "#248B8E",
  "#D7A592", 
  "#76508A",
  "#E2B556",
  "#85C1B2",
  "#b37157",
  "#4B7B8E",
  "#B6A7D3",
  "#457a3c",
  "#C9E1EB",
  "#DCC8A9",
  "#1C3D57"
)

palette_constant <- c("#4A90A4", "#DB9A8E", "#1C3D57","#D6C4A2", "#7F8B89")


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

library(ggplot2)
library(tidyverse)
library(tidyr)
library(dplyr)
library(lubridate)
library(scales)

#set theme for all plots
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
                                hjust = 0.5, margin = margin(b=10)),
      plot.caption = element_text(size = font_sizes['legend title'],
                                  color = text, face = "italic"))
    )


#list of reference data for environmental variables
enviro_data <- list(
  "Sea Level Anomaly"= list(
    dataset_id = "noaacwBLENDEDsshDaily",
    var = 'sla',
    axis = "Sea level anomaly (m)"
  ),
  "Sea Surface Temperature" = list(
    dataset_id = "noaacrwsstDaily",
    var = 'analysed_sst',
    axis = "Analyzed SST (°C)"
  ),
  "Chlorophyll A" = list(
    dataset_id = "noaacwNPPVIIRSSQchlaDaily",
    var = 'chlor_a',
    axis = "Chlorophyll (mg m-3)"
  ),
  "KD490" = list(
    dataset_id = "noaacwNPPVIIRSSQkd490Daily",
    var = 'kd_490',
    axis = "Diffuse attenuation coefficient (m-1)"
  ),
  "Lunar Cycles" = list(
    dataset_id = "R:Suncalc",
    var = "moon_illum",
    axis = "Moon illumination (%)"
  )
)



#' Process Zip File
#' 
#' @description Processes the project zip files for the app
process_zip <- function(zip_path) {
  # temp_dir <- tempfile()
  # dir.create(temp_dir)
  # unzip(zip_path, exdir = temp_dir)
  # 
  # top_level_dirs <- list.dirs(temp_dir, recursive = FALSE, full.names = TRUE)
  # 
  # if (length(top_level_dirs) != 1) {
  #   stop("Multiple folders found at root of ZIP. Please ensure the ZIP contains a single project folder.")
  # }
  # 
  # root_path <- top_level_dirs[[1]]
  
  # Create a dedicated temporary directory for this upload
  temp_dir <- file.path(tempdir(), paste0("upload_", as.integer(Sys.time())))
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Unzip into this temp folder
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
#' output: df with UTC, species, callType, and duration
get_data <- function(location, base_path, 
                     months_of_interest = c('All'), species_of_interest = c("All")) {
  
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
  
  if (!("All" %in% months_of_interest)) {
    species_df <- species_df %>%
      filter(month(UTC, label = TRUE, abbr = FALSE) %in% months_of_interest)
  }
  
  if (!("All") %in% species_of_interest) {
    species_df <- species_df %>%
      filter(species %in% species_of_interest)
  }
  
  
  return(species_df)
}


#' Get metadata
#' 
#' @description Retrieves specific metadata variables 
#' (Latitude, Longitude, Depth_m, dc, dc_per_hour, tz) 
#' from csv files in the PAMportal folder.
#'
#' @examples
#' get_metadata(location, base_path, variable = "Latitude")
#' output: entry for the site/variable combo in Metadata.csv
get_metadata <- function(location, base_path, variable) {
  csv_path <- file.path(base_path, "Metadata.csv")
  df <- read.csv(csv_path, stringsAsFactors = FALSE)
  return(df[df$Site == location, variable])
}

#' Get time zone
#' 
#' @description Retrieves time zone for the location.
#'
#' @examples
#' get_timezone(location, base_path)
#' ex. output: "Pacific/Wake"
get_timezone <- function(location, base_path) {
  library(lutz)
  
  latitude <- get_metadata(location, base_path, "Latitude")
  longitude <- get_metadata(location, base_path, "Longitude")
  
  tz <- suppressWarnings(lutz::tz_lookup_coords(
    lat = latitude, lon = longitude, method = "fast"
  ))
  
  return(tz)
}


#' Get sound map
#' 
#' @description Retrieves sound map df from csv and filters to months of interest.
#'
#' @examples
#' get_soundmap(location, base_path, c(6,7))
#' 
get_soundmap <- function(location, base_path, months_of_interest = c("All")) {
  
  file_name <- file.path(base_path, paste0(location, "_sound_map.csv"))
  
  sound_map <- read.csv(file_name, stringsAsFactors = FALSE)

  if (!("All" %in% months_of_interest)) {
    sound_map <- sound_map %>%
      filter(month(local_time, label = TRUE, abbr = FALSE) %in% months_of_interest)
  }
  
  return(sound_map)
}




#' Get environmental data
#' 
#' @description Retrieves environmental df from csv and filters to months of interest.
#'
#' @examples
#' get_environmental(location, base_path, c(6,7))
#' 
get_environmental <- function(location, base_path, months_of_interest = c("All")) {
  
  file_name <- file.path(base_path, paste0(location, "_environmental_data.csv"))
  environmental = read.csv(file_name)
  
  if(!("All" %in% months_of_interest)) {
    environmental <- environmental %>%
      filter(month(day, label = TRUE, abbr = FALSE) %in% months_of_interest)
  }
  
  environmental$moon_illum <- 
    environmental$moon_illum * 100
  
  return(environmental)
  
}


#' Set species colors
#' 
#' @description Creates a map of colors so that species are consistently represented across figures.
#'
#' @examples
#' 
#' 
set_colors <- function(location, base_path) {
  set.seed(47)
  
  sp_order <- get_data(location, base_path) %>%
    group_by(species) %>%
    summarise(n = n()) %>%
    arrange(desc(n))
  
  n_sp <- nrow(sp_order)
  n_needed <- n_sp - length(palette_secondary)
  
  if (n_needed <= 0) {
    sp_order$color <- palette_secondary[1:n_sp]
  } else if (n_needed > 0) {
    new_colors <- colorRampPalette(palette_secondary)(n_needed*2)
    new_colors <- new_colors[!new_colors %in% palette_secondary]
    colors <- c(palette_secondary, sample(new_colors)[1:n_needed])
    sp_order$color <- colors
  }
  
  color_map <- setNames(sp_order$color, sp_order$species)
  
  return(color_map)
  
}


#' Time zone conversion
#' 
#' @description If metadata reflects current time zone in UTC, 
#' converts an input dataframe to local time zone with day/hour/minute columns
#' for future use.  Input dataframe MUST have a UTC col.
#'
#' @examples
#' convert_timezone(df, data_tz = "UTC", local_tz = "Pacific/Wake")
#' Output: modified df with local_time, species, callType, duration, day, hour, minute
#' 
convert_timezone <- function(df, data_tz, local_tz) {
  converted <- df
  data_tz <- data_tz
  
  if (data_tz == 'utc') {
    converted$UTC <- with_tz(converted$UTC, tzone = local_tz)
  }
  
  converted <- converted %>%
    mutate(day = as.Date(UTC, tz = local_tz),
           hour = hour(UTC),
           minute = minute(UTC)) %>%
    rename(local_time = UTC)
  
  return(converted)
}


#' Make an expanded grid
#' 
#' @description Takes an input dataframe (must have columns "day" and "species"), a list of species of interest, 
#' and specification of minute resolution (T/F) and returns a dataframe with a row for each
#' day/hour/species/(minute) combination in the given time frame
#'
#' @examples
#' get_grid(df, species_list = c("Fin whale", "Blue whale"), minutes = FALSE))
#' 
get_grid <- function(df, location, base_path,
                     months_of_interest = c("All"), species_of_interest = c("All"),
                     minutes = FALSE) {
  
  sound_map = get_soundmap(location, base_path, months_of_interest) %>%
    mutate(day = as.Date(local_time))
  
  all_days <- seq(min(sound_map$day), max(sound_map$day), by="day")
  all_hours <- seq(0,23)
  all_minutes <- seq(0,60)
  
  if ('All' %in% species_of_interest) {
    species_of_interest = unique(df$species)
  } else {
    species_of_interest = intersect(species_of_interest, unique(df$species))
  }
  
  if (minutes) {
    grid <- expand_grid(
      day = all_days,
      hour = all_hours, 
      minute = all_minutes,
      species = species_of_interest)
  } else {
    grid <- expand_grid(
      day = all_days,
      hour = all_hours,
      species = species_of_interest)
  }
  
  return(grid)
}




#' Return daylight classification
#' 
#' @description Takes an input dataframe (full grid, converted TZ; must have "day" col) and returns daylight T/F for each row.
#'
#' @examples
#' get_daylight(df, local_tz = "Pacific/Wake", location, basepath)
#' 
#' 
get_daylight <- function(df, local_tz,
                         location, base_path, months_of_interest = c("All")) {

  
  lat <- get_metadata(location, base_path, "Latitude")

  sun_times <- get_environmental(location, base_path, months_of_interest) %>%
    select(day, sunrise, sunset) %>%
    mutate(day = as.Date(day),
           sunrise = force_tz(as.POSIXct(sunrise), tzone = local_tz),
           sunset = force_tz(as.POSIXct(sunset), tzone = local_tz))

  
  #merge sunrise/sunset times with original df
  merged <- df %>%
    left_join(sun_times, by = "day", relationship = "many-to-many") %>%
    mutate(datetime = force_tz(as.POSIXct(day) + hours(hour), tzone = local_tz),
           month = lubridate::month(day))
  
  ### IF daylight DFs not showing up as expected, make sure this DF is correct first -
  ### should have columns for sunrise / sunset that make sense

  if ("minute" %in% names(merged)) {
    merged$datetime <- merged$datetime + merged$minute * 60
  }
  
  merged <- merged %>% 
    mutate(daylight = case_when(
      !is.na(sunrise) & !is.na(sunset) ~ 
        datetime >= sunrise & datetime <= sunset,
      
      #northern hemisphere, polar day
      (is.na(sunrise) | is.na(sunset)) & lat > 0 & 
        month %in% 5:8 ~ TRUE,
      #northern hemisphere, polar night
      (is.na(sunrise) | is.na(sunset)) & lat > 0 & 
        month %in% c(10, 11, 12, 1, 2, 3) ~ FALSE,
      
      #southern hemisphere, polar day
      (is.na(sunrise) | is.na(sunset)) & lat < 0 & 
        month %in% c(10, 11, 12, 1, 2, 3) ~ TRUE,
      #southern hemisphere, polar night
      (is.na(sunrise) | is.na(sunset)) & lat < 0 & 
        month %in% 5:8 ~ FALSE
    ))
  
  #select relevant cols to return
  if ("minute" %in% names(merged)) {
    merged <- merged %>% select(species, day, hour, minute, daylight)
  } else {
    merged <- merged %>% select(species, day, hour, daylight)
  }

  
  return(merged)
}


#' Species Title
#' 
#' @description This formats the selected species names into a title structure
#' 
format_species_title <- function(species_vec) {
  n <- length(species_vec)
  
  if (n == 1) {
    return(species_vec)
  } 
  if (n == 2) {
    return(paste(species_vec, collapse = " & "))
  }
  
  # For 3 or more: a, b, & c
  return(
    paste(
      paste(species_vec[-n], collapse = ", "),
      species_vec[n],
      sep = ", & "
    )
  )
}


#' Occurrence plot
#' 
#' @description Plot of number of minutes an animal was detected with option to show total number of 
#' minutes monitored (effort) for species of interest (default all) and months of interest (default all), 
#' with environmental variable of interest (options are in the enviro_data list)
#'
#' @examples
#' plot_occurrence()
#' 
plot_occurrence <- function(location, base_path,
                            months_of_interest = c("All"), species_of_interest = c("All"), 
                            environmental_variable = NA, show_effort = FALSE) {
  
  env_var_choices <- c(
    "None" = "None",
    "Sea Level Anomaly"  = "sla",
    "Sea Surface Temperature" = "analysed_sst",
    "Chlorophyll A"  = "chlor_a",
    "KD490"  = "kd_490",
    "Lunar Cycles" = "moon_illum"
  )
  
  enviro_data <- enviro_data
  
  months_of_interest <- months_of_interest
  
  # --- Check if months_of_interest are sequential ---
  if (!("All" %in% months_of_interest)) {
    
    # Convert month names to numbers
    month_nums <- match(months_of_interest, month.name)
    
    # Check if they are in strictly increasing order
    if (!all(diff(month_nums) == 1)) {
      showNotification("Occurence Plot Stopped", type = "error", duration = 8)
      showNotification("Selected Months must be in Consecutive Order (e.g., Jan–Feb–Mar).", type = "warning", duration = 8)
      stop("Selected Months must be in Consecutive Order (e.g., Jan–Feb–Mar).")
    }
  }
  
  #pull data and prep grid
  df <- get_data(location, base_path, months_of_interest)
  local_tz <- get_timezone(location, base_path)
  data_tz <- get_metadata(location, base_path, "tz")
  df <- convert_timezone(df, data_tz, local_tz)
  grid <- get_grid(df, location, base_path, months_of_interest,
                   species_of_interest, minutes = FALSE) %>%
    distinct(day, species)
  species_list <- species_of_interest
  environmental_variable <- environmental_variable


  #filter for species of interest
  if (!('All' %in% species_list)) {
    df <- df %>% filter(species %in% species_list)
  }
  
  if(nrow(df) == 0) {
    showNotification("Occurence Plot Stopped", type = "error", duration = 8)
    showNotification("No data found for the species/month combination specified.", type = "warning", duration = 8)
    stop("No data found for the species/month combination specified.")
  }
  
  #count number of occurrences in unique day/hour/min/species combinations
  df <- df %>%
    distinct(day, hour, minute, species) %>%
    group_by(day, species) %>%
    summarise(detected = n(), .groups = "drop")
  
  #pull sound map information to calculate daily effort
  sound_map <- get_soundmap(location, base_path, months_of_interest) %>%
    mutate(Status = str_trim(Status),
           day = as.Date(local_time, tz = tz_func$tz)) %>%
    filter(Status %in% c("Start", "Continue")) %>%
    group_by(day) %>%
    summarise(monitored = n())
  
  color_levels = c(detected = "Detected", undetected = "Undetected")
  
  #join with grid
  merged <- grid %>%
    left_join(df, by = c("species", "day")) %>%
    mutate(detected = replace_na(detected, 0)) %>%
    left_join(sound_map, by="day") %>%
    mutate(undetected = monitored - detected) %>%
    select(-monitored) %>%
    pivot_longer(c(detected, undetected),
                 names_to = "segment", values_to = "minutes") %>%
    mutate(fill_color = factor(color_levels[segment],
                               levels = c("Undetected", "Detected")))
  
  title = paste(str_replace(location, "_", " "), "Daily Detections")
  
  if(show_effort) {
    p <- ggplot(merged, aes(x = day, y = minutes, fill = fill_color)) +
                  geom_bar(stat = "identity") +
                  scale_fill_manual(values = c("Undetected" = alpha(palette_constant[4], 0.25),
                                               "Detected" = palette_constant[1])) +
                  labs(y = "Monitored minutes", fill = "") +
      theme(plot.background = element_rect(fill = "white", color = NA),
            panel.background = element_rect(fill = "white", color = NA),
            legend.background = element_rect(fill = "white", color = NA),
            legend.position = "bottom")
  } else {
    merged <- merged %>% filter(segment=='detected')
    p <- ggplot(merged, aes(x = day, y = minutes)) +
      geom_bar(stat = "identity", fill = palette_constant[1]) +
      labs(y = "Detected minutes", fill = "")
  }
  
  p <- p +
    facet_wrap(~species, ncol = 1, scales = "free_y") +
    labs(title = title, x = "") + 
    scale_y_continuous(expand = c(0,0)) +
    scale_x_date(labels = label_date_short(),
                 breaks = scales::breaks_pretty(n = 10),
                 expand = c(0,0)) +
    theme(
      axis.line = element_line(color = "grey30", size = 1),
      panel.border = element_blank()
    )
  
  #add environmental data to graph if specified
  if(!is.null(environmental_variable) &&
      !is.na(environmental_variable) &&
      environmental_variable != "None") {
    
    environmental_df <- get_environmental(location, base_path, months_of_interest)
    
    env_name <- names(env_var_choices)[env_var_choices == environmental_variable]
    environmental_variable <- env_name

    env_var_csv_col <- enviro_data[[environmental_variable]]$var
    env_var_axis_label <- enviro_data[[environmental_variable]]$axis
    env_var_source <- enviro_data[[environmental_variable]]$dataset_id
    env_var_title <- names(env_var_choices)[env_var_choices == env_var_csv_col]

    
    
    if (all(is.na(environmental_df[[env_var_csv_col]]))) {
      msg = paste("No data available for", environmental_variable, "during the selected time period.")
      showNotification(msg, type = "warning", duration = 8)
      stop(msg)
    }
    

    title = paste0(title, "\nwith ", env_var_title)
    # 
    # # If the column doesn't exist, stop with a helpful message
    if (!env_var_csv_col %in% names(environmental_df)) {
      stop(paste("No data found for the selected environmental variable:", env_var_title))
    }

    max_count <- merged %>%
      group_by(day, species) %>%
      summarise(minutes = sum(minutes, na.rm = TRUE), .groups = "drop") %>%
      summarise(max_value = max(minutes, na.rm = TRUE))

    environmental_df$scaled <- environmental_df[[env_var_csv_col]] -
      min(environmental_df[[env_var_csv_col]], na.rm = TRUE)

    scalar = max_count$max_value / max(environmental_df$scaled, na.rm = TRUE)

    environmental_df$scaled <- environmental_df$scaled * scalar

    environmental_df <- environmental_df %>%
        drop_na(scaled) %>%
      mutate(day = as.Date(day))


    p <- p +
      geom_line(data = environmental_df,
                aes(x = day, y = scaled),
                na.rm = TRUE,
                color = alpha(text),
                linewidth = 0.5,
                inherit.aes = FALSE) +
      scale_y_continuous(
        sec.axis = sec_axis(~ . / scalar +
                              min(environmental_df[[env_var_csv_col]], na.rm = TRUE),
                            name = env_var_axis_label),
        expand = c(0,0)
      ) +
      labs(title = title,
           caption = paste0("Environmental data retrieved from ", env_var_source, "."))
  }
  return(p)
}



#' Call count plot
#' 
#' @description Plot the number of calls recorded per day (on a regular or logarithmic scale) along with call type.
#' Option to add environmental data.
#'
#' @examples
#' plot_call_count()
#' 
plot_call_count <- function(location, base_path, 
                            months_of_interest = c("All"), species_of_interest = c("All"), 
                            environmental_variable = NA, log_scale = FALSE) {
  
  env_var_choices <- c(
    "None" = "None",
    "Sea Level Anomaly"  = "sla",
    "Sea Surface Temperature" = "analysed_sst",
    "Chlorophyll A"  = "chlor_a",
    "KD490"  = "kd_490",
    "Lunar Cycles" = "moon_illum"
  )
  
  enviro_data <- enviro_data

  months_of_interest <- months_of_interest
  
  # --- Check if months_of_interest are sequential ---
  if (!("All" %in% months_of_interest)) {
    
    # Convert month names to numbers
    month_nums <- match(months_of_interest, month.name)
    
    # Check if they are in strictly increasing order
    if (!all(diff(month_nums) == 1)) {
      showNotification("Call count Plot Stopped", type = "error", duration = 8)
      showNotification("Selected Months must be in Consecutive Order (e.g., Jan–Feb–Mar).", type = "warning", duration = 8)
      stop("Selected Months must be in Consecutive Order (e.g., Jan–Feb–Mar).")
    }
  }
  
  
  #prep data
  df <- get_data(location, base_path, months_of_interest, species_of_interest)
  local_tz <- get_timezone(location, base_path)
  data_tz <- get_metadata(location, base_path, "tz")
  df <- convert_timezone(df, data_tz, local_tz) %>%
    group_by(callType, day, species) %>%
    summarise(callCount = n(), .groups = 'drop') %>%
    mutate(callType = str_to_title(callType))
  species_list <- species_of_interest
  environmental_variable <- environmental_variable
  
  if(nrow(df) == 0) {
    showNotification("Call Count Plot Stopped", type = "error", duration = 8)
    showNotification("No data found for the species/month combination specified.", type = "warning", duration = 8)
    stop("No data found for the species/month combination specified.")
  }
  
  
  #get start/end effort from soundmap
  sound_df <- get_soundmap(location, base_path, months_of_interest) %>%
    mutate(day = as.Date(local_time, tz = local_tz))
  
  start_day <- min(sound_df$day, na.rm = TRUE)
  end_day <- max(sound_df$day, na.rm = TRUE)
  
  title = paste(str_replace(location, "_", " "), "Daily Calls")
  ylabel = "Number of calls"
  
  if (log_scale) {
    df$callCount <- log10(df$callCount)
    ylabel <- paste0(ylabel, "\n(log10 scale)")
  }
  
  p <- ggplot(df, aes(x = day, y = callCount, fill = callType)) +
    geom_bar(stat = "identity") +
    facet_wrap(~species, ncol = 1, scales = "free_y") +
    scale_x_date(limits = c(start_day, end_day),
                 labels = label_date_short(), expand = c(0,0)) +
    scale_fill_manual(values = palette_constant) +
    labs(title = title, y = ylabel, x = "", fill = "")
  
  if (length(unique(df$callType)) > 1) {
    p <- p + 
      theme(legend.position = "bottom",
            legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
            axis.line = element_line(color = "grey30", size = 1),
            panel.border = element_blank(),
            plot.margin = margin(t = 10, r = 10, b = 20, l = 10),
            plot.background = element_rect(fill = "white", color = NA),
            panel.background = element_rect(fill = "white", color = NA),
            legend.background = element_rect(fill = "white", color = NA)) 
  } else {
    p <- p +
      theme(axis.line = element_line(color = "grey30", size = 1),
            panel.border = element_blank(),
            plot.background = element_rect(fill = "white", color = NA),
            panel.background = element_rect(fill = "white", color = NA),
            legend.background = element_rect(fill = "white", color = NA),
            legend.position = "none")
  }
  
  
  if(!is.null(environmental_variable) &&
     !is.na(environmental_variable) &&
     environmental_variable != "None") {
    environmental_df <- get_environmental(location, base_path, months_of_interest)
    
    env_name <- names(env_var_choices)[env_var_choices == environmental_variable]
    environmental_variable <- env_name
    
    env_var_csv_col <- enviro_data[[environmental_variable]]$var
    env_var_axis_label <- enviro_data[[environmental_variable]]$axis
    title = paste0(title, "\nwith ", environmental_variable)
    env_var_source <- enviro_data[[environmental_variable]]$dataset_id
    
    if (all(is.na(environmental_df[[env_var_csv_col]]))) {
      msg = paste("No data available for", environmental_variable, "during the selected time period.")
      showNotification(msg, type = "warning", duration = 8)
      stop(msg)
    }
    
    max_count <- df %>%
      group_by(day, species) %>%
      summarise(totalCalls = sum(callCount, na.rm = TRUE), .groups = "drop") %>%
      summarise(max_value = max(totalCalls, na.rm = TRUE))
    environmental_df$scaled <- environmental_df[[env_var_csv_col]] - 
      min(environmental_df[[env_var_csv_col]], na.rm = TRUE)
    
    scalar = max_count$max_value / max(environmental_df$scaled, na.rm = TRUE)
    
    environmental_df$scaled <- environmental_df$scaled * scalar
    
    environmental_df <- environmental_df %>%
      drop_na(scaled) %>%
      mutate(day = as.Date(day))
    
    p <- p +
      labs(title = title,
           caption = paste0("Environmental data retrieved from ", env_var_source, ".")) + 
      geom_line(data = environmental_df, 
                aes(x = day, y = scaled),
                na.rm = TRUE,
                color = alpha(text),
                linewidth = 0.5,
                inherit.aes = FALSE) +
      scale_y_continuous(
        name = ylabel,
        sec.axis = sec_axis(~ . / scalar + min(environmental_df[[env_var_csv_col]], na.rm = TRUE), 
                            name = env_var_axis_label), expand = c(0,0)
      )
  }
  return(p)
}


#' Call density plot
#' 
#' @description View the relative call density of each species across time.
#'
#' @examples
#' plot_call_density()
#' 
plot_call_density <- function(location, base_path, 
                              months_of_interest = c("All"), species_of_interest = c("All"), 
                              environmental_variable = NA) {
  
  env_var_choices <- c(
    "None" = "None",
    "Sea Level Anomaly"  = "sla",
    "Sea Surface Temperature" = "analysed_sst",
    "Chlorophyll A"  = "chlor_a",
    "KD490"  = "kd_490",
    "Lunar Cycles" = "moon_illum"
  )
  
  enviro_data <- enviro_data
  
  months_of_interest <- months_of_interest
  species_of_interest <- species_of_interest
  
  # --- Check if months_of_interest are sequential ---
  if (!("All" %in% months_of_interest)) {
    
    # Convert month names to numbers
    month_nums <- match(months_of_interest, month.name)
    
    # Check if they are in strictly increasing order
    if (!all(diff(month_nums) == 1)) {
      showNotification("Call Density Plot Stopped", type = "error", duration = 8)
      showNotification("Selected Months must be in Consecutive Order (e.g., Jan–Feb–Mar).", type = "warning", duration = 8)
      stop("Selected Months must be in Consecutive Order (e.g., Jan–Feb–Mar).")
    }
  }
  
  #retrieve and convert all data
  df <- get_data(location, base_path, months_of_interest, species_of_interest)
  local_tz <- get_timezone(location, base_path)
  data_tz <- get_metadata(location, base_path, "tz")
  df <- convert_timezone(df, data_tz, local_tz)
  species_list <- species_of_interest
  environmental_variable <- environmental_variable
  
  if(nrow(df) == 0) {
    showNotification("Density Plot Stopped", type = "error", duration = 8)
    showNotification("No data found for the species/month combination specified.", type = "warning", duration = 8)
    stop("No data found for the species/month combination specified.")
  }
  
  
  #get all dates for effort
  sound_df <- get_soundmap(location, base_path, months_of_interest) %>%
    mutate(day = as.Date(local_time, tz = local_tz))
  
  start_day <- min(sound_df$day, na.rm = TRUE)
  end_day <- max(sound_df$day, na.rm = TRUE)
  
  title = paste(str_replace(location, "_", " "), "Relative Call Density")
  color_map = set_colors(location, base_path)
  
  p <- ggplot(df, aes(x = day, color = species, fill = species)) + 
    geom_density(alpha = 0.5) +
    labs(title = title, x = "", y = "Normalized Call Density",
         fill = '', color = '') +
    scale_y_continuous(expand = c(0,0)) +
    scale_color_manual(values = color_map) + 
    scale_fill_manual(values = color_map) + 
    scale_x_date(labels = label_date_short(),
                 expand = c(0,0),
                 limits = c(start_day, end_day)) +
    theme(axis.line = element_line(color = "grey30", size = 1),
          panel.border = element_blank(),
          plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA),
          legend.background = element_rect(fill = "white", color = NA),
          legend.position = "bottom")
  
  if(!is.null(environmental_variable) &&
     !is.na(environmental_variable) &&
     environmental_variable != "None") {
    environmental_df <- get_environmental(location, base_path, months_of_interest)
    
    env_name <- names(env_var_choices)[env_var_choices == environmental_variable]
    environmental_variable <- env_name
    
    env_var_csv_col <- enviro_data[[environmental_variable]]$var
    env_var_axis_label <- enviro_data[[environmental_variable]]$axis
    env_var_source <- enviro_data[[environmental_variable]]$dataset_id
    title = paste0(title, "\nwith ", environmental_variable)
    
    if (all(is.na(environmental_df[[env_var_csv_col]]))) {
      msg = paste("No data available for", environmental_variable, "during the selected time period.")
      showNotification(msg, type = "warning", duration = 8)
      stop(msg)
    }
    
    pd <- ggplot_build(p)
    
    environmental_df$scaled <- environmental_df[[env_var_csv_col]] - 
      min(environmental_df[[env_var_csv_col]], na.rm = TRUE)
    
    scalar = max(pd$data[[1]]$y) / max(environmental_df$scaled, na.rm = TRUE)
    
    environmental_df$scaled <- environmental_df$scaled * scalar
    
    environmental_df <- environmental_df %>%
      drop_na(scaled) %>%
      mutate(day = as.Date(day))
    
    p <- p +
      geom_line(data = environmental_df, 
                aes(x = day, y = scaled), 
                color = alpha(text, .75), linewidth = 0.5,
                inherit.aes = FALSE) +
      labs(title = title, 
           caption = paste0("Environmental data retrieved from ", env_var_source, ".")) +  
      scale_y_continuous(
        sec.axis = sec_axis(~ . / scalar + 
                              min(environmental_df[[env_var_csv_col]], na.rm = TRUE), 
                            name = env_var_axis_label),
        expand = c(0,0))
  }
  return(p)
}




#' Hourly presence
#' 
#' @description View hourly presence and acoustic intensity by species.  Specify metric as "Count" or "Duration".
#'
#' @examples
#' plot_hourly_presence()
#' 
plot_hourly_presence<- function(location, base_path, 
                                months_of_interest = c("All"), species_of_interest = c("All"),
                                metric = "Count", log_scale = FALSE) {
  
  months_of_interest <- months_of_interest
  
  # --- Check if months_of_interest are sequential ---
  if (!("All" %in% months_of_interest)) {
    
    # Convert month names to numbers
    month_nums <- match(months_of_interest, month.name)
    
    # Check if they are in strictly increasing order
    if (!all(diff(month_nums) == 1)) {
      showNotification("Diel Presence Plot Stopped", type = "error", duration = 8)
      showNotification("Selected Months must be in Consecutive Order (e.g., Jan–Feb–Mar).", type = "warning", duration = 8)
      stop("Selected Months must be in Consecutive Order (e.g., Jan–Feb–Mar).")
    }
  }
  
  #get data and compile grid
  df <- get_data(location, base_path, months_of_interest, species_of_interest)
  local_tz <- get_timezone(location, base_path)
  data_tz <- get_metadata(location, base_path, "tz")
  df <- convert_timezone(df, data_tz, local_tz)
  grid <- get_grid(df, location, base_path, months_of_interest,
                   species_of_interest, minutes = FALSE)
  full_grid <- get_daylight(grid, local_tz, location, base_path, months_of_interest)
  ### Jared - this full_grid object should have "TRUE" in the daylight column for hours ~8-17
  
  species_list <- species_of_interest
  
  if(nrow(df) == 0) {
    showNotification("Presence Plot Stopped", type = "error", duration = 8)
    showNotification("No data found for the species/month combination specified.", type = "warning", duration = 8)
    stop("No data found for the species/month combination specified.")
  }
  
  
  #group by either count or duration
  if (metric == "Duration") {
    df <- df %>%
      group_by(day, hour, species) %>%
      summarise(metric = sum(duration), .groups = 'drop')
  } else if (metric == "Count") {
    df <- df %>%
      group_by(day, hour, species) %>%
      summarise(metric = n(), .groups = 'drop')
  }
  
  #adjust for log scale if needed
  if (log_scale) {
    df$metric <- log10(df$metric)
  }
  
  merged <- full_grid %>%
    left_join(df, by = c("day", "hour", "species")) %>%
    mutate(time_hour = substr(hms::as_hms(sprintf("%02d:00:00", hour)), 1, 5)) %>%
    select(-hour)
  
  shadow <- merged %>% filter(!daylight)
  merged$time_hour <- factor(merged$time_hour,
                             levels = c(sprintf("%02d:00", 0:23), "24:00"))
  n_species <- length(unique(df$species))
  nbreaks <- case_when(
    n_species == 1         ~ 4,
    n_species == 2         ~ 6,
    n_species %in% 3:4     ~ 8,
    n_species >= 5         ~ 12
  )
  breaks <- sprintf("%02d:00", seq(0, 24, by = nbreaks))
  title <- paste(str_replace(location, "_", " "), "Hourly Acoustic Presence")
  
  p <- ggplot(merged, aes(x = day, y = time_hour, fill = metric)) +
    geom_tile(data = shadow, fill='black', alpha = 0.25) +
    geom_tile() +
    facet_wrap(~species, ncol=1) +
    scale_fill_gradientn(
      colors = c(palette_constant[2], palette_constant[1], palette_constant[3]), 
      na.value = alpha('#F2F2F2', 0.5),
      guide = guide_colorbar(
        title.position = "right", title.hjust = 0.5,
        barheight = unit(6, 'cm')
      )) +
    scale_x_date(labels = label_date_short(),
                 expand = c(0,0)) +
    scale_y_discrete(breaks=breaks, 
                     expand=c(0,1)) +
    labs(title = title, x = "", y = "Time of Day") +
    theme(axis.line = element_line(color = "grey30", size = 1),
          panel.border = element_blank(),
          plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA),
          legend.background = element_rect(fill = "white", color = NA),
          legend.title = element_text(angle = 270))
  
  if (metric == "Duration" && log_scale == TRUE) {
    p <- p +
      labs(fill = "Total Call Duration\n(log10 scale)")
  } else if (metric == "Count" && log_scale == TRUE) {
    p <- p +
      labs(fill = "Total Call Count\n(log10 scale)")
  } else if (metric == "Duration" && log_scale == FALSE) {
    p <- p +
      labs(fill = "Total Call Duration\n(seconds)")
  } else if (metric == "Count" && log_scale == FALSE) {
    p <- p +
      labs(fill = "Total Call Count")
  }
  
  return(p)
  
}



#' Detections by minute
#' 
#' @description Presence/absence of detections by minute.
#'
#' @examples
#' plot_detections_by_minute()
#' 
plot_detections_by_minute <- function(location, base_path, 
                                      months_of_interest = c("All"), species_of_interest = c("All"),
                                      see_duty_cycle = FALSE) {
  
  months_of_interest <- months_of_interest
  
  # --- Check if months_of_interest are sequential ---
  if (!("All" %in% months_of_interest)) {
    
    # Convert month names to numbers
    month_nums <- match(months_of_interest, month.name)
    
    # Check if they are in strictly increasing order
    if (!all(diff(month_nums) == 1)) {
      showNotification("Diel Detection Plot Stopped", type = "error", duration = 8)
      showNotification("Selected Months must be in Consecutive Order (e.g., Jan–Feb–Mar).", type = "warning", duration = 8)
      stop("Selected Months must be in Consecutive Order (e.g., Jan–Feb–Mar).")
    }
  }
  
  #get required metadata
  lat = get_metadata(location, base_path, "Latitude")
  lon = get_metadata(location, base_path, "Longitude")
  dc_length = get_metadata(location, base_path, "dc")
  dc_per_hour = get_metadata(location, base_path, "dc_per_hour")
  
  #get/convert species data
  df <- get_data(location, base_path, months_of_interest, species_of_interest)
  
  if(nrow(df) == 0) {
    showNotification("Detection Plot Stopped", type = "error", duration = 8)
    showNotification("No data found for the species/month combination specified.", type = "warning", duration = 8)
    stop("No data found for the species/month combination specified.")
  }
  
  
  
  local_tz <- get_timezone(location, base_path)
  data_tz <- get_metadata(location, base_path, "tz")
  
  df <- convert_timezone(df, data_tz, local_tz) %>%
    group_by(day, hour, minute) %>%
    summarise(species = if (n_distinct(species) > 1) 
      "Multiple species" else first(species), .groups = "drop")

  months_of_interest = months_of_interest
  grid <- get_grid(df, location, base_path, months_of_interest,
                   species_of_interest = unique(df$species)[1], minutes = TRUE)
  full_grid <- get_daylight(grid, local_tz, location, base_path, months_of_interest) %>%
    select(-species)
  
  species_list <- species_of_interest
  
  #calculate effort data
  starts <- seq(0, by = 60 / dc_per_hour, length.out = dc_per_hour)
  active_minutes <- unlist(lapply(
    starts, function(s) seq(s, s + dc_length - 1)))
  sound_df <- get_soundmap(location, base_path, months_of_interest) %>%
    mutate(local_time = as.POSIXct(local_time, format = "%Y-%m-%d %H:%M", tz = local_tz),
           Status = str_trim(Status),
           day = as.Date(local_time, tz = local_tz),
           hour = hour(local_time),
           minute = minute(local_time),
           effort = TRUE) %>%
    filter(Status %in% c("Start", "Continue")) %>%
    select(day, hour, minute, effort)
  
  #prep full dataframe 
  merged <- full_grid %>%
    left_join(df, by = c("day", "hour", "minute")) %>%
    left_join(sound_df, by = c("day", "hour", "minute")) %>%
    mutate(effort = if_else(!is.na(species) & species != "" & is.na(effort),
                       TRUE, effort)
    )
  df <- merged %>%
    filter(!is.na(hour) & !is.na(minute)) %>%
    mutate(annotation = case_when(
      is.na(effort) & daylight ~ "No effort (day)",
      is.na(effort) & !daylight ~ "No effort (night)",
      is.na(species) & daylight & effort ~ "No detections (day)",
      is.na(species) & !daylight & effort ~ "No detections (night)",
      TRUE ~ as.character(species)),
      time_of_day = hour*60 + minute) %>%
    select(day, hour, minute, time_of_day, annotation)
  
  #define colors for each annotation category
  fixed_colors <- c(
    "Multiple species" = text,
    "No detections (day)" = background,
    "No effort (day)" = alpha(background, 0.25),
    "No detections (night)" = "#D3E0F4",
    "No effort (night)" = alpha("#D3E0F4", 0.5))
  
  color_map <- set_colors(location, base_path)
  color_map <- c(color_map, fixed_colors)
  
  species <- setdiff(unique(df$annotation), names(fixed_colors))
  df$annotation <- factor(df$annotation, 
                          levels = c(species, names(fixed_colors)))
  
  #plot differently if duty cycle not being displayed
  if (see_duty_cycle == FALSE) {
    df <- df %>%
      filter(minute %in% active_minutes) %>%
      arrange(time_of_day) %>%
      mutate(
        y_display = match(time_of_day, sort(unique(time_of_day)))
      )
    
    unique_times <- sort(unique(df$time_of_day))
    hour_breaks <- seq(0, 24 * 60, by = 180)  # every 3 hours in minutes
    break_indices <- match(hour_breaks, unique_times)
    break_indices <- break_indices[!is.na(break_indices)] 
    
    p <- ggplot(df, aes(x = day, y = y_display, fill = annotation)) +
      geom_tile() +
      scale_y_continuous(
        breaks = break_indices,
        labels = sprintf("%02d:00", (unique_times[break_indices] %/% 60)),
        expand = c(0, 0))
  } else {
    p <- ggplot(df, aes(x = day, y = time_of_day)) + 
      geom_tile(aes(fill = annotation)) + 
      scale_y_continuous( 
        breaks = seq(0, 24*60, by = 180), 
        limits = c(0, 24*60), 
        labels = function(x) sprintf("%02d:00", x %/% 60),
        expand = c(0,0))
  }
  
  title <- paste(str_replace(location, "_", " "), "Detections by Minute")
  
  p <- p + 
    labs(title = title, y = "Time of day") + 
    scale_fill_manual(values = color_map) +
    scale_x_date(labels = label_date_short(),
                 expand = c(0,0),
                 breaks = scales::breaks_pretty(n = 10)) +
    labs(title = title, x = "", fill = "") + 
    theme(legend.position = "bottom",
          legend.justification = "center",
          axis.line = element_line(color = "grey30", size = 1),
          panel.border = element_blank(),
          plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA),
          legend.background = element_rect(fill = "white", color = NA),
          panel.grid = element_blank())
    
  return(p)
  
}


#' Measurements
#' 
#' @description Compare whistle/click characteristics between multiple events.  Options for detector type are "Whistle & Moan" or "Click".
#' User can choose any of the variables available for their chosen 
#'
#' @examples
#' plot_measurements(location_list = c("H11S1_2024", "H11S1_2025"), base_path = "C://PAMPortal_CTBTO",
#' detector_type = "Whistle & Moan", variables_of_interest = c("freqMean", "duration"), species = "Fin whale")
#' 
plot_measurements <- function(location_list, base_path,
                              detector_type, variables_of_interest,
                              species, events_of_interest = c("All")) {
  
  # --- Whistle variables ---
  var_names <- c(
    "Beginning Frequency (Hz)" = "freqBeg",
    "Ending Frequency (Hz)" = "freqEnd",
    "Mean Frequency (Hz)" = "freqMean",
    "Frequency Standard Deviation (Hz)" = "freqStdDev",
    "Duration (s)" = "duration",
    "Frequency Slope Mean (Hz)" = "freqSlopeMean",
    "Frequency Slope Ratio (Hz)" = "freqSlopeRatio",
    "Frequency Spread (Hz)" = "freqSpread",
    "Minimum Frequency (Hz)" = "freqMin",
    "Maximum Frequency (Hz)" = "freqMax",
    "Frequency Range (Hz)" = "freqRange",
    "Frequency Median (Hz)" = "freqMedian",
    "Frequency Maximum:Minimum Ratio (Hz)" = "freqMaxMinRatio",
    "Frequency Beginning:End Ratio (Hz)" = "freqBegEndRatio",
    "Step Duration (s)" = "stepDur"
  )
  
  # --- Click variables ---
  var_names2 <- c(
    "Noise Level (dB)" = "noiseLevel",
    "Duration (s)" = "duration",
    "Peak Time (s)" = "peakTime",
    "Peak Amplitude (dB)" = "peak",
    "Second Peak Amplitude (dB)" = "peak2",
    "Third Peak Amplitude (dB)" = "peak3",
    "Trough Amplitude (dB)" = "trough",
    "Second Trough Amplitude (dB)" = "trough2",
    "Peak-to-Peak Amplitude (1–2) (dB)" = "peakToPeak2",
    "Peak-to-Peak Amplitude (1–3) (dB)" = "peakToPeak3",
    "Peak Amplitude Ratio (Peak2:Peak3)" = "peak2ToPeak3",
    "Peak-to-Peak Level (dB)" = "dBPP",
    "Q10 (Quality Factor at -10 dB)" = "Q_10dB",
    "Minimum Frequency (-10 dB) (Hz)" = "fmin_10dB",
    "Maximum Frequency (-10 dB) (Hz)" = "fmax_10dB",
    "Bandwidth (-10 dB) (Hz)" = "BW_10dB",
    "Center Frequency (-10 dB) (kHz)" = "centerkHz_10dB",
    "Q3 (Quality Factor at -3 dB)" = "Q_3dB",
    "Minimum Frequency (-3 dB) (Hz)" = "fmin_3dB",
    "Maximum Frequency (-3 dB) (Hz)" = "fmax_3dB",
    "Bandwidth (-3 dB) (Hz)" = "BW_3dB",
    "Center Frequency (-3 dB) (kHz)" = "centerkHz_3dB"
  )
  
  # --- Choose variable dictionary based on detector ---
  variable_dict <- if (detector_type == "Whistle & Moan") var_names else var_names2
  
  sp_title <- species %>% str_to_title() %>% format_species_title()
  # sp_title <- species %>% str_to_title()
  # sp_title <- format_species_title(sp_title) 

  if (detector_type == "Whistle & Moan") {
    dfs <- list()
    
    for (location in location_list) {
      rds <- readRDS(file.path(base_path, "RDS", paste0(location, ".rds")))
      for (event in names(rds@events)) {
        data <- rds@events[[event]][["Whistle_and_Moan_Detector"]]
        if (!is.null(data) && is.data.frame(data)) {
          data$eventName <- event
          data$species <- rds@events[[event]]@species$id
          data$location <- location
          dfs <- append(dfs, list(data))
        }
      }
    }
    
    if (length(dfs) == 0) {
      stop("No Data found for the chosen Detector: Whistle & Moan")
    }
    
    title <- paste(sp_title, "Call Measurements")
    df <- do.call(rbind, dfs) 
    
  } else if (detector_type == "Click") {
    
    dfs <- list()
    
    for (location in location_list) {
      rds <- readRDS(file.path(base_path, "RDS", paste0(location, ".rds")))
      
      #print(str(rds))
      for (event in names(rds@events)) {
        for (detector in names(rds@events[[event]]@detectors)) {
          if (grepl("Click_Detector", detector)) {
            data <- rds@events[[event]][[detector]]
            if (!is.null(data) && is.data.frame(data)) {
              data$eventName <- event
              data$species <- rds@events[[event]]@species$id
              data$location <- location
              dfs <- append(dfs, list(data))
            }
          }
        }
      }
    }
    
    if (length(dfs) == 0) {
      stop("No Data found for the chosen Detector: Click")
    }
    
    title <- paste(sp_title, "Echolocation Click Measurements")
    df <- do.call(rbind, dfs)
  }
  
  species_choice <- species
  
  # --- Filter to chosen species ---
  df_species <- df %>% filter(species %in% species_choice)
  
  # --- Species with no data for this detector ---
  missing_species <- setdiff(species_choice, unique(df$species))
  # --- Throw warnings if needed ---
  if (length(df_species) == 0) {
    stop(paste0(
      "No ", detector_type, " detector data found for species: ",
      paste(species, collapse = ", ")
    ))
  }
  
  if (length(missing_species) > 0) {
    stop(paste0(
      "No ", detector_type, " detector data found for: ",
      paste(missing_species, collapse = ", ")
    ))
  }
  
  # Use only the filtered species data
  df <- df_species
  
  #df <- df %>% filter(species == species_choice)
  if (!identical(events_of_interest, c("All"))) {
    df <- df %>% filter(eventName %in% events_of_interest)}
  df <- df %>%
    filter(species %in% species) %>%
    select(all_of(variables_of_interest), "eventName") %>%
    mutate(eventId = str_replace(eventName, "^[^_]+_", "")) %>%
    pivot_longer(cols = variables_of_interest, 
                 names_to = "measurement", 
                 values_to = "value") %>%
    mutate(
      measurement = names(variable_dict)[match(measurement, variable_dict)]
    )
  p <- ggplot(df, aes(x = eventId, y = value, fill = measurement)) +
    geom_violin(width=1, alpha = 0.75,
                linewidth = 0.4, color = text) +
    geom_boxplot(width = 0.075,
                 fill = 'white', alpha = 0.75,
                 linewidth = 0.4, color = text) +
    facet_wrap(~measurement, ncol = 1, scales = "free_y") +
    scale_fill_manual(values = palette_constant) +
    labs(x = "", y = "", title = title) +
    theme(axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
          plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA),
          legend.background = element_rect(fill = "white", color = NA),
          legend.position = "none") 
  
  return(p)
  
}



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
        numericInput(ns(paste0("wl_", index)), "Window Length (wl)", value = 1024, min = 128, step = 128),
        sliderInput(ns(paste0("dyn_range_", index)), "Amplitude dynamic range (dB)", min = 20, max = 120, value = 60, step = 5),
        actionButton(ns(paste0("render_", index)), "Render Spectrogram", icon = shiny::icon("file-audio"),
                     class = "custom-btn"
                     )
      ),
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
                               #floor = -50,
                               dyn_range = 60,
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
  
  zmax <- max(spect_df$amp, na.rm = TRUE)
  zmin <- zmax - dyn_range
  
  # spect_df_floor <- spect_df |>
  #   mutate(amp_floor = ifelse(amp < floor, floor, amp))
  
  spect_df_floor <- spect_df |>
    mutate(amp_floor = pmax(amp, zmin))
  
  spect_plot <- plot_ly(
    data = spect_df_floor,
    x = ~time,
    y = ~freq,
    z = ~amp_floor,
    type = "heatmap",
    colorscale = "Jet",  # rainbow style
    # zmin = floor,
    # zmax = max(spect_df$amp),
    zmin = zmin,
    zmax = zmax,
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
  folders <- list.dirs(path = soundscape_path, recursive = TRUE)
  band_cols <- paste0("band_", bandwidth_list)
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

