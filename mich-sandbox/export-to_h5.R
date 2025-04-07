library(PAMpal)
library(methods)
library(jsonlite)
library(dplyr)
library(rhdf5)

# Load data
my_data <- readRDS("data/AcousticStudy.rds")
events <- my_data@events

# Create a list to hold detector type tables
detector_tables <- list()


# Make a JSON-ready version of detector tables
json_ready <- lapply(detector_tables, function(df) {
  # Convert each row of the data frame into a named list
  unname(split(df, seq(nrow(df))))
})


# File name
h5_file <- "detectors.h5"

# Remove if it already exists
if (file.exists(h5_file)) file.remove(h5_file)

# Loop through and save each detector table
for (det_name in names(detector_tables)) {
  df <- detector_tables[[det_name]]
  h5write(df, file = h5_file, name = det_name)
}







# Write it out
write(
  toJSON(json_ready, pretty = TRUE, auto_unbox = TRUE),
  "all_detectors.json"
)





# Loop through all events
for (event_name in names(events)) {
  evt <- events[[event_name]]
  
  for (det_name in names(evt@detectors)) {
    det_df <- evt@detectors[[det_name]]
    
    # Add event_name to each row
    det_df$event_name <- event_name
    
    # Store by detector type
    if (!det_name %in% names(detector_tables)) {
      detector_tables[[det_name]] <- det_df
    } else {
      detector_tables[[det_name]] <- bind_rows(detector_tables[[det_name]], det_df)
    }
  }
}

# Optional: check what we got
str(detector_tables, max.level = 1)

# Example: save each table to CSV
for (det_name in names(detector_tables)) {
  filename <- paste0(det_name, "_detections.csv")
  print(det_name)
  write.csv(detector_tables[[det_name]], filename, row.names = FALSE)
}