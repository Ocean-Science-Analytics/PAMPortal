#' Convert an AcousticStudy RDS file to an HDF5 file of detectors
#'
#' This function loads a PAMpal AcousticStudy object from an RDS file,
#' extracts all detector data frames across all events, groups them by detector type,
#' and writes each detector table to a separate dataset in an HDF5 file.
#'
#' @param rds_path Path to the input RDS file containing an AcousticStudy object.
#' @param output_path Path to the output HDF5 file. Defaults to "detections.h5".
#'
#' @return NULL. Writes an HDF5 file to disk.
#'
#' @examples
#' convert_acoustic_study_to_hdf5("data/AcousticStudy.rds")
#' convert_acoustic_study_to_hdf5("study.rds", output_path = "my_detections.h5")
#'
#' @importFrom PAMpal readRDS
#' @importFrom dplyr bind_rows
#' @importFrom rhdf5 h5write
convert_acoustic_study_to_hdf5 <- function(rds_path, output_path = "detections.h5") {
  
  # Validate the input file path
  if (!file.exists(rds_path)) {
    stop("❌ File does not exist: ", rds_path)
  }
  
  # Load the AcousticStudy object from RDS
  my_data <- readRDS(rds_path)
  events <- my_data@events
  
  # Initialize a list to hold combined data frames by detector type
  detector_tables <- list()
  
  # Loop through each event and collect detector data frames
  for (event_name in names(events)) {
    evt <- events[[event_name]]
    
    for (det_name in names(evt@detectors)) {
      det_df <- evt@detectors[[det_name]]
      
      # Add an event identifier to each row
      det_df$event_name <- event_name
      
      # Combine tables across events, grouped by detector type
      if (!det_name %in% names(detector_tables)) {
        detector_tables[[det_name]] <- det_df
      } else {
        detector_tables[[det_name]] <- bind_rows(detector_tables[[det_name]], det_df)
      }
    }
  }
  
  # Remove existing output file, if present
  if (file.exists(output_path)) {
    message("⚠️  Removing existing file: ", output_path)
    file.remove(output_path)
  }
  
  # Write each detector table to a separate group in the HDF5 file
  for (det_name in names(detector_tables)) {
    df <- detector_tables[[det_name]]
    h5write(df, file = output_path, name = det_name)
  }
  
  message("✅ Conversion complete. HDF5 saved to: ", output_path)
}
