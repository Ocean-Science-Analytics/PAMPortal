#' cicerone 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
#' @import cicerone

guide <- cicerone::Cicerone$
  new()$
  step(
    el = "main_1-sidebar",
    title = "Data Uploading",
    description = "This is where you'll input the data you want to be displayed. The 'Data File' input can only read one file at a time, and must be in .json or .rds format. The 'Audio File' input can read multiple files at a time, and must be in .wav format.",
    position = "right-center"
  )$
  step(
    el = "main_1-map_container",
    title = "Map View",
    description = "This map will display the data collection locations.",
    position = "right-center"
  )$
  step(
    el = "nav",
    title = "Tab Selection",
    description = "Use these tabs to switch between the different pages.",
    position = "middle"
  )
