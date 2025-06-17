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
    description = "This is where you'll input the Zipped folder containing your PAMGuard data. The app does not require anything to be changed within the zipped data folder. All data will be correctly read into the app",
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
  )$
  step(
    el = "overview_1-overview_input",
    title = "Summary Selection",
    description = "Select which data location to analyze. You can also select to compare two different locations.",
    position = "middle"
  )$
  step(
    el = "overview_1-overview_cards",
    title = "Summary Output",
    description = "Species Events displays the list of species and their relevent files. Species Distribution displays the percentage distribution of all species from a specific location.",
    position = "middle"
  )$
  step(
    el = "overview_1-overview_datatable",
    title = "Analysis Data Table",
    description = "Here is a variety of selections for the data table.",
    position = "middle"
  )
