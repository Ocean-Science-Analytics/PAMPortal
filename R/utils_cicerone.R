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
    el = "[data-value='summary']",
    title = "Summary Page",
    description = "The summary page displays the general information for each location dataset.",
    is_id = FALSE,
    position = "middle",
    on_highlighted = "function() { document.querySelector(\"[data-value='summary']\").click(); }"
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
  )$
  step(
    el = "[data-value='spectro']",
    title = "Spectrogram Page",
    description = "The spectrogram page provides two tabs to visualize call events from the different locations.",
    is_id = FALSE,
    position = "middle",
    on_highlighted = "function() { document.querySelector(\"[data-value='spectro']\").click(); }"
  )$
  step(
    el = "spectro_1-spectro_card",
    title = "Spectrogram",
    description = "Adjust these setting to specify which call event to display on in the spectrogram.",
    position = "middle"
  )$
  step(
    el = "[data-value='analysis']",
    title = "Spectrogram Page",
    description = "The data visualization page contains two different analysis methods. The effort/detection plot displays the call events as a function of effort measured from the duty cycle of the hydrophones. The Distribution plot displays the range of different call features for each individual event.",
    is_id = FALSE,
    position = "middle",
    on_highlighted = "function() { document.querySelector(\"[data-value='analysis']\").click(); }"
  )
