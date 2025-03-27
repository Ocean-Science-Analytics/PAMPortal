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
    description = "Guide being built...",
    position = "right-center"
  )$
  step(
    el = "nav",
    title = "Tab Selection",
    description = "Guide being built...",
    position = "right-center"
  )
