#' Starts up Shiny app that demonstrates some use of sportsanalyticsr data
#'
#' @import shiny
#' @export
run_example <- function() {
  appDir <- system.file("shiny_examples", "app", package = "sportsanalyticsr")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing
         `sportsanalyticsr`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
