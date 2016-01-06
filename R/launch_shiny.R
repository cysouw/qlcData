# launch shiny app

launch_shiny <- function(shiny_app) {
  appDir <- system.file("shiny", shiny_app, package = "qlcData")
  shiny::runApp(appDir, display.mode = "normal")
}