# ==============================================================================
# FILE: app.R - MAIN ENTRY POINT
# ==============================================================================
rm(list = ls())
gc()

library(shiny)
library(shinyjs)

source("R/00_packages.R")
source("R/01_globals.R")
source("R/02_ui_main.R")
source("R/03_ui_tab1_upload.R")
source("R/04_ui_tab2_stationarity.R")
source("R/05_ui_tab3_parameters.R")
source("R/06_ui_tab4_diagnostics.R")
source("R/07_ui_tab5_forecast.R")
source("R/08_ui_tab6_about.R")
source("R/09_server_main.R")

shinyApp(
  ui = create_ui(),
  server = create_server
)
