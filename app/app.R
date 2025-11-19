packages <- c("shiny", "shinythemes", "forecast", "ggplot2", "dplyr")
library(shiny)

# source semua komponen
source("app/ui.R")
source("app/server.R")
source("app/ts_decomp.R")
source("app/forecast.R")
source("app/diagnostics.R")

# jalankan aplikasi
shinyApp(ui = ui, server = server)
