packages <- c("shiny", "shinythemes", "forecast", "ggplot2", "dplyr")
library(shiny)

# source semua komponen
source("r_shiny_dashboard/app/ui.R")
source("r_shiny_dashboard/app/server.R")
source("r_shiny_dashboard/app/ts_decomp.R")
source("r_shiny_dashboard/app/forecast.R")
source("r_shiny_dashboard/app/diagnostics.R")

# jalankan aplikasi
shinyApp(ui = ui, server = server)
