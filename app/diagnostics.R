# diagnostics_module.R
# komstat keren
# komstat hebat 
#dddd

acfPacfUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(6, plotOutput(ns("acf_plot"))),
    column(6, plotOutput(ns("pacf_plot")))
  )
}

acfPacfServer <- function(input, output, session, ts_data, diff_series) {

  output$acf_plot <- renderPlot({
    tsdisplay(diff_series(), main = "ACF (Differenced Series)")
  })
  
  output$pacf_plot <- renderPlot({
    pacf(diff_series(), main = "PACF (Differenced Series)")
  })
}
<<<<<<< HEAD
=======

>>>>>>> f76d71f0261e0fa01b84f490ba5ab49258c8061e
