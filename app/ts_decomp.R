# ts_decomp_module.R
library(forecast)

decompUI <- function(id) {
  ns <- NS(id)
  tagList(
    radioButtons(
      ns("method"), "Metode dekomposisi:",
      choices = c("Classical additive", "STL"),
      inline = TRUE
    ),
    plotOutput(ns("decomp_plot"))
  )
}

decompServer <- function(input, output, session, ts_data) {
  
  output$decomp_plot <- renderPlot({
    ts_obj <- ts_data()
    
    if (input$method == "STL") {
      fit <- stl(ts_obj, s.window = "periodic")
      autoplot(fit) + ggtitle("STL Decomposition")
    } else {
      fit <- decompose(ts_obj, type = "additive")
      autoplot(fit) + ggtitle("Classical Decomposition")
    }
  })
}
