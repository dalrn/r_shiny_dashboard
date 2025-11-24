# forecast_module.R
library(forecast)

forecastUI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(
      ns("model_type"), "Model:",
      choices = c("ARIMA (auto.arima)", "ETS"),
      selected = "ARIMA (auto.arima)"
    ),
    plotOutput(ns("fc_plot")),
    tableOutput(ns("fc_table"))
  )
}

forecastServer <- function(input, output, session,
                           ts_data, h, auto_model) {
  
  model_fit <- reactive({
    ts_obj <- ts_data()
    
    if (input$model_type == "ETS") {
      ets(ts_obj)
    } else {
      if (auto_model()) {
        auto.arima(ts_obj)
      } else {
        # contoh: fixed ARIMA(1,1,1) bila ingin manual
        Arima(ts_obj, order = c(1, 1, 1))
      }
    }
  })
  
  fc_obj <- reactive({
    forecast(model_fit(), h = h())
  })
  
  output$fc_plot <- renderPlot({
    autoplot(fc_obj()) +
      labs(title = "Forecast", x = "Time", y = "Value")
  })
  
  output$fc_table <- renderTable({
    as.data.frame(fc_obj())
  }, rownames = TRUE)
}

