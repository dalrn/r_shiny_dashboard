# server.R
# aaaa
# test server.R

library(forecast)
library(ggplot2)
library(dplyr)

server <- function(input, output, session) {

  # 1) baca data sekali (diasumsikan ada kolom 'value')
  raw_data <- reactive({
    read.csv("Electric_Production.csv")
    read.csv("app/Electric_Production.csv")
  })

  # 2) buat objek ts berdasarkan input$freq
  ts_data <- reactive({
    x <- raw_data()[["value"]]
    x <- raw_data()[["IPG2211A2N"]]
    freq <- as.numeric(input$freq)
    ts(x, frequency = freq)
  })

  # 3) plot raw time series
  output$raw_ts_plot <- renderPlot({
    autoplot(ts_data()) +
      labs(title = "Raw Time Series", x = "Time", y = "Value")
  })

  # 4) differencing sederhana untuk cek stasioneritas
  diff_series <- reactive({
    diff(ts_data(), differences = 1)
  })

  output$diff_ts_plot <- renderPlot({
    autoplot(diff_series()) +
      labs(title = "Differenced Series (d = 1)", x = "Time", y = "Diff")
  })

  # 5) panggil modul-modul
  callModule(decompServer, "decomp", ts_data = ts_data)
  callModule(acfPacfServer, "acf_pacf", ts_data = ts_data, diff_series = diff_series)
  callModule(forecastServer, "forecast",
             ts_data = ts_data,
             h = reactive(as.integer(input$h)),
             auto_model = reactive(input$auto_model))
}