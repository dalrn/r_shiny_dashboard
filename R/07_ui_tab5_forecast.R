# ============================================================================
# TAB 5 UI - FORECAST
# ============================================================================

ui_tab5_forecast <- function() {
  fluidPage(
    h2("🎯 Forecasting"),
    
    fluidRow(
      column(
        width = 4,
        wellPanel(
          h4("⚙️ Forecast Configuration"),
          sliderInput("forecast_horizon", "Forecast Horizon (Periode):",
                      min = 1, max = 36, value = 12, step = 1),
          hr(),
          h4("Confidence Level:"),
          radioButtons("forecast_ci", NULL,
                       choices = list("80%" = 80, "90%" = 90, "95%" = 95, "99%" = 99),
                       selected = 95),
          hr(),
          actionButton("generate_forecast_btn", "📊 Generate Forecast",
                       class = "btn-success btn-lg", width = "100%"),
          hr(),
          downloadButton("download_forecast_csv", "⬇️ Download CSV",
                        class = "btn-info", width = "100%")
        )
      ),
      
      column(
        width = 8,
        wellPanel(
          h4("📈 Forecast Visualization"),
          plotlyOutput("plot_forecast", height = "500px")
        )
      )
    ),
    
    hr(),
    
    fluidRow(
      column(
        width = 12,
        wellPanel(
          h4("📋 Forecast Values"),
          DT::dataTableOutput("forecast_table")
        )
      )
    )
  )
}