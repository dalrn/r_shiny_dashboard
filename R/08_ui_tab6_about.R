# ============================================================================
# TAB 6 UI - ABOUT 
# ============================================================================

ui_tab6_about <- function() {
  fluidPage(
    h2("ℹ️ About Dashboard"),
    
    fluidRow(
      column(
        width = 12,
        wellPanel(
          h3("📊 TIME SERIES FORECASTING DASHBOARD"),
          
          p(
            "This dashboard is designed to support exploratory analysis and forecasting ",
            "of univariate time series data. It provides tools for data visualization, ",
            "stationarity testing, ARIMA model specification, diagnostic checking, and ",
            "short-term forecasting."
          ),
          
          p(
            "The application is intended for educational and analytical purposes, ",
            "particularly for users learning classical time series methods."
          ),
          
          hr(),
                    
          h4("🎯 Intended Users"),
          p(
            "This dashboard is intended for students and practitioners who are learning ",
            "or applying classical time series forecasting techniques and want an ",
            "interactive, visual workflow."
          ),
          
          hr(),
          
          h4("🧮 Methods Used"),
          p(
            "The analysis and forecasting features are based on classical time series ",
            "techniques, including stationarity tests (ADF and KPSS), ARIMA modeling, ",
            "residual diagnostics, and point forecasting using the forecast package in R."
          ),
          
          hr(),
          
          h4("⚠️ Limitations"),
          tags$ul(
            tags$li("The dashboard focuses on univariate time series only."),
            tags$li("External regressors, structural breaks, and regime changes are not considered."),
          ),
          
          hr(),
          
          p(
            em("Built using R and Shiny.")
          )
        )
      )
    )
  )
}

