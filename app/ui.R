# ui.R
library(shiny)
library(shinythemes)

ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Time Series Analysis – Full ARIMA Model Selection"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Pengaturan Data"),
      selectInput(
        "freq", "Frequency:",
        choices = c("Monthly (12)" = 12,
                    "Quarterly (4)" = 4,
                    "Yearly (1)" = 1),
        selected = 12
      ),
      numericInput(
        "h", "Forecast Horizon:",
        value = 12, min = 1, step = 1
      ),
      hr(),
      checkboxInput("auto_model", "Pemilihan Model Otomatis", TRUE)
    ),
    
    mainPanel(
      tabsetPanel(id = "main_tabs",
        tabPanel("1. Cek Stasioneritas",
                 plotOutput("raw_ts_plot")),
        tabPanel("2. Transformasi & Differencing",
                 plotOutput("diff_ts_plot")),
        tabPanel("3. ACF & PACF",
                 acfPacfUI("acf_pacf")),     # dari diagnostics_module
        tabPanel("4. Time Series Decomposition",
                 decompUI("decomp")),        # dari ts_decomp_module
        tabPanel("5. Forecast (Best Model)",
                 forecastUI("forecast"))     # dari forecast_module
      )
    )
  )
)
