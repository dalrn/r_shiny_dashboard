# ui.R
library(shiny)
library(shinythemes)

ui <- fluidPage(
  theme = shinytheme("flatly"),

  tags$head(
    tags$style(HTML("
      body {
        background: linear-gradient(135deg, #1d2b64, #f8cdda);
        font-family: 'Segoe UI', sans-serif;
      }

      h4, h3, h2, h1 {
        font-weight: 600;
        color: #ffffff;
      }

      /* Card styling */
      .custom-card {
        background: white;
        padding: 20px;
        border-radius: 15px;
        box-shadow: 0 4px 15px rgba(0,0,0,0.15);
        margin-bottom: 20px;
        transition: 0.3s;
      }
      .custom-card:hover {
        transform: translateY(-4px);
        box-shadow: 0 10px 25px rgba(0,0,0,0.25);
      }

      /* Sidebar styling */
      .well {
        background: rgba(255, 255, 255, 0.9) !important;
        border-radius: 12px !important;
        border: 1px solid #eee !important;
      }

      /* Tab styling */
      .nav-tabs > li > a {
        font-size: 15px;
        font-weight: 600;
        color: #ffffff !important;
        background: rgba(255,255,255,0.1);
        border-radius: 8px 8px 0 0;
        margin-right: 3px;
      }

      .nav-tabs > li > a:hover {
        background: rgba(255,255,255,0.3) !important;
      }

      .nav-tabs > li.active > a {
        background: white !important;
        color: #333 !important;
      }

    "))
  ),

  div(
    style="text-align:center; padding:20px;",
    h1("📈 Time Series Analysis - ARIMA Model Selection"),
    h4(style="color:white; opacity:0.9;",
       "Dashboard Analisis Time Series")
  ),

  sidebarLayout(

    # ---------- SIDEBAR ----------
    sidebarPanel(
      h3("⚙ Pengaturan Data"),
      selectInput(
        "freq", "Frequency:",
        choices = c("Monthly (12)" = 12,
                    "Quarterly (4)" = 4,
                    "Yearly (1)" = 1),
        selected = 12
      ),
      numericInput("h", "Forecast Horizon:", value = 12, min = 1),
      hr(),
      checkboxInput("auto_model", "Pemilihan Model Otomatis", TRUE),
      width = 3
    ),

    mainPanel(
      tabsetPanel(id = "main_tabs",
                  
        # ------ TAB 1 ------
        tabPanel("1. Cek Stasioneritas",
          div(class = "custom-card",
              plotOutput("raw_ts_plot")
          )
        ),

        # ------ TAB 2 ------
        tabPanel("2. Transformasi & Differencing",
          div(class = "custom-card",
              plotOutput("diff_ts_plot")
          )
        ),

        # ------ TAB 3 ------
        tabPanel("3. ACF & PACF",
          div(class = "custom-card",
              acfPacfUI("acf_pacf")
          )
        ),

        # ------ TAB 4 ------
        tabPanel("4. Time Series Decomposition",
          div(class = "custom-card",
              decompUI("decomp")
          )
        ),

        # ------ TAB 5 ------
        tabPanel("5. Forecast (Best Model)",
          div(class = "custom-card",
              forecastUI("forecast")
          )
        )
      )
    )
  )
)

