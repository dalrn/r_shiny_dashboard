# ============================================================================
# FILE: R/04_ui_tab2_stationarity.R
# TUJUAN: UI untuk Tab 2 - Uji Stationaritas
# ============================================================================

ui_tab2_stationarity <- function() {
  fluidPage(
    h2("🔍 Uji Stationaritas (ADF Test)"),
    
    fluidRow(
      column(
        width = 12,
        wellPanel(
          h4("ADF Test - Data Original"),
          verbatimTextOutput("adf_test_original"),
          uiOutput("adf_interpretation_original")
        )
      )
    ),
    
    fluidRow(
      column(
        width = 6,
        wellPanel(
          h4("Pilih Tingkat Differencing"),
          radioButtons("diff_choice", "d (Differencing Level):",
                       choices = list("d = 0 (No differencing)" = 0,
                                     "d = 1 (1st difference)" = 1,
                                     "d = 2 (2nd difference)" = 2),
                       selected = 0)
        )
      ),
      
      column(
        width = 6,
        wellPanel(
          h4("ADF Test - Setelah Differencing"),
          verbatimTextOutput("adf_test_differenced"),
          uiOutput("adf_interpretation_differenced")
        )
      )
    ),
    
    hr(),
    
    h4("ACF & PACF Plots"),
    fluidRow(
      column(width = 6, plotOutput("plot_acf")),
      column(width = 6, plotOutput("plot_pacf"))
    )
  )
}