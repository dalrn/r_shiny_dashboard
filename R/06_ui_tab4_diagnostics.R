# ============================================================================
# FILE: R/06_ui_tab4_diagnostics.R
# TUJUAN: UI untuk Tab 4 - Diagnostik Model
# ============================================================================

ui_tab4_diagnostics <- function() {
  fluidPage(
    h2("🔬 Diagnostik Model"),
    
    fluidRow(
      column(
        width = 12,
        wellPanel(
          h4("📊 Model Quality Metrics"),
          verbatimTextOutput("diagnostics_metrics")
        )
      )
    ),
    
    fluidRow(
      column(
        width = 12,
        wellPanel(
          h4("🧪 Ljung-Box Test (White Noise Check)"),
          verbatimTextOutput("ljung_box_output"),
          uiOutput("ljung_box_interpretation")
        )
      )
    ),
    
    hr(),
    
    h4("📈 Residual Diagnostics (4-Plot)"),
    fluidRow(
      column(width = 6, plotOutput("plot_residuals_ts")),
      column(width = 6, plotOutput("plot_residuals_acf"))
    ),
    fluidRow(
      column(width = 6, plotOutput("plot_residuals_qq")),
      column(width = 6, plotOutput("plot_residuals_hist"))
    ),
    
    hr(),
    
    fluidRow(
      column(
        width = 12,
        wellPanel(
          h4("✅ Diagnostik Conclusion"),
          uiOutput("diagnostics_conclusion")
        )
      )
    )
  )
}