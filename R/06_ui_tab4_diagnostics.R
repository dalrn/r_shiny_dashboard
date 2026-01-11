# ============================================================================
# FILE: R/06_ui_tab4_diagnostics.R
# TAB 4 UI - Model Diagnostics
# ============================================================================

ui_tab4_diagnostics <- function() {
  fluidPage(
    h2("🔬 Model Diagnostics"),
    
    fluidRow(
      column(
        width = 12,
        wellPanel(
          h4("📊 Model Quality"),
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
    
    h4("📈 Diagnostik Residual (4-Plot)"),
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
          h4("✅ Diagnostics Conclusion"),
          uiOutput("diagnostics_conclusion")
        )
      )
    )
  )
}