# ============================================================================
# FILE: R/02_ui_main.R
# TUJUAN: Membuat struktur UI utama dengan semua tabs
# ============================================================================

create_ui <- function() {
  navbarPage(
    title = "📊 TIME SERIES FORECASTING DASHBOARD",
    theme = "default",
    
    # Tab 1: Upload & Exploration
    tabPanel(
      title = "📁 Data & Eksplorasi",
      icon = icon("upload"),
      ui_tab1_upload()
    ),
    
    # Tab 2: Stationarity
    tabPanel(
      title = "🔍 Uji Stationaritas",
      icon = icon("chart-line"),
      ui_tab2_stationarity()
    ),
    
    # Tab 3: Parameters
    tabPanel(
      title = "⚙️ Identifikasi Parameter",
      icon = icon("sliders"),
      ui_tab3_parameters()
    ),
    
    # Tab 4: Diagnostics
    tabPanel(
      title = "🔬 Diagnostik Model",
      icon = icon("microscope"),
      ui_tab4_diagnostics()
    ),
    
    # Tab 5: Forecast
    tabPanel(
      title = "🎯 Forecasting",
      icon = icon("chart-bar"),
      ui_tab5_forecast()
    ),
    
    # Tab 6: About
    tabPanel(
      title = "ℹ️ Tentang",
      icon = icon("info-circle"),
      ui_tab6_about()
    )
  )
}
