# ============================================================================
# FILE: R/02_ui_main.R
# TUJUAN: Struktur UI utama dengan semua tab
# ============================================================================

create_ui <- function() {
  navbarPage(
    title = "📊 TIME SERIES FORECASTING DASHBOARD",
    theme = "default",
    
    # Tab 1: Upload & Exploration
    tabPanel(
      title = "📁 Data & Eksplorasi",
      ui_tab1_upload()
    ),
    
    # Tab 2: Stationarity
    tabPanel(
      title = "🔍 Uji Stasioneritas",
      ui_tab2_stationarity()
    ),
    
    # Tab 3: Parameters
    tabPanel(
      title = "⚙️ Identifikasi Parameter",
      ui_tab3_parameters()
    ),
    
    # Tab 4: Diagnostics
    tabPanel(
      title = "🔬 Diagnostik Model",
      ui_tab4_diagnostics()
    ),
    
    # Tab 5: Forecast
    tabPanel(
      title = "🎯 Forecasting",
      ui_tab5_forecast()
    ),
    
    # Tab 6: About
    tabPanel(
      title = "ℹ️ Tentang",
      ui_tab6_about()
    )
  )
}
