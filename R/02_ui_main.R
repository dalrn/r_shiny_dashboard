# ============================================================================
# MAIN UI STRUCTURE ACROSS FILES
# ============================================================================

create_ui <- function() {
  navbarPage(
    title = "📊 TIME SERIES FORECASTING DASHBOARD",
    theme = "default",
    
    # Tab 1: Upload & Exploration
    tabPanel(
      title = "📁 Data Exploration",
      ui_tab1_upload()
    ),
    
    # Tab 2: Stationarity
    tabPanel(
      title = "🔍 Stationarity Test",
      ui_tab2_stationarity()
    ),
    
    # Tab 3: Modeling
    tabPanel(
      title = "⚙️ Model Parameters",
      ui_tab3_parameters()
    ),
    
    # Tab 4: Diagnostics
    tabPanel(
      title = "🔬 Model Diagnostics",
      ui_tab4_diagnostics()
    ),
    
    # Tab 5: Forecast
    tabPanel(
      title = "🎯 Forecasting",
      ui_tab5_forecast()
    ),
    
    # Tab 6: About
    tabPanel(
      title = "ℹ️ About",
      ui_tab6_about()
    )
  )
}
