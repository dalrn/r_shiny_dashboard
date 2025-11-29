# ============================================================================
# FILE: R/09_server_main.R (FIXED VERSION)
# TUJUAN: Main server coordinator yang menghubungkan semua modules
# PENTING: File ini TIDAK boleh di-source di luar shinyApp()
# ============================================================================

create_server <- function(input, output, session) {
  
  # Global reactive values container
  rv <- reactiveValues(
    # Data
    raw_data = NULL,
    time_col = NULL,
    value_col = NULL,
    
    # Metadata
    time_col_name = NULL,
    value_col_name = NULL,
    data_frequency = NA,
    is_seasonal = FALSE,
    seasonal_period = NA,
    
    # Time series object
    ts_object = NULL,
    ts_differenced = NULL,
    diff_level = 0,
    
    # ADF tests
    adf_result_original = NULL,
    adf_result_differenced = NULL,
    
    # Model
    fitted_model = NULL,
    model_summary = NULL,
    model_type = "ARIMA",
    
    # Diagnostics
    diagnostics = NULL,
    ljung_box_result = NULL,
    
    # Forecast
    forecast_result = NULL,
    forecast_table = NULL
  )
  
  # IMPORTANT: Load server modules DENGAN local = TRUE
  # Ini memastikan output, input, session tersedia di dalam scope module
  source("R/10_server_upload.R", local = TRUE)
  source("R/11_server_stationarity.R", local = TRUE)
  source("R/12_server_parameters.R", local = TRUE)
  source("R/13_server_diagnostics.R", local = TRUE)
  source("R/14_server_forecast.R", local = TRUE)
  
  cat("✅ Server initialized!\n")
}