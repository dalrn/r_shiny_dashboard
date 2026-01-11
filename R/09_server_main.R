# ============================================================================
# MAIN SERVER COORDINATOR CONNECTING ALL MODULES
# ============================================================================

# Load server module functions 
source("R/10_server_tab1_upload.R")
source("R/11_server_tab2_stationarity.R")
source("R/12_server_tab3_parameters.R")
source("R/13_server_tab4_diagnostics.R")
source("R/14_server_tab5_forecast.R")

create_server <- function(input, output, session) {

  # Container global reactive values
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

  # Call the modules
  server_upload(input, output, session, rv)
  server_stationarity(input, output, session, rv)
  server_parameters(input, output, session, rv)
  server_diagnostics(input, output, session, rv)
  server_forecast(input, output, session, rv)

  cat("✅ Server initialized!\n")
}
