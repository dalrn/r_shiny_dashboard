# ============================================================================
# FILE: R/01_globals.R
# TUJUAN: Global configuration, constants, dan utility functions
# ============================================================================

# Global constants
COLOR_PALETTE <- list(
  primary = "#2E86AB",
  success = "#06A77D",
  danger = "#D62828",
  warning = "#F77F00",
  info = "#457B9D",
  light_bg = "#F8F9FA",
  dark_text = "#2C3E50"
)

CONFIDENCE_LEVELS <- c(80, 90, 95, 99)
MAX_FORECAST_HORIZON <- 36
MAX_DIFF_LEVEL <- 2
ADF_ALPHA <- 0.05

# ============================================================================
# UTILITY FUNCTIONS
# ============================================================================

format_number <- function(x, digits = 2) {
  format(round(x, digits = digits), big.mark = ",", trim = TRUE)
}

create_success_message <- function(text) {
  tags$div(
    style = "background-color: #E8F5E9; border-left: 4px solid #06A77D; padding: 12px; margin-bottom: 12px; border-radius: 4px;",
    tags$span(style = "color: #06A77D; font-weight: bold;", "✅ ", text)
  )
}

create_error_message <- function(text) {
  tags$div(
    style = "background-color: #FFEBEE; border-left: 4px solid #D62828; padding: 12px; margin-bottom: 12px; border-radius: 4px;",
    tags$span(style = "color: #D62828; font-weight: bold;", "❌ ", text)
  )
}

create_info_message <- function(text) {
  tags$div(
    style = "background-color: #E3F2FD; border-left: 4px solid #457B9D; padding: 12px; margin-bottom: 12px; border-radius: 4px;",
    tags$span(style = "color: #457B9D; font-weight: bold;", "ℹ️ ", text)
  )
}

# ============================================================================
# DATA VALIDATION & PREPROCESSING
# ============================================================================

validate_time_series_data <- function(time_col, value_col) {
  errors <- c()
  
  if (is.null(time_col) || is.null(value_col)) {
    errors <- c(errors, "Kolom time dan nilai harus dipilih")
  }
  
  if (length(time_col) < 10) {
    errors <- c(errors, "Data harus memiliki minimal 10 observasi")
  }
  
  if (any(is.na(value_col))) {
    errors <- c(errors, "Kolom nilai memiliki missing value")
  }
  
  if (length(unique(value_col)) < 2) {
    errors <- c(errors, "Kolom nilai tidak memiliki variasi (semuanya bernilai sama)")
  }
  
  list(
    is_valid = length(errors) == 0,
    errors = errors
  )
}

# ============================================================================
# TIME SERIES FREQUENCY DETECTION
# ============================================================================

detect_frequency <- function(dates) {
  if (length(dates) < 3) return(NA)
  
  tryCatch({
    dates_parsed <- NA
    
    if (all(is.na(dates_parsed))) {
      dates_parsed <- tryCatch(as.Date(dates, format = "%Y-%m-%d"), error = function(e) NA)
    }
    if (all(is.na(dates_parsed))) {
      dates_parsed <- tryCatch(as.Date(paste0(dates, "-01"), format = "%Y-%m-%d"), error = function(e) NA)
    }
    if (all(is.na(dates_parsed))) {
      dates_parsed <- tryCatch(as.Date(dates, format = "%m/%d/%Y"), error = function(e) NA)
    }
    if (all(is.na(dates_parsed))) {
      dates_parsed <- tryCatch(lubridate::parse_date_time(dates, orders = c("ymd", "mdy", "dmy")), error = function(e) NA)
    }
    
    if (all(is.na(dates_parsed))) return(NA)
    
    dates_sorted <- sort(dates_parsed)
    diffs <- diff(as.numeric(dates_sorted))
    avg_diff <- mean(diffs, na.rm = TRUE)
    
    if (avg_diff < 2) return(365)
    if (avg_diff < 5) return(52)
    if (avg_diff < 35) return(12)
    if (avg_diff < 100) return(4)
    return(1)
    
  }, error = function(e) NA)
}

detect_seasonality_strength <- function(ts_data, frequency) {
  tryCatch({
    if (length(ts_data) < 2 * frequency) {
      return(list(is_seasonal = FALSE, period = NA))
    }
    
    acf_result <- acf(ts_data, lag.max = min(length(ts_data) - 1, 2 * frequency), plot = FALSE)
    acf_values <- acf_result$acf[-1]
    
    seasonal_lag_values <- acf_values[seq(frequency, length(acf_values), by = frequency)]
    
    if (length(seasonal_lag_values) > 0 && max(abs(seasonal_lag_values), na.rm = TRUE) > 0.3) {
      list(is_seasonal = TRUE, period = frequency)
    } else {
      list(is_seasonal = FALSE, period = NA)
    }
  }, error = function(e) {
    list(is_seasonal = FALSE, period = NA)
  })
}

# ============================================================================
# ADF TEST WRAPPER
# ============================================================================

perform_adf_test <- function(ts_data) {
  tryCatch({
    result <- tseries::adf.test(ts_data)
    
    list(
      statistic = result$statistic,
      p_value = result$p.value,
      critical_values = result$critical.value,
      is_stationary = result$p.value <= ADF_ALPHA,
      interpretation = if (result$p.value <= ADF_ALPHA) {
        "Data STASIONER (tolak H0)"
      } else {
        "Data NON-STASIONER (gagal tolak H0)"
      }
    )
  }, error = function(e) {
    list(
      statistic = NA,
      p_value = NA,
      critical_values = NA,
      is_stationary = FALSE,
      interpretation = paste("Error:", e$message)
    )
  })
}

# ============================================================================
# LJUNG-BOX TEST WRAPPER
# ============================================================================

perform_ljung_box_test <- function(residuals) {
  tryCatch({
    result <- lmtest::Box.test(residuals, lag = 10, type = "Ljung-Box")
    
    list(
      statistic = result$statistic,
      p_value = result$p.value,
      is_white_noise = result$p.value > ADF_ALPHA,
      interpretation = if (result$p.value > ADF_ALPHA) {
        "Residual adalah WHITE NOISE (tidak ada autokorelasi)"
      } else {
        "Residual BUKAN WHITE NOISE (ada autokorelasi)"
      }
    )
  }, error = function(e) {
    list(
      statistic = NA,
      p_value = NA,
      is_white_noise = FALSE,
      interpretation = paste("Error:", e$message)
    )
  })
}

cat("✅ Global configuration loaded successfully!\n")