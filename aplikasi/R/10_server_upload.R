# ============================================================================
# FILE: R/10_server_upload.R
# TUJUAN: Server logic untuk upload dan explorasi data
# ============================================================================

# Observe file upload
observeEvent(input$file_upload, {
  tryCatch({
    file <- input$file_upload
    ext <- tools::file_ext(file$datapath)
    
    if (ext == "csv") {
      rv$raw_data <- read.csv(file$datapath, stringsAsFactors = FALSE)
    } else if (ext %in% c("xlsx", "xls")) {
      rv$raw_data <- readxl::read_excel(file$datapath)
    } else {
      showNotification("File format not supported. Use CSV or Excel.", type = "error")
      return()
    }
    
    col_choices <- names(rv$raw_data)
    updateSelectInput(session, "time_column", choices = col_choices, selected = col_choices[1])
    updateSelectInput(session, "value_column", choices = col_choices, selected = col_choices[2])
    
    showNotification("✅ File loaded successfully!", type = "message")
    
  }, error = function(e) {
    showNotification(paste("❌ Error loading file:", e$message), type = "error")
  })
})

# Observe column selection
observeEvent(list(input$time_column, input$value_column), {
  if (is.null(rv$raw_data) || is.null(input$time_column) || is.null(input$value_column)) return()
  
  tryCatch({
    rv$time_col_name <- input$time_column
    rv$value_col_name <- input$value_column
    
    rv$time_col <- rv$raw_data[[input$time_column]]
    rv$value_col <- rv$raw_data[[input$value_column]]
    
    validation <- validate_time_series_data(rv$time_col, rv$value_col)
    if (!validation$is_valid) {
      showNotification(paste(validation$errors, collapse = "; "), type = "error")
      rv$time_col <- NULL
      rv$value_col <- NULL
      return()
    }
    
    rv$value_col <- as.numeric(rv$value_col)
    
    rv$time_col <- tryCatch({
      result <- tryCatch(as.Date(rv$time_col), error = function(e) NA)
      if (all(is.na(result))) {
        result <- tryCatch(as.Date(paste0(rv$time_col, "-01"), format = "%Y-%m-%d"), error = function(e) NA)
      }
      if (all(is.na(result))) {
        result <- tryCatch(lubridate::parse_date_time(rv$time_col, orders = c("ymd", "mdy", "dmy")), error = function(e) NA)
      }
      result
    }, error = function(e) rv$time_col)
    
    rv$data_frequency <- detect_frequency(rv$time_col)
    if (!is.na(rv$data_frequency)) {
      rv$ts_object <- ts(rv$value_col, frequency = rv$data_frequency)
    } else {
      rv$ts_object <- ts(rv$value_col)
    }
    
    # ========================================================================
    # INISIALISASI DEFAULT: Set diff_level = 0 dan ts_differenced = ts_object
    # Ini memastikan ACF/PACF plot langsung muncul untuk d=0 tanpa user klik
    # ========================================================================
    rv$diff_level <- 0
    rv$ts_differenced <- rv$ts_object
    
    if (!is.na(rv$data_frequency)) {
      seasonal_info <- detect_seasonality_strength(rv$value_col, rv$data_frequency)
      rv$is_seasonal <- seasonal_info$is_seasonal
      rv$seasonal_period <- seasonal_info$period
    }
    
    showNotification("✅ Columns selected successfully!", type = "message")
    
  }, error = function(e) {
    showNotification(paste("❌ Error:", e$message), type = "error")
    rv$time_col <- NULL
    rv$value_col <- NULL
  })
})

output$data_loaded_msg <- renderUI({
  if (is.null(rv$value_col)) return(NULL)
  
  create_success_message(
    glue::glue("Data LOADED: {length(rv$value_col)} observations | Frequency: {rv$data_frequency} | Seasonal: {if(rv$is_seasonal) 'YES' else 'NO'}")
  )
})

output$data_preview <- DT::renderDataTable({
  if (is.null(rv$raw_data)) return(NULL)
  
  DT::datatable(
    head(rv$raw_data, 20),
    options = list(pageLength = 10, scrollX = TRUE, lengthMenu = list(c(10, 20, 50, -1), c("10", "20", "50", "All")))
  )
})

output$data_summary <- renderPrint({
  if (is.null(rv$value_col)) return(cat("No data loaded yet"))
  
  cat("=== DATA SUMMARY ===\n\n")
  cat("Observations:", length(rv$value_col), "\n")
  cat("Mean:", format_number(mean(rv$value_col)), "\n")
  cat("Std Dev:", format_number(sd(rv$value_col)), "\n")
  cat("Min:", format_number(min(rv$value_col)), "\n")
  cat("Max:", format_number(max(rv$value_col)), "\n")
  cat("\nFrequency:", rv$data_frequency, "\n")
  cat("Seasonal:", if(rv$is_seasonal) paste("YES, Period =", rv$seasonal_period) else "NO", "\n")
})

output$auto_detection_info <- renderPrint({
  if (is.null(rv$value_col)) return(cat("No data loaded yet"))
  
  freq_label <- if (rv$data_frequency == 365) "Daily"
  else if (rv$data_frequency == 52) "Weekly"
  else if (rv$data_frequency == 12) "Monthly"
  else if (rv$data_frequency == 4) "Quarterly"
  else "Yearly"
  
  cat("=== AUTO DETECTION ===\n\n")
  cat("Frequency:", rv$data_frequency, "(", freq_label, ")\n")
  cat("Seasonality Detected:", if(rv$is_seasonal) paste("YES, Period =", rv$seasonal_period) else "NO", "\n")
})

output$plot_initial <- renderPlotly({
  if (is.null(rv$ts_object)) return(NULL)
  
  df <- data.frame(
    Time = seq_along(rv$value_col),
    Value = rv$value_col
  )
  
  fit <- lm(Value ~ Time, data = df)
  df$Trend <- predict(fit)
  
  plotly::plot_ly(df, x = ~Time, y = ~Value, type = "scatter", mode = "lines",
          name = "Data", line = list(color = "#2E86AB")) %>%
    plotly::add_trace(y = ~Trend, name = "Trend", mode = "lines", line = list(color = "#D62828", dash = "dash")) %>%
    plotly::layout(
      title = "Original Time Series",
      xaxis = list(title = "Time"),
      yaxis = list(title = "Value"),
      hovermode = "x unified"
    )
})