# ============================================================================
# FILE: R/14_server_forecast.R
# TUJUAN: Server logic untuk forecasting
# ============================================================================

observeEvent(input$generate_forecast_btn, {
  tryCatch({
    # Validasi: model harus sudah di-fit terlebih dahulu
    if (is.null(rv$fitted_model)) {
      showNotification("❌ Please fit model first in 'Identifikasi Parameter' tab", type = "error")
      return()
    }
    
    # Validasi: horizon harus numeric dan valid
    h <- as.integer(input$forecast_horizon)
    if (is.na(h) || h < 1) {
      showNotification("❌ Forecast horizon must be >= 1", type = "error")
      return()
    }
    
    # Validasi: confidence level harus numeric dan valid
    ci_level <- as.integer(input$forecast_ci) / 100
    if (is.na(ci_level) || ci_level < 0.5 || ci_level > 0.99) {
      showNotification("❌ Confidence level must be between 50-99%", type = "error")
      return()
    }
    
    showNotification("⏳ Generating forecast...", type = "message")
    
    tryCatch({
      # Generate forecast menggunakan fitted model
      rv$forecast_result <- forecast::forecast(
        rv$fitted_model,
        h = h,
        level = ci_level * 100  # forecast package expects 0-100 scale, not 0-1
      )
      
      # Buat table dengan hasil forecast
      rv$forecast_table <- data.frame(
        Period = seq_len(h),
        Forecast = as.numeric(rv$forecast_result$mean),
        Lower_CI = as.numeric(rv$forecast_result$lower[, 1]),
        Upper_CI = as.numeric(rv$forecast_result$upper[, 1]),
        stringsAsFactors = FALSE
      )
      
      showNotification("✅ Forecast generated successfully!", type = "message")
      
    }, error = function(e) {
      showNotification(paste("❌ Error generating forecast:", e$message), type = "error")
      rv$forecast_result <- NULL
      rv$forecast_table <- NULL
    })
    
  }, error = function(e) {
    showNotification(paste("❌ Error:", e$message), type = "error")
  })
})

# ============================================================================
# OUTPUT: Forecast Plot
# ============================================================================

output$plot_forecast <- renderPlotly({
  if (is.null(rv$forecast_result)) {
    return(
      plotly::plot_ly() %>%
        plotly::add_text(x = 0.5, y = 0.5, text = "Generate forecast first", 
                 textposition = "center", showlegend = FALSE) %>%
        plotly::layout(title = "Forecast Visualization", xaxis = list(title = "Time"), 
              yaxis = list(title = "Value"), hovermode = "x unified")
    )
  }
  
  tryCatch({
    h <- nrow(rv$forecast_table)
    hist_points <- seq_along(rv$value_col)
    fc_points <- seq_along(rv$value_col)[length(rv$value_col)] + seq_len(h)
    
    p <- plotly::plot_ly() %>%
      plotly::add_trace(x = hist_points, y = rv$value_col, name = "Historical", mode = "lines",
                line = list(color = "#2E86AB")) %>%
      plotly::add_trace(x = fc_points, y = rv$forecast_result$mean, name = "Forecast", 
                mode = "lines+markers", line = list(color = "#06A77D")) %>%
      plotly::add_trace(x = fc_points, y = rv$forecast_result$lower[, 1], name = "Lower CI",
                mode = "lines", line = list(color = "rgba(100,100,100,0)", width = 0),
                hoverinfo = "skip", showlegend = FALSE) %>%
      plotly::add_trace(x = fc_points, y = rv$forecast_result$upper[, 1], name = "Upper CI",
                mode = "lines", line = list(color = "rgba(100,100,100,0)", width = 0),
                fill = "tonexty", fillcolor = "rgba(0,200,100,0.2)", hoverinfo = "skip", 
                showlegend = FALSE) %>%
      plotly::layout(
        title = paste("Forecast -", input$forecast_ci, "% Confidence Interval"),
        xaxis = list(title = "Time"),
        yaxis = list(title = "Value"),
        hovermode = "x unified"
      )
    
    p
    
  }, error = function(e) {
    plotly::plot_ly() %>%
      plotly::add_text(x = 0.5, y = 0.5, text = paste("Error:", e$message), 
               textposition = "center", showlegend = FALSE) %>%
      plotly::layout(title = "Forecast Visualization Error", xaxis = list(title = "Time"), 
            yaxis = list(title = "Value"))
  })
})

# ============================================================================
# OUTPUT: Forecast Table
# ============================================================================

output$forecast_table <- DT::renderDataTable({
  if (is.null(rv$forecast_table)) {
    return(
      DT::datatable(
        data.frame(Message = "No forecast generated yet"),
        options = list(dom = "t")
      )
    )
  }
  
  DT::datatable(
    rv$forecast_table,
    options = list(
      pageLength = 12,
      scrollX = TRUE,
      lengthMenu = list(c(10, 20, 50, -1), c("10", "20", "50", "All"))
    ),
    rownames = FALSE
  )
})

# ============================================================================
# OUTPUT: Download Forecast CSV
# ============================================================================

output$download_forecast_csv <- downloadHandler(
  filename = function() {
    paste0("forecast_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
  },
  content = function(file) {
    if (is.null(rv$forecast_table)) {
      showNotification("❌ No forecast to download. Generate forecast first.", type = "error")
      return()
    }
    
    tryCatch({
      write.csv(rv$forecast_table, file, row.names = FALSE)
      showNotification("✅ Forecast downloaded successfully!", type = "message")
    }, error = function(e) {
      showNotification(paste("❌ Error downloading:", e$message), type = "error")
    })
  }
)