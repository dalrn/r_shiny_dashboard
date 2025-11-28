# ============================================================================
# FILE: R/13_server_diagnostics.R
# TUJUAN: Server logic untuk model diagnostics
# ============================================================================

output$diagnostics_metrics <- renderPrint({
  if (is.null(rv$fitted_model)) {
    cat("No model fitted yet\n")
    cat("Please fit model in 'Identifikasi Parameter' tab first\n")
    return()
  }
  
  tryCatch({
    cat("=== MODEL QUALITY METRICS ===\n\n")
    cat("AIC:", format_number(AIC(rv$fitted_model)), "\n")
    cat("BIC:", format_number(BIC(rv$fitted_model)), "\n")
    
    residuals_model <- residuals(rv$fitted_model)
    rmse <- sqrt(mean(residuals_model^2))
    mae <- mean(abs(residuals_model))
    
    cat("RMSE:", format_number(rmse), "\n")
    cat("MAE:", format_number(mae), "\n")
    
  }, error = function(e) {
    cat("Error calculating metrics:", e$message, "\n")
  })
})

# ============================================================================
# OUTPUT: Ljung-Box Test (White Noise Check)
# ============================================================================

output$ljung_box_output <- renderPrint({
  if (is.null(rv$fitted_model)) {
    cat("No model fitted yet\n")
    return()
  }
  
  tryCatch({
    residuals_model <- residuals(rv$fitted_model)
    
    # ========================================================================
    # PENTING: Gunakan Box.test() dari base R (bukan lmtest::Box.test)
    # Box.test adalah base R function untuk Ljung-Box test
    # ========================================================================
    ljung_box_test <- Box.test(residuals_model, lag = 10, type = "Ljung-Box")
    
    rv$ljung_box_result <- list(
      statistic = ljung_box_test$statistic,
      p_value = ljung_box_test$p.value,
      is_white_noise = ljung_box_test$p.value > 0.05,
      interpretation = if (ljung_box_test$p.value > 0.05) 
        "Residuals are WHITE NOISE (model adequate)" 
        else "Residuals are NOT white noise (model may need improvement)"
    )
    
    cat("=== LJUNG-BOX TEST ===\n\n")
    cat("Test Statistic:", format_number(rv$ljung_box_result$statistic), "\n")
    cat("P-value:", format(rv$ljung_box_result$p_value, digits = 4), "\n\n")
    cat("Interpretation:", rv$ljung_box_result$interpretation, "\n")
    
  }, error = function(e) {
    cat("Error performing Ljung-Box test:", e$message, "\n")
    rv$ljung_box_result <- NULL
  })
})

# ============================================================================
# OUTPUT: Ljung-Box Interpretation (UI)
# ============================================================================

output$ljung_box_interpretation <- renderUI({
  if (is.null(rv$ljung_box_result)) return(NULL)
  
  if (rv$ljung_box_result$is_white_noise) {
    create_success_message(
      paste("✅ Residuals are WHITE NOISE (p-value =", 
            format(rv$ljung_box_result$p_value, digits = 4), ")")
    )
  } else {
    create_error_message(
      paste("❌ Residuals are NOT white noise (p-value =", 
            format(rv$ljung_box_result$p_value, digits = 4), ")")
    )
  }
})

# ============================================================================
# OUTPUT: Residuals Over Time Plot
# ============================================================================

output$plot_residuals_ts <- renderPlot({
  if (is.null(rv$fitted_model)) {
    plot.new()
    text(0.5, 0.5, "No model fitted yet", cex = 1.2, col = "gray50")
    return()
  }
  
  tryCatch({
    residuals_model <- residuals(rv$fitted_model)
    plot(residuals_model, 
         main = "Residuals Over Time", 
         ylab = "Residuals",
         xlab = "Time",
         type = "l")
    abline(h = 0, col = "red", lty = 2)
    grid()
    
  }, error = function(e) {
    plot.new()
    text(0.5, 0.5, paste("Error:", e$message), cex = 1, col = "red")
  })
})

# ============================================================================
# OUTPUT: ACF of Residuals Plot
# ============================================================================

output$plot_residuals_acf <- renderPlot({
  if (is.null(rv$fitted_model)) {
    plot.new()
    text(0.5, 0.5, "No model fitted yet", cex = 1.2, col = "gray50")
    return()
  }
  
  tryCatch({
    residuals_model <- residuals(rv$fitted_model)
    acf(residuals_model, 
        main = "ACF of Residuals",
        xlab = "Lag",
        ylab = "ACF")
    
  }, error = function(e) {
    plot.new()
    text(0.5, 0.5, paste("Error:", e$message), cex = 1, col = "red")
  })
})

# ============================================================================
# OUTPUT: Q-Q Plot (Normality Check)
# ============================================================================

output$plot_residuals_qq <- renderPlot({
  if (is.null(rv$fitted_model)) {
    plot.new()
    text(0.5, 0.5, "No model fitted yet", cex = 1.2, col = "gray50")
    return()
  }
  
  tryCatch({
    residuals_model <- residuals(rv$fitted_model)
    qqnorm(residuals_model, 
           main = "Q-Q Plot",
           xlab = "Theoretical Quantiles",
           ylab = "Sample Quantiles")
    qqline(residuals_model, col = "red", lwd = 2)
    grid()
    
  }, error = function(e) {
    plot.new()
    text(0.5, 0.5, paste("Error:", e$message), cex = 1, col = "red")
  })
})

# ============================================================================
# OUTPUT: Histogram of Residuals
# ============================================================================

output$plot_residuals_hist <- renderPlot({
  if (is.null(rv$fitted_model)) {
    plot.new()
    text(0.5, 0.5, "No model fitted yet", cex = 1.2, col = "gray50")
    return()
  }
  
  tryCatch({
    residuals_model <- residuals(rv$fitted_model)
    hist(residuals_model, 
         main = "Histogram of Residuals", 
         xlab = "Residuals",
         ylab = "Frequency",
         breaks = 15,
         col = "steelblue",
         border = "white")
    
  }, error = function(e) {
    plot.new()
    text(0.5, 0.5, paste("Error:", e$message), cex = 1, col = "red")
  })
})

# ============================================================================
# OUTPUT: Diagnostics Conclusion
# ============================================================================

output$diagnostics_conclusion <- renderUI({
  if (is.null(rv$fitted_model) || is.null(rv$ljung_box_result)) return(NULL)
  
  tryCatch({
    if (rv$ljung_box_result$is_white_noise) {
      create_success_message("✅ MODEL IS VALID - Ready for forecasting!")
    } else {
      create_error_message("⚠️ Model may need improvement - Consider trying different parameters")
    }
  }, error = function(e) {
    create_error_message("Error generating conclusion")
  })
})