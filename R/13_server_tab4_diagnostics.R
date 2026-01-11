# ============================================================================
# TAB 5 SERVER LOGIC - DIAGNOSTIC CHECKING
# ============================================================================

server_diagnostics <- function(input, output, session, rv) {

  # --------------------------------------------------------------------------
  # 1. MODEL METRICS (AIC, BIC, RMSE, MAE)
  # --------------------------------------------------------------------------
  output$diagnostics_metrics <- renderPrint({
    if (is.null(rv$fitted_model)) {
      cat("Model not fitted yet\n")
      cat("Please fit model in 'Model Parameters' tab first\n")
      return()
    }

    tryCatch({
      cat("=== MODEL METRICS ===\n\n")

      cat("AIC:", format_number(AIC(rv$fitted_model)), "\n")
      cat("BIC:", format_number(BIC(rv$fitted_model)), "\n")

      residuals_model <- residuals(rv$fitted_model)
      rmse <- sqrt(mean(residuals_model^2))
      mae  <- mean(abs(residuals_model))

      cat("RMSE:", format_number(rmse), "\n")
      cat("MAE :", format_number(mae), "\n")

    }, error = function(e) {
      cat("Error calculating metrics:", e$message, "\n")
    })
  })

  # --------------------------------------------------------------------------
  # 2. LJUNG-BOX TEST
  # --------------------------------------------------------------------------
  output$ljung_box_output <- renderPrint({
    if (is.null(rv$fitted_model)) {
      cat("Model not fitted yet\n")
      return()
    }

    tryCatch({
      residuals_model <- residuals(rv$fitted_model)

      # Perform Ljung-Box test
      ljung_box_test <- Box.test(
        residuals_model,
        lag  = 10,
        type = "Ljung-Box"
      )

      # Save in list for UI interpretation
      rv$ljung_box_result <- list(
        statistic = ljung_box_test$statistic,
        p_value = ljung_box_test$p.value,
        is_white_noise = ljung_box_test$p.value > 0.05,
        interpretation = if (ljung_box_test$p.value > 0.05)
          "Residuals are WHITE NOISE (no autocorrelation)"
        else
          "Residuals are NOT WHITE NOISE (have autocorrelation)"
      )

      cat("=== LJUNG-BOX TEST ===\n\n")
      cat("Test Statistic:",
          format_number(rv$ljung_box_result$statistic), "\n")
      cat("P-value:",
          format(rv$ljung_box_result$p_value, digits = 4), "\n\n")
      cat("Decision:",
          rv$ljung_box_result$interpretation, "\n")

    }, error = function(e) {
      cat("Error in Ljung-Box test:", e$message, "\n")
      rv$ljung_box_result <- NULL
    })
  })

  # --------------------------------------------------------------------------
  # LJUNG-BOX (UI) INTERPRETATION
  # --------------------------------------------------------------------------
  output$ljung_box_interpretation <- renderUI({
    if (is.null(rv$ljung_box_result)) return(NULL)

    if (rv$ljung_box_result$is_white_noise) {
      create_success_message(
        paste(
          "Residuals are WHITE NOISE (p-value =",
          format(rv$ljung_box_result$p_value, digits = 4), ")"
        )
      )
    } else {
      create_error_message(
        paste(
          "❌ Residuals are NOT WHITE NOISE (p-value =",
          format(rv$ljung_box_result$p_value, digits = 4), ")"
        )
      )
    }
  })

  # --------------------------------------------------------------------------
  # PLOT RESIDUAL OVER TIME
  # --------------------------------------------------------------------------
  output$plot_residuals_ts <- renderPlot({
    if (is.null(rv$fitted_model)) {
      plot.new()
      text(0.5, 0.5, "Model not fitted yet", cex = 1.2, col = "gray50")
      return()
    }

    tryCatch({
      residuals_model <- residuals(rv$fitted_model)

      plot(
        residuals_model,
        main = "Residuals Over Time",
        ylab = "Residuals",
        xlab = "Time",
        type = "l"
      )
      abline(h = 0, col = "red", lty = 2)
      grid()

    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error:", e$message), cex = 1, col = "red")
    })
  })

  # --------------------------------------------------------------------------
  # PLOT ACF RESIDUAL
  # --------------------------------------------------------------------------
  output$plot_residuals_acf <- renderPlot({
    if (is.null(rv$fitted_model)) {
      plot.new()
      text(0.5, 0.5, "Model not fitted yet", cex = 1.2, col = "gray50")
      return()
    }

    tryCatch({
      residuals_model <- residuals(rv$fitted_model)

      acf(
        residuals_model,
        main = "ACF of Residuals",
        xlab = "Lag",
        ylab = "ACF"
      )

    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error:", e$message), cex = 1, col = "red")
    })
  })

  # --------------------------------------------------------------------------
  # Q-Q PLOT RESIDUAL
  # --------------------------------------------------------------------------
  output$plot_residuals_qq <- renderPlot({
    if (is.null(rv$fitted_model)) {
      plot.new()
      text(0.5, 0.5, "Model not fitted yet", cex = 1.2, col = "gray50")
      return()
    }

    tryCatch({
      residuals_model <- residuals(rv$fitted_model)

      qqnorm(
        residuals_model,
        main = "Q-Q Plot",
        xlab = "Theoretical Quantiles",
        ylab = "Sample Quantiles"
      )
      qqline(residuals_model, col = "red", lwd = 2)
      grid()

    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error:", e$message), cex = 1, col = "red")
    })
  })

  # --------------------------------------------------------------------------
  # HISTOGRAM RESIDUAL
  # --------------------------------------------------------------------------
  output$plot_residuals_hist <- renderPlot({
    if (is.null(rv$fitted_model)) {
      plot.new()
      text(0.5, 0.5, "Model not fitted yet", cex = 1.2, col = "gray50")
      return()
    }

    tryCatch({
      residuals_model <- residuals(rv$fitted_model)

      hist(
        residuals_model,
        main = "Histogram of Residuals",
        xlab = "Residuals",
        ylab = "Frequency",
        breaks = 15,
        col = "steelblue",
        border = "white"
      )

    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error:", e$message), cex = 1, col = "red")
    })
  })

  # --------------------------------------------------------------------------
  # MODEL DIAGNOSTICS CONCLUSION
  # --------------------------------------------------------------------------
  output$diagnostics_conclusion <- renderUI({
    if (is.null(rv$fitted_model) || is.null(rv$ljung_box_result)) return(NULL)

    tryCatch({
      if (rv$ljung_box_result$is_white_noise) {
        create_success_message("MODEL VALID - Ready for forecasting!")
      } else {
        create_error_message(
          "⚠️ MODEL NOT VALID. Try adjusting parameters or differencing level."
        )
      }
    }, error = function(e) {
      create_error_message("Error concluding.")
    })
  })
}
