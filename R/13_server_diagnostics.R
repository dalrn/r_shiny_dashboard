# ============================================================================
# FILE: R/13_server_diagnostics.R
# TUJUAN: Server logic untuk diagnostic checking
# ============================================================================

server_diagnostics <- function(input, output, session, rv) {

  # --------------------------------------------------------------------------
  # 1. METRIK MODEL (AIC, BIC, RMSE, MAE)
  # --------------------------------------------------------------------------
  output$diagnostics_metrics <- renderPrint({
    if (is.null(rv$fitted_model)) {
      cat("Model belum di-fit\n")
      cat("Harap fit model di tab 'Identifikasi Parameter' dahulu\n")
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
      cat("Error menghitung metrik:", e$message, "\n")
    })
  })

  # --------------------------------------------------------------------------
  # 2. LJUNG-BOX TEST
  # --------------------------------------------------------------------------
  output$ljung_box_output <- renderPrint({
    if (is.null(rv$fitted_model)) {
      cat("Model belum di-fit\n")
      return()
    }

    tryCatch({
      residuals_model <- residuals(rv$fitted_model)

      # Uji Ljung-Box pada residual
      ljung_box_test <- Box.test(
        residuals_model,
        lag  = 10,
        type = "Ljung-Box"
      )

      # Simpan hasil dalam bentuk list agar mudah dipakai di UI lain
      rv$ljung_box_result <- list(
        statistic = ljung_box_test$statistic,
        p_value = ljung_box_test$p.value,
        is_white_noise = ljung_box_test$p.value > 0.05,
        interpretation = if (ljung_box_test$p.value > 0.05)
          "Residual adalah WHITE NOISE (tidak ada autokorelasi)"
        else
          "Residual BUKAN WHITE NOISE (ada autokorelasi)"
      )

      cat("=== LJUNG-BOX TEST ===\n\n")
      cat("Statistik Uji:",
          format_number(rv$ljung_box_result$statistic), "\n")
      cat("P-value:",
          format(rv$ljung_box_result$p_value, digits = 4), "\n\n")
      cat("Kesimpulan:",
          rv$ljung_box_result$interpretation, "\n")

    }, error = function(e) {
      cat("Error dalam Ljung-Box test:", e$message, "\n")
      rv$ljung_box_result <- NULL
    })
  })

  # --------------------------------------------------------------------------
  # INTERPRETASI LJUNG-BOX (UI)
  # --------------------------------------------------------------------------
  output$ljung_box_interpretation <- renderUI({
    if (is.null(rv$ljung_box_result)) return(NULL)

    if (rv$ljung_box_result$is_white_noise) {
      create_success_message(
        paste(
          "Residual adalah WHITE NOISE (p-value =",
          format(rv$ljung_box_result$p_value, digits = 4), ")"
        )
      )
    } else {
      create_error_message(
        paste(
          "❌ Residual BUKAN white noise (p-value =",
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
      text(0.5, 0.5, "Model belum di-fit", cex = 1.2, col = "gray50")
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
      text(0.5, 0.5, "Model belum di-fit", cex = 1.2, col = "gray50")
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
      text(0.5, 0.5, "Model belum di-fit", cex = 1.2, col = "gray50")
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
      text(0.5, 0.5, "Model belum di-fit", cex = 1.2, col = "gray50")
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
  # KESIMPULAN DIAGNOSTIK MODEL
  # --------------------------------------------------------------------------
  output$diagnostics_conclusion <- renderUI({
    if (is.null(rv$fitted_model) || is.null(rv$ljung_box_result)) return(NULL)

    tryCatch({
      if (rv$ljung_box_result$is_white_noise) {
        create_success_message("MODEL VALID - Siap untuk forecast!")
      } else {
        create_error_message(
          "⚠️ MODEL BELUM VALID. Coba sesuaikan parameter atau tingkat differencing."
        )
      }
    }, error = function(e) {
      create_error_message("Error dalam menyimpulkan")
    })
  })
}
