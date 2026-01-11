# ============================================================================
# TAB 2 SERVER LOGIC - STATIONARITY TESTING
# ============================================================================

server_stationarity <- function(input, output, session, rv) {

  # --------------------------------------------------------------------------
  # OBSERVE DIFFERENCING (d) SELECTION
  # --------------------------------------------------------------------------
  observeEvent(input$diff_choice, {
    if (is.null(rv$ts_object)) return()

    tryCatch({
      # Save user's differencing selection
      rv$diff_level <- as.integer(input$diff_choice)

      # Difference data based on selection
      if (rv$diff_level == 0) {
        rv$ts_differenced <- rv$ts_object
      } else if (rv$diff_level == 1) {
        rv$ts_differenced <- diff(rv$ts_object, differences = 1)
      } else if (rv$diff_level == 2) {
        rv$ts_differenced <- diff(rv$ts_object, differences = 2)
      }

    }, error = function(e) {
      showNotification(paste("Differencing ERROR:", e$message), type = "error")
    })
  })

  # --------------------------------------------------------------------------
  # ADF TEST FOR ORIGINAL DATA
  # --------------------------------------------------------------------------
  output$adf_test_original <- renderPrint({
    if (is.null(rv$ts_object)) {
      cat("No dataset uploaded\n")
      cat("Please upload data on the 'Data Exploration' tab first\n")
      return()
    }

    tryCatch({
      # Perform ADF
      rv$adf_result_original <- perform_adf_test(rv$ts_object)

      cat("=== ADF TEST RESULTS (ORIGINAL) ===\n\n")
      cat("Test Statistic:",
          format_number(rv$adf_result_original$statistic), "\n")
      cat("P-value:",
          format(rv$adf_result_original$p_value,
                 scientific = TRUE, digits = 4), "\n")
      cat("Decision:",
          rv$adf_result_original$interpretation, "\n\n")

    }, error = function(e) {
      cat("Error in ADF test:", e$message, "\n")
    })
  })

  # --------------------------------------------------------------------------
  # INTERPRET ORIGINAL ADF (UI)
  # --------------------------------------------------------------------------
  output$adf_interpretation_original <- renderUI({
    if (is.null(rv$adf_result_original)) return(NULL)

    if (rv$adf_result_original$is_stationary) {
      create_success_message(
        paste(
          "Data STATIONARY (p-value =",
          format(rv$adf_result_original$p_value, digits = 4), ")"
        )
      )
    } else {
      create_error_message(
        paste(
          "❌ Data NOT STATIONARY (p-value =",
          format(rv$adf_result_original$p_value, digits = 4), ")"
        )
      )
    }
  })

  # --------------------------------------------------------------------------
  # ADF TEST FOR DIFFERENCED DATA
  # --------------------------------------------------------------------------
  output$adf_test_differenced <- renderPrint({
    if (is.null(rv$ts_differenced)) {
      cat("Please select differencing level (d) above\n")
      return()
    }

    tryCatch({
      # Perform ADF
      rv$adf_result_differenced <- perform_adf_test(rv$ts_differenced)

      cat("=== ADF TEST RESULTS (d =", rv$diff_level, ") ===\n\n")
      cat("Test Statistic:",
          format_number(rv$adf_result_differenced$statistic), "\n")
      cat("P-value:",
          format(rv$adf_result_differenced$p_value,
                 scientific = TRUE, digits = 4), "\n")
      cat("Decision:",
          rv$adf_result_differenced$interpretation, "\n\n")

    }, error = function(e) {
      cat("Error in ADF test:", e$message, "\n")
    })
  })

  # --------------------------------------------------------------------------
  # INTERPRET DIFFERENCED ADF (UI)
  # --------------------------------------------------------------------------
  output$adf_interpretation_differenced <- renderUI({
    if (is.null(rv$adf_result_differenced)) return(NULL)

    if (rv$adf_result_differenced$is_stationary) {
      create_success_message(
        paste(
          "STATIONARY (p-value =",
          format(rv$adf_result_differenced$p_value, digits = 4), ")"
        )
      )
    } else {
      create_error_message(
        paste(
          "❌ NOT STATIONARY (p-value =",
          format(rv$adf_result_differenced$p_value, digits = 4), ")"
        )
      )
    }
  })

  # --------------------------------------------------------------------------
  # PLOT ACF FOR DIFFERENCED DATA
  # --------------------------------------------------------------------------
  output$plot_acf <- renderPlot({
    if (is.null(rv$ts_differenced)) {
      plot.new()
      text(
        0.5, 0.5,
        "Please select differencing level (d) to display ACF plot",
        cex = 1.2, col = "gray50", adj = c(0.5, 0.5)
      )
      return()
    }

    tryCatch({
      # at least 2 observations
      if (length(rv$ts_differenced) < 2) {
        plot.new()
        text(
          0.5, 0.5,
          "Not enough data for ACF plot",
          cex = 1.2, col = "red", adj = c(0.5, 0.5)
        )
        return()
      }

      acf(
        rv$ts_differenced,
        main = paste("ACF - d =", rv$diff_level),
        xlab = "Lag",
        ylab = "ACF"
      )

    }, error = function(e) {
      plot.new()
      text(
        0.5, 0.5,
        paste("Error:", e$message),
        cex = 1, col = "red", adj = c(0.5, 0.5)
      )
    })
  })

  # --------------------------------------------------------------------------
  # PLOT PACF FOR DIFFERENCED DATA
  # --------------------------------------------------------------------------
  output$plot_pacf <- renderPlot({
    if (is.null(rv$ts_differenced)) {
      plot.new()
      text(
        0.5, 0.5,
        "Please select differencing level (d) to display PACF plot",
        cex = 1.2, col = "gray50", adj = c(0.5, 0.5)
      )
      return()
    }

    tryCatch({
      # at least 2 observations
      if (length(rv$ts_differenced) < 2) {
        plot.new()
        text(
          0.5, 0.5,
          "Not enough data for PACF plot",
          cex = 1.2, col = "red", adj = c(0.5, 0.5)
        )
        return()
      }

      pacf(
        rv$ts_differenced,
        main = paste("PACF - d =", rv$diff_level),
        xlab = "Lag",
        ylab = "PACF"
      )

    }, error = function(e) {
      plot.new()
      text(
        0.5, 0.5,
        paste("Error:", e$message),
        cex = 1, col = "red", adj = c(0.5, 0.5)
      )
    })
  })
}
