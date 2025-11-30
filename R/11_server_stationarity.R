# ============================================================================
# FILE: R/11_server_stationarity.R
# TUJUAN: Server logic untuk stationarity testing
# ============================================================================

server_stationarity <- function(input, output, session, rv) {

  # --------------------------------------------------------------------------
  # OBSERVE PILIHAN DIFFERENCING (d)
  # --------------------------------------------------------------------------
  observeEvent(input$diff_choice, {
    if (is.null(rv$ts_object)) return()

    tryCatch({
      # Simpan level differencing yang dipilih pengguna
      rv$diff_level <- as.integer(input$diff_choice)

      # Bentuk data yang sudah di-differencing sesuai level d
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
  # ADF TEST UNTUK DATA ORIGINAL
  # --------------------------------------------------------------------------
  output$adf_test_original <- renderPrint({
    if (is.null(rv$ts_object)) {
      cat("Belum ada data yang di-upload\n")
      cat("Harap upload data pada tab 'Data & Eksplorasi' dahulu\n")
      return()
    }

    tryCatch({
      # ADF pada data asli
      rv$adf_result_original <- perform_adf_test(rv$ts_object)

      cat("=== ADF TEST RESULTS (ORIGINAL) ===\n\n")
      cat("Statistik Uji:",
          format_number(rv$adf_result_original$statistic), "\n")
      cat("P-value:",
          format(rv$adf_result_original$p_value,
                 scientific = TRUE, digits = 4), "\n")
      cat("Kesimpulan:",
          rv$adf_result_original$interpretation, "\n\n")

    }, error = function(e) {
      cat("Error dalam ADF test:", e$message, "\n")
    })
  })

  # --------------------------------------------------------------------------
  # INTERPRETASI ADF ORIGINAL (UI)
  # --------------------------------------------------------------------------
  output$adf_interpretation_original <- renderUI({
    if (is.null(rv$adf_result_original)) return(NULL)

    if (rv$adf_result_original$is_stationary) {
      create_success_message(
        paste(
          "Data STASIONER (p-value =",
          format(rv$adf_result_original$p_value, digits = 4), ")"
        )
      )
    } else {
      create_error_message(
        paste(
          "❌ Data TIDAK STASIONER (p-value =",
          format(rv$adf_result_original$p_value, digits = 4), ")"
        )
      )
    }
  })

  # --------------------------------------------------------------------------
  # ADF TEST UNTUK DATA YANG SUDAH DI-DIFFERENCING
  # --------------------------------------------------------------------------
  output$adf_test_differenced <- renderPrint({
    if (is.null(rv$ts_differenced)) {
      cat("Pilih tingkatan differencing (d) di atas\n")
      return()
    }

    tryCatch({
      # ADF pada data hasil differencing
      rv$adf_result_differenced <- perform_adf_test(rv$ts_differenced)

      cat("=== ADF TEST RESULTS (d =", rv$diff_level, ") ===\n\n")
      cat("Statistik Uji:",
          format_number(rv$adf_result_differenced$statistic), "\n")
      cat("P-value:",
          format(rv$adf_result_differenced$p_value,
                 scientific = TRUE, digits = 4), "\n")
      cat("Kesimpulan:",
          rv$adf_result_differenced$interpretation, "\n\n")

    }, error = function(e) {
      cat("Error dalam ADF test:", e$message, "\n")
    })
  })

  # --------------------------------------------------------------------------
  # INTERPRETASI ADF DIFFERENCED (UI)
  # --------------------------------------------------------------------------
  output$adf_interpretation_differenced <- renderUI({
    if (is.null(rv$adf_result_differenced)) return(NULL)

    if (rv$adf_result_differenced$is_stationary) {
      create_success_message(
        paste(
          "STASIONER (p-value =",
          format(rv$adf_result_differenced$p_value, digits = 4), ")"
        )
      )
    } else {
      create_error_message(
        paste(
          "❌ TIDAK STASIONER (p-value =",
          format(rv$adf_result_differenced$p_value, digits = 4), ")"
        )
      )
    }
  })

  # --------------------------------------------------------------------------
  # PLOT ACF UNTUK DATA YANG DI-DIFFERENCING
  # --------------------------------------------------------------------------
  output$plot_acf <- renderPlot({
    if (is.null(rv$ts_differenced)) {
      plot.new()
      text(
        0.5, 0.5,
        "Pilih tingkat differencing untuk menampilkan ACF plot",
        cex = 1.2, col = "gray50", adj = c(0.5, 0.5)
      )
      return()
    }

    tryCatch({
      # Minimal 2 observasi
      if (length(rv$ts_differenced) < 2) {
        plot.new()
        text(
          0.5, 0.5,
          "Tidak cukup data untuk ACF plot",
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
  # PLOT PACF UNTUK DATA YANG DI-DIFFERENCING
  # --------------------------------------------------------------------------
  output$plot_pacf <- renderPlot({
    if (is.null(rv$ts_differenced)) {
      plot.new()
      text(
        0.5, 0.5,
        "Pilih tingkat differencing untuk menampilkan PACF plot",
        cex = 1.2, col = "gray50", adj = c(0.5, 0.5)
      )
      return()
    }

    tryCatch({
      # Minimal 2 observasi
      if (length(rv$ts_differenced) < 2) {
        plot.new()
        text(
          0.5, 0.5,
          "Tidak cukup data untuk PACF plot",
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
