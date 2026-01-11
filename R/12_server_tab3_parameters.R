# ============================================================================
# TAB 3 SERVER LOGIC - PARAMETER SELECTION AND MODELING
# ============================================================================

server_parameters <- function(input, output, session, rv) {

  # --------------------------------------------------------------------------
  # FIT MODEL
  # --------------------------------------------------------------------------
  observeEvent(input$fit_model_btn, {
    tryCatch({
      # Make sure time series is available
      if (is.null(rv$ts_object)) {
        showNotification(
          "❌ Harap upload data dan pilih kolom terlebih dahulu",
          type = "error"
        )
        return()
      }

      showNotification("⏳ Fitting model...", type = "message")

      # ----------------------------------------------------------------------
      # AUTO MODE: [forecast::auto.arima]
      # ----------------------------------------------------------------------
      if (input$param_method == "auto") {

        rv$fitted_model <- forecast::auto.arima(
          rv$ts_object,
          d = rv$diff_level,
          seasonal = rv$is_seasonal,
          stepwise = TRUE,
          trace = FALSE,
          max.p = 5,
          max.q = 5,
          max.P = 2,
          max.Q = 2
        )

        rv$model_type <- "Auto.ARIMA"

      # ----------------------------------------------------------------------
      # MANUAL MODE: parameters p, d, q (and P, D, Q, m if seasonal)
      # ----------------------------------------------------------------------
      } else {

        # Select non seasonal parameters
        p_val <- input$param_p
        d_val <- input$param_d
        q_val <- input$param_q

        # Must be numeric
        if (is.na(p_val) || is.na(d_val) || is.na(q_val)) {
          showNotification("❌ p, d, q harus numerik", type = "error")
          return()
        }

        # --------------------------------------------------------------------
        # MANUAL + SEASONAL [SARIMA]
        # --------------------------------------------------------------------
        if (rv$is_seasonal && input$is_seasonal_manual) {

          # Select seasonal parameters
          P_val <- input$param_P
          Q_val <- input$param_Q
          D_val <- input$param_D
          m_val <- input$param_m

          if (is.na(P_val) || is.na(Q_val) || is.na(D_val) || is.na(m_val)) {
            showNotification(
              "❌ Parameter seasonal (P, Q, D, m) harus numerik",
              type = "error"
            )
            return()
          }

          # Fit SARIMA Model
          rv$fitted_model <- stats::arima(
            rv$ts_object,
            order = c(
              as.integer(p_val),
              as.integer(d_val),
              as.integer(q_val)
            ),
            seasonal = list(
              order  = c(
                as.integer(P_val),
                as.integer(D_val),
                as.integer(Q_val)
              ),
              period = as.integer(m_val)
            )
          )

          # Simpan label jenis model (SARIMA(p,d,q)(P,D,Q,m))
          rv$model_type <- paste0(
            "SARIMA(",
            as.integer(p_val), ",", as.integer(d_val), ",", as.integer(q_val), ")",
            "(",
            as.integer(P_val), ",", as.integer(D_val), ",", as.integer(Q_val), ",",
            as.integer(m_val), ")"
          )

        # --------------------------------------------------------------------
        # MANUAL NONSEASONAL
        # --------------------------------------------------------------------
        } else {

          rv$fitted_model <- stats::arima(
            rv$ts_object,
            order = c(
              as.integer(p_val),
              as.integer(d_val),
              as.integer(q_val)
            )
          )

          rv$model_type <- paste0(
            "ARIMA(",
            as.integer(p_val), ",", as.integer(d_val), ",", as.integer(q_val), ")"
          )
        }
      }

      # If successful, show success notification
      showNotification(
        paste("✅ Fitting model berhasil:", rv$model_type),
        type = "message"
      )

    }, error = function(e) {
      # If error, show error notification
      showNotification(
        paste("❌ Fitting model error:", e$message),
        type = "error"
      )
      rv$fitted_model <- NULL
      rv$model_type <- "ARIMA"
    })
  })

  # --------------------------------------------------------------------------
  # FITTING MODEL STATUS (UI)
  # --------------------------------------------------------------------------
  output$model_fit_status <- renderUI({
    if (is.null(rv$fitted_model)) {
      create_error_message("Model belum di-fit")
    } else {
      create_success_message(paste("Model:", rv$model_type))
    }
  })

  # --------------------------------------------------------------------------
  # MODEL SUMMARY + ERROR MEASURES
  # --------------------------------------------------------------------------
  output$model_summary_output <- renderPrint({
    if (is.null(rv$fitted_model)) {
      cat("Model belum di-fit\n")
      cat("1. Upload data di tab 'Data & Explorasi'\n")
      cat("2. Cek stasioneritas di tab 'Uji Stasioneritas'\n")
      cat("3. Pilih metode (AUTO.ARIMA atau MANUAL)\n")
      cat("4. Klik 'Fit Model'\n")
      return()
    }

    # Real summary output
    print(summary(rv$fitted_model))

    cat("\nCustom training error measures (dihitung manual):\n")

    # Actual vs fitted values
    y <- as.numeric(rv$ts_object)
    fv <- try(fitted(rv$fitted_model), silent = TRUE)

    if (inherits(fv, "try-error") || length(fv) != length(y)) {
      cat("Tidak bisa menghitung error measures (panjang fitted != data).\n")
      return()
    }

    # Residuals
    e <- y - fv
    me <- mean(e, na.rm = TRUE)
    rmse <- sqrt(mean(e^2, na.rm = TRUE))
    mae <- mean(abs(e), na.rm = TRUE)

    if (any(y == 0 | is.na(y))) {
      mpe <- NA_real_
      mape <- NA_real_
    } else {
      mpe <- mean(100 * e / y, na.rm = TRUE)
      mape <- mean(100 * abs(e / y), na.rm = TRUE)
    }

    # Show metrics
    cat(sprintf("ME   : %.4f\n", me))
    cat(sprintf("RMSE : %.4f\n", rmse))
    cat(sprintf("MAE  : %.4f\n", mae))
    cat(sprintf("MPE  : %s\n",
                ifelse(is.na(mpe),  "NA", sprintf("%.4f", mpe))))
    cat(sprintf("MAPE : %s\n",
                ifelse(is.na(mape), "NA", sprintf("%.4f", mape))))
  })
}
