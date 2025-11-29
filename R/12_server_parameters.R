# ============================================================================
# FILE: R/12_server_parameters.R
# TUJUAN: Server logic untuk parameter selection dan model fitting (modul)
# ============================================================================

server_parameters <- function(input, output, session, rv) {

  observeEvent(input$fit_model_btn, {
    tryCatch({
      if (is.null(rv$ts_object)) {
        showNotification("❌ Harap upload data dan pilih kolom terlebih dahulu", type = "error")
        return()
      }

      showNotification("⏳ Fitting model...", type = "message")

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

      } else {

        p_val <- input$param_p
        d_val <- input$param_d
        q_val <- input$param_q

        if (is.na(p_val) || is.na(d_val) || is.na(q_val)) {
          showNotification("❌ p, d, q harus numerik", type = "error")
          return()
        }

        if (rv$is_seasonal && input$is_seasonal_manual) {

          P_val <- input$param_P
          Q_val <- input$param_Q
          D_val <- input$param_D
          m_val <- input$param_m

          if (is.na(P_val) || is.na(Q_val) || is.na(D_val) || is.na(m_val)) {
            showNotification("❌ Parameter seasonal (P, Q, D, m) harus numerik", type = "error")
            return()
          }

          rv$fitted_model <- forecast::arima(
            rv$ts_object,
            order = c(as.integer(p_val), as.integer(d_val), as.integer(q_val)),
            seasonal = list(order = c(as.integer(P_val), as.integer(D_val), as.integer(Q_val)),
                            period = as.integer(m_val))
          )

          rv$model_type <- paste0(
            "SARIMA(",
            as.integer(p_val), ",", as.integer(d_val), ",", as.integer(q_val), ")",
            "(",
            as.integer(P_val), ",", as.integer(D_val), ",", as.integer(Q_val), ",",
            as.integer(m_val), ")"
          )

        } else {

          rv$fitted_model <- forecast::arima(
            rv$ts_object,
            order = c(as.integer(p_val), as.integer(d_val), as.integer(q_val))
          )

          rv$model_type <- paste0(
            "ARIMA(",
            as.integer(p_val), ",", as.integer(d_val), ",", as.integer(q_val), ")"
          )
        }
      }

      showNotification(paste("✅ Fitting model berhasil:", rv$model_type), type = "message")

    }, error = function(e) {
      showNotification(paste("❌ Fitting model error:", e$message), type = "error")
      rv$fitted_model <- NULL
      rv$model_type <- "ARIMA"
    })
  })

  output$model_fit_status <- renderUI({
    if (is.null(rv$fitted_model)) {
      create_error_message("Model belum di-fit")
    } else {
      create_success_message(paste("Model:", rv$model_type))
    }
  })

  output$model_summary_output <- renderPrint({
    if (is.null(rv$fitted_model)) {
      cat("Model belum di-fit\n")
      cat("1. Upload data di tab 'Data & Explorasi'\n")
      cat("2. Cek stasioneritas di tab 'Uji Stasioneritas'\n")
      cat("3. Pilih metode (AUTO.ARIMA atau MANUAL)\n")
      cat("4. Klik 'Fit Model'\n")
      return()
    }

    summary(rv$fitted_model)
  })
}
