# ============================================================================
# FILE: R/12_server_parameters.R
# TUJUAN: Server logic untuk parameter selection dan model fitting
# ============================================================================

# PENTING: Gunakan observeEvent (bukan observe) untuk button click
observeEvent(input$fit_model_btn, {
  tryCatch({
    # Validasi: data harus sudah ada
    if (is.null(rv$ts_object)) {
      showNotification("❌ Please upload data and select columns first", type = "error")
      return()
    }
    
    showNotification("⏳ Fitting model...", type = "message")
    
    # KONDISI: AUTO.ARIMA atau MANUAL?
    if (input$param_method == "auto") {
      # ========================================================================
      # AUTO.ARIMA METHOD
      # ========================================================================
      
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
      # ========================================================================
      # MANUAL ARIMA METHOD
      # ========================================================================
      
      # Ambil nilai dari input
      p_val <- input$param_p
      d_val <- input$param_d
      q_val <- input$param_q
      
      # Validasi bahwa p, d, q adalah numeric dan valid
      if (is.na(p_val) || is.na(d_val) || is.na(q_val)) {
        showNotification("❌ p, d, q must be numeric values", type = "error")
        return()
      }
      
      # CEK apakah seasonal model
      if (rv$is_seasonal && input$is_seasonal_manual) {
        # SARIMA (dengan seasonal parameters)
        
        P_val <- input$param_P
        Q_val <- input$param_Q
        D_val <- input$param_D
        m_val <- input$param_m
        
        if (is.na(P_val) || is.na(Q_val) || is.na(D_val) || is.na(m_val)) {
          showNotification("❌ Seasonal parameters (P, Q, D, m) must be numeric", type = "error")
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
        # ARIMA (non-seasonal)
        
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
    
    showNotification(paste("✅ Model fitted successfully:", rv$model_type), type = "message")
    
  }, error = function(e) {
    showNotification(paste("❌ Error fitting model:", e$message), type = "error")
    rv$fitted_model <- NULL
    rv$model_type <- "ARIMA"
  })
})

# Output: Model fit status
output$model_fit_status <- renderUI({
  if (is.null(rv$fitted_model)) {
    create_error_message("No model fitted yet")
  } else {
    create_success_message(paste("Model:", rv$model_type))
  }
})

# Output: Model summary
output$model_summary_output <- renderPrint({
  if (is.null(rv$fitted_model)) {
    cat("No model fitted yet\n")
    cat("\nInstructions:\n")
    cat("1. Upload data in 'Data & Explorasi' tab\n")
    cat("2. Check stationarity in 'Uji Stationaritas' tab\n")
    cat("3. Select method (AUTO.ARIMA or MANUAL)\n")
    cat("4. Click 'Fit Model' button\n")
    return()
  }
  
  summary(rv$fitted_model)
})