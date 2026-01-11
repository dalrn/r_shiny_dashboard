# ============================================================================
# TAB 5 SERVER LOGIC - FORECASTING
# ============================================================================

server_forecast <- function(input, output, session, rv) {

  # --------------------------------------------------------------------------
  # GENERATE FORECAST
  # --------------------------------------------------------------------------
  observeEvent(input$generate_forecast_btn, {
    tryCatch({
      # Check if model is fitted
      if (is.null(rv$fitted_model)) {
        showNotification(
          "❌ Please fit model in 'Model Parameters' tab first",
          type = "error"
        )
        return()
      }

      # Prediction horizon (forecast to how many future periods)
      h <- as.integer(input$forecast_horizon)
      if (is.na(h) || h < 1) {
        showNotification(
          "❌ Minimum forecast horizon is 1",
          type = "error"
        )
        return()
      }

      # Confidence interval level
      ci_level <- as.integer(input$forecast_ci) / 100
      if (is.na(ci_level) || ci_level < 0.5 || ci_level > 0.99) {
        showNotification(
          "❌ Confidence level must be between 50-99%",
          type = "error"
        )
        return()
      }

      showNotification("⏳ Forecasting...", type = "message")

      # ----------------------------------------------------------------------
      # Call forecast function and create result table
      # ----------------------------------------------------------------------
      tryCatch({
        rv$forecast_result <- forecast::forecast(
          rv$fitted_model,
          h = h,
          level = ci_level * 100
        )

        rv$forecast_table <- data.frame(
          Period = seq_len(h),
          Forecast = as.numeric(rv$forecast_result$mean),
          Lower_CI = as.numeric(rv$forecast_result$lower[, 1]),
          Upper_CI = as.numeric(rv$forecast_result$upper[, 1]),
          stringsAsFactors = FALSE
        )

        showNotification("✅ Forecast successful!", type = "message")

      }, error = function(e) {
        showNotification(
          paste("❌ Error in forecasting:", e$message),
          type = "error"
        )
        rv$forecast_result <- NULL
        rv$forecast_table  <- NULL
      })

    }, error = function(e) {
      showNotification(paste("❌ Error:", e$message), type = "error")
    })
  })

  # --------------------------------------------------------------------------
  # PLOT FORECAST
  # --------------------------------------------------------------------------
  output$plot_forecast <- renderPlotly({
    # If no forecast result, show message
    if (is.null(rv$forecast_result)) {
      return(
        plotly::plot_ly() %>%
          plotly::add_text(
            x = 0.5, y = 0.5,
            text = "Perform forecast first",
            textposition = "center",
            showlegend = FALSE
          ) %>%
          plotly::layout(
            title = "Forecast Visualization",
            xaxis = list(title = "Time"),
            yaxis = list(title = "Value"),
            hovermode = "x unified"
          )
      )
    }

    tryCatch({
      h <- nrow(rv$forecast_table)

      # Numeric index for historical and forecast points
      hist_points <- seq_along(rv$value_col)
      fc_points   <- seq_along(rv$value_col)[length(rv$value_col)] + seq_len(h)

      # Plot: historical + forecast + band CI
      p <- plotly::plot_ly() %>%
        plotly::add_trace(
          x = hist_points,
          y = rv$value_col,
          name = "Historical",
          mode = "lines",
          line = list(color = "#2E86AB")
        ) %>%
        plotly::add_trace(
          x = fc_points,
          y = rv$forecast_result$mean,
          name = "Forecast",
          mode = "lines+markers",
          line = list(color = "#06A77D")
        ) %>%
        # Lower CI
        plotly::add_trace(
          x = fc_points,
          y = rv$forecast_result$lower[, 1],
          name = "Lower CI",
          mode = "lines",
          line = list(color = "rgba(100,100,100,0)", width = 0),
          hoverinfo  = "skip",
          showlegend  = FALSE
        ) %>%
        # Upper CI
        plotly::add_trace(
          x = fc_points,
          y = rv$forecast_result$upper[, 1],
          name = "Upper CI",
          mode = "lines",
          line = list(color = "rgba(100,100,100,0)", width = 0),
          fill = "tonexty",
          fillcolor = "rgba(0,200,100,0.2)",
          hoverinfo = "skip",
          showlegend = FALSE
        ) %>%
        plotly::layout(
          title = paste("Forecast -", input$forecast_ci, "% Interval Konfidensi"),
          xaxis = list(title = "Time"),
          yaxis = list(title = "Value"),
          hovermode = "x unified"
        )

      p

    }, error = function(e) {
      # If error, show message
      plotly::plot_ly() %>%
        plotly::add_text(
          x = 0.5,
          y = 0.5,
          text = paste("Error:", e$message),
          textposition = "center",
          showlegend  = FALSE
        ) %>%
        plotly::layout(
          title = "Forecast Visualization Error",
          xaxis = list(title = "Time"),
          yaxis = list(title = "Value")
        )
    })
  })

  # --------------------------------------------------------------------------
  # FORECAST RESULTS TABLE
  # --------------------------------------------------------------------------
  output$forecast_table <- DT::renderDataTable({
    if (is.null(rv$forecast_table)) {
      return(
        DT::datatable(
          data.frame(Message = "Forecast not yet performed."),
          options = list(dom = "t")
        )
      )
    }

    DT::datatable(
      rv$forecast_table,
      options = list(
        pageLength = 12,
        scrollX = TRUE,
        lengthMenu = list(
          c(10, 20, 50, -1),
          c("10", "20", "50", "All")
        )
      ),
      rownames = FALSE
    )
  })

  # --------------------------------------------------------------------------
  # DOWNLOAD FORECAST AS CSV
  # --------------------------------------------------------------------------
  output$download_forecast_csv <- downloadHandler(
    filename = function() {
      paste0("forecast_",
             format(Sys.time(), "%Y%m%d_%H%M%S"),
             ".csv")
    },
    content = function(file) {
      if (is.null(rv$forecast_table)) {
        showNotification("❌ Forecast not yet performed.", type = "error")
        return()
      }

      tryCatch({
        write.csv(rv$forecast_table, file, row.names = FALSE)
        showNotification("✅ Forecast saved successfully!", type = "message")
      }, error = function(e) {
        showNotification(paste("❌ Error:", e$message), type = "error")
      })
    }
  )
}
