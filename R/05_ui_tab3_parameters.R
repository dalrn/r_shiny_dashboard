# ============================================================================
# TAB 3 UI - MODEL PARAMETERS
# ============================================================================

ui_tab3_parameters <- function() {
  fluidPage(
    h2("⚙️ ARIMA Parameters"),
    
    fluidRow(
      column(
        width = 4,
        wellPanel(
          h4("Estimation Method"),
          radioButtons(
            "param_method", 
            NULL,
            choices = list("AUTO.ARIMA" = "auto", "MANUAL INPUT" = "manual"),
            selected = "auto"
          ),
          
          hr(),
          
          h4("Enter Parameters"),
          numericInput("param_p", "p (AR Order):", value = 1, min = 0, max = 5),
          numericInput("param_q", "q (MA Order):", value = 1, min = 0, max = 5),
          numericInput("param_d", "d (Differencing):", value = 1, min = 0, max = 2),
          
          hr(),
          
          h4("Parameter Seasonal"),
          checkboxInput("is_seasonal_manual", "Seasonal Model?", value = FALSE),
          
          conditionalPanel(
            condition = "input.is_seasonal_manual == true",
            numericInput("param_P", "P (Seasonal AR):", value = 1, min = 0, max = 3),
            numericInput("param_Q", "Q (Seasonal MA):", value = 1, min = 0, max = 3),
            numericInput("param_D", "D (Seasonal Diff):", value = 1, min = 0, max = 2),
            numericInput("param_m", "m (Seasonal Period):", value = 12, min = 2, max = 52)
          ),
          
          hr(),
          
          actionButton(
            "fit_model_btn", 
            "🔨 Fit Model", 
            class = "btn-success", 
            width = "100%"
          )
        )
      ),
      
      column(
        width = 8,
        wellPanel(
          h4("Model Status"),
          uiOutput("model_fit_status"),
          
          hr(),
          
          h4("Model Summary"),
          verbatimTextOutput("model_summary_output")
        )
      )
    )
  )
}