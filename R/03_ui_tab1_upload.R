# ============================================================================
# TAB 1 UI - UPLOAD AND EXPLORE DATA
# ============================================================================

ui_tab1_upload <- function() {
  fluidPage(
    h2("📁 Upload & Explore Data"),
    
    fluidRow(
      column(
        width = 4,
        wellPanel(
          h4("Upload File (CSV/Excel)"),
          fileInput("file_upload", "Select File:", accept = c(".csv", ".xlsx", ".xls")),
          hr(),
          h5("Select Columns:"),
          selectInput("time_column", "Time (Date/Time):", choices = NULL),
          selectInput("value_column", "Value (Numeric):", choices = NULL),
          hr(),
          h5("Notes:"),
          tags$ul(
            tags$li("Data must have at least 10 observations"),
            tags$li("Date format: YYYY-MM-DD or YYYY-MM"),
            tags$li("Value column must be numeric"),
            tags$li("No missing values allowed")
          )
        )
      ),
      
      column(
        width = 8,
        uiOutput("data_loaded_msg"),
        tabsetPanel(
          tabPanel(
            "📋 Preview Data",
            DT::dataTableOutput("data_preview")
          ),
          tabPanel(
            "📊 Descriptive Statistics",
            verbatimTextOutput("data_summary")
          ),
          tabPanel(
            "📈 Initial Visualization",
            plotlyOutput("plot_initial")
          )
        )
      )
    )
  )
}