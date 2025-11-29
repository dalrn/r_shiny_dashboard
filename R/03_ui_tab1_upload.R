# ============================================================================
# FILE: R/03_ui_tab1_upload.R
# TUJUAN: UI untuk Tab 1 - Upload & Explorasi Data
# ============================================================================

ui_tab1_upload <- function() {
  fluidPage(
    h2("📁 Upload Data & Explorasi"),
    
    fluidRow(
      column(
        width = 4,
        wellPanel(
          h4("Upload File (CSV/Excel)"),
          fileInput("file_upload", "Pilih File:", accept = c(".csv", ".xlsx", ".xls")),
          hr(),
          h5("Pilih Kolom:"),
          selectInput("time_column", "Waktu (Date/Time):", choices = NULL),
          selectInput("value_column", "Nilai (Numeric):", choices = NULL),
          hr(),
          h5("Catatan:"),
          tags$ul(
            tags$li("Minimal 10 observasi"),
            tags$li("Format tanggal: YYYY-MM-DD atau YYYY-MM"),
            tags$li("Kolom nilai harus numerik"),
            tags$li("Tidak boleh ada missing values")
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
            "📊 Statistik Deskriptif",
            verbatimTextOutput("data_summary")
          ),
          tabPanel(
            "🔍 Deteksi Otomatis",
            verbatimTextOutput("auto_detection_info")
          ),
          tabPanel(
            "📈 Visualisasi Awal",
            plotlyOutput("plot_initial")
          )
        )
      )
    )
  )
}