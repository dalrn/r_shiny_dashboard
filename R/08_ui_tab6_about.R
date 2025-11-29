# ============================================================================
# FILE: R/08_ui_tab6_about.R
# TUJUAN: UI untuk Tab 6 - About
# ============================================================================

ui_tab6_about <- function() {
  fluidPage(
    h2("ℹ️ Tentang Dashboard"),
    
    fluidRow(
      column(
        width = 12,
        wellPanel(
          h3("📊 TIME SERIES FORECASTING DASHBOARD"),
          p("Dikembangkan untuk memenuhi tugas mata kuliah Komputasi Statistika I."),
          hr(),
          h4("👥 Anggota Kelompok:"),
          tags$ol(
            tags$li("Andalan Raihad Nobelim"),
            tags$li("Fadia Az-Zahra Puteri"),
            tags$li("Faqih Arjiyo"),
            tags$li("Nisrina Rachmi Maulidina"),
            tags$li("Wafi Zhafarina Khasanah")
          )
        )
      )
    )
  )
}
