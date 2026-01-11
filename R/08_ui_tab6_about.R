# ============================================================================
# TAB 6 UI - ABOUT 
# ============================================================================

ui_tab6_about <- function() {
  fluidPage(
    h2("ℹ️ About Dashboard"),
    
    fluidRow(
      column(
        width = 12,
        wellPanel(
          h3("📊 TIME SERIES FORECASTING DASHBOARD"),
          p("Developed to fulfill the requirements of the Statistical Computing I course."),
          hr(),
          h4("👥 Developed by:"),
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
