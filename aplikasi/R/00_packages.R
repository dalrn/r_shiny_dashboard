# ==============================================================================
# FILE: R/00_packages.R
# TUJUAN: Load semua packages yang diperlukan
# ==============================================================================

required_packages <- c(
  # Shiny framework
  "shiny", "shinyjs",
  
  # Data manipulation
  "tidyverse", "lubridate", "readxl",
  
  # Time series & forecasting
  "forecast", "tseries", "lmtest",
  
  # Visualization
  "plotly", "ggplot2",
  
  # Tables
  "DT",
  
  # Utilities
  "scales", "glue", "stringr", "purrr",

  # Hosting
  "rsconnect"
)

# Function to install & load packages
load_packages <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      cat("📦 Installing", pkg, "...\n")
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }
  cat("✅ All packages loaded successfully!\n\n")
}

# Load packages
load_packages(required_packages)

cat("Packages loaded at:", format(Sys.time()), "\n")