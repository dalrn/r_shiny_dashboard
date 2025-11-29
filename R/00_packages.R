# ==============================================================================
# FILE: R/00_packages.R
# TUJUAN: Load semua packages yang diperlukan
# ==============================================================================
library(shiny)
library(shinyjs)
library(tidyverse)
library(lubridate)
library(readxl)
library(forecast)
library(tseries)
library(lmtest)
library(plotly)
library(ggplot2)
library(DT)
library(scales)
library(glue)
library(stringr)
library(purrr)

cat("Packages loaded at:", format(Sys.time()), "\n")