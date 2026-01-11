# Time Series Forecasting Dashboard

An interactive Shiny dashboard for time series analysis, ARIMA modeling, diagnostics, and forecasting of univariate time series data.  

---

## Overview

The **Time Series Forecasting Dashboard** provides a workflow for analyzing time series data, from initial exploration to model diagnostics and forecasting. The application is designed for educational and analytical purposes, focusing on classical time series methods.

---

## Features

- Data upload and exploration
- Time series visualization
- Stationarity testing (ADF, KPSS)
- ARIMA model parameter selection
- Model diagnostics (residual analysis)
- Forecasting and visualization of future values

---

## Data Requirements

The dashboard expects a dataset with **at least two columns**:

| Column | Description |
|------|------------|
| `date` | Date column (`YYYY-MM-DD` or `YYYY-MM`) |
| `value` | Numeric time series values |

Notes:
- The data should represent a univariate time series
- Missing values should be handled prior to analysis
- Regular time intervals (daily, monthly, etc.) are recommended

---

## Methods Used

This dashboard implements classical time series techniques, including:

- Stationarity tests (Augmented Dickey-Fuller, KPSS)
- ARIMA and SARIMA modeling
- Residual diagnostics
- Point forecasting

All analyses are performed using established R packages.

---

## Run the App

1. Clone the repository
2. Open the project folder in R or VS Code
3. Install required packages on `R/00_packages.R`
4. Run the app: `shiny::runApp()`

## Limitations

- The dashboard focuses on univariate time series only
- External regressors, structural breaks, and regime changes are not considered

### Developed by Andalan, Fadia, Faqih, Ninis, and Wafi.



