# 🌤 Daily Weather Viewer (Shiny App)

This Shiny application allows users to explore, visualize, and analyze daily weather data (temperature and humidity) across various air quality monitoring sites for the year 2024.

## 📁 Directory Structure

```
SYSEN-5460-Project-main/
├── data/
│   ├── sites.rds
│   ├── complement38.csv
│   └── weather.csv
├── v1.R
├── README.md
```

## 🚀 How to Run the App

1. **Open RStudio**.
2. Set the working directory to the root of this folder (or open this folder as an RStudio Project).
3. Run the following command:

```r
shiny::runApp("v1.R")
```

> 📌 Alternatively, open `v1.R` and click the "Run App" button in RStudio.

## 📦 Required Packages

Make sure the following packages are installed:

```r
install.packages(c("shiny", "tidyverse", "lubridate", "sf", "broom", "plotly"))
```

## 📊 Features

- **Date Range Selector**: Filter the data by a specific date range.
- **Site Selection**: Choose from available monitoring sites (e.g., Manhattan Bridge).
- **Variable Toggle**: View either temperature or humidity metrics.
- **Summary Statistics**: View average, min, and max of selected variable.
- **Interactive Plots**:
  - Temporal analysis plot (`plotly`)
  - Distribution density plot (`plotly`)
- **Trend Analysis**:
  - Linear regression on daily average temperature
  - Slope (β), R², and p-value reported
  - Regression line with annotation on plot

## 📁 Data Files

- `data/sites.rds`: Spatial dataset of monitoring site locations
- `data/complement38.csv`: Site name complement data
- `data/weather.csv`: Hourly weather observations including temperature and humidity

## 📝 Notes

- The app automatically filters data to include only the year **2024**.
- At least **3 days** of data are required to run the trend analysis.
- Missing values (`NA`) in temperature or humidity are excluded from analysis.