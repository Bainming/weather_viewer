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
├── v2.R
├── README.md
```

## 🚀 How to Run the App

1. **Open RStudio**.
2. Set the working directory to the root of this folder (or open this folder as an RStudio Project).
3. Run the following command:

```r
shiny::runApp("v2.R")
```

> 📌 Alternatively, open `v2.R` and click the "Run App" button in RStudio.

## 📦 Required Packages

Make sure the following packages are installed:

```r
install.packages(c("shiny", "shinydashboard" "tidyverse", "lubridate", "sf", "broom", "tigris", "ggspatial", "plotly", "leaflet", "DT"))
```

## 📊 Features
- **Dashboard**:
  - Interactive time series plot of selected weather variables
  - Density distribution plot to show value spread
  
- **Temporal Analysis**:
  - Performs linear regression on daily average temperature
  - Displays regression line with annotated slope (Temperature change per day)
  - Includes a concise summary of trends for users

- **Spatial Analysis**:
  - Clusters monitoring sites into 3 groups using k-means based on annual average temperature
  - Visualizes clustered sites on a map
  - Includes a 3D scatter plot of clusters by longitude, latitude, and temperature
  - Provides a searchable and sortable table of site-level data
  
## 📁 Data Files

- `data/sites.rds`: Spatial dataset of monitoring site locations
- `data/complement38.csv`: Site name complement data
- `data/weather.csv`: Hourly weather observations including temperature and humidity
