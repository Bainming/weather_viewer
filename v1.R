# ---- Revised Shiny App Code with Separated Inputs ----
library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(sf)
library(broom)
library(ggridges)
library(plotly)

# Load and process data
sites_sf <- readRDS("data/sites.rds")
sites_complement <- read_csv("data/complement38.csv", show_col_types = FALSE)

site_data <- st_drop_geometry(sites_sf) |> 
  left_join(sites_complement, by = "aqs_id_full") |> 
  mutate(site_name = coalesce(site_name.x, site_name.y)) |> 
  select(-site_name.x, -site_name.y)

weather_df <- read_csv("data/weather.csv", 
                       show_col_types = FALSE,
                       col_types = cols(datetime = col_character())) |> 
  mutate(datetime = parse_date_time(datetime, orders = c("ymd HMS z", "ymd HM z", "ymd H z", "ymd"), tz = "UTC")) |> 
  filter(datetime >= as_datetime("2024-01-01 00:00:00", tz = "UTC"),
         datetime < as_datetime("2025-01-01 00:00:00", tz = "UTC"),
         !is.na(temp), !is.na(humidity)) |> 
  left_join(site_data, by = "aqs_id_full") |> 
  mutate(month = month(datetime, label = TRUE, abbr = FALSE),
         hour = hour(datetime),
         date = as_date(datetime))

daily_weather <- weather_df |> 
  group_by(aqs_id_full, date, site_name) |> 
  summarise(avg_temp = mean(temp, na.rm = TRUE),
            max_temp = max(temp, na.rm = TRUE),
            min_temp = min(temp, na.rm = TRUE),
            avg_humidity = mean(humidity, na.rm = TRUE), .groups = "drop")

# ---- UI ----
ui <- dashboardPage(
  dashboardHeader(title = "Weather Monitoring Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Temperature Trend Analysis", tabName = "trend", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    fluidRow(
      box(width = 12, title = "Current Data Info", solidHeader = TRUE, status = "primary",
          style = "font-size: 20px; text-align: center;",
          textOutput("total_observations"), textOutput("current_selection"))
    ),
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                box(width = 4, dateRangeInput("date_range_dashboard", "Date Range", 
                                              start = min(weather_df$date), end = min(weather_df$date)+2, 
                                              min = min(weather_df$date), max = max(weather_df$date))),
                box(width = 4, selectInput("site_select_dashboard", "Select Site", 
                                           choices = unique(weather_df$site_name), selected = "Manhattan Bridge")),
                box(width = 4, radioButtons("variable_dashboard", "Weather Variable", 
                                            choices = c("Temperature" = "temp", "Humidity" = "humidity"), 
                                            selected = "temp"))
              ),
              fluidRow(
                valueBoxOutput("mean_temp"),
                valueBoxOutput("max_temp"),
                valueBoxOutput("min_temp")
              ),
              fluidRow(box(plotlyOutput("ts_plot"), width = 12)),
              fluidRow(box(plotlyOutput("dist_plot"), width = 12))
      ),
      tabItem(tabName = "trend",
              fluidRow(
                box(width = 6, dateRangeInput("date_range_trend", "Date Range",
                                              start = min(weather_df$date), end = min(weather_df$date)+2,
                                              min = min(weather_df$date), max = max(weather_df$date))),
                box(width = 6, selectInput("site_select_trend", "Select Site", 
                                           choices = unique(weather_df$site_name), selected = "Manhattan Bridge"))
              ),
              fluidRow(
                box(plotOutput("trend_plot"), width = 8),
                valueBoxOutput("beta_value"),
                valueBoxOutput("beta_ci")
              ),
              fluidRow(
                box(verbatimTextOutput("model_summary"), width = 12)
              )
      )
    )
  )
)

# ---- SERVER ----
server <- function(input, output) {
  
  filtered_data <- reactive({
    req(input$date_range_dashboard, input$site_select_dashboard, input$variable_dashboard)
    weather_df |> filter(date >= input$date_range_dashboard[1],
                         date <= input$date_range_dashboard[2],
                         site_name %in% input$site_select_dashboard)
  })
  
  trend_data <- reactive({
    req(input$site_select_trend, input$date_range_trend)
    daily_weather |> filter(date >= input$date_range_trend[1],
                            date <= input$date_range_trend[2],
                            site_name %in% input$site_select_trend)
  })
  
  reactive_model <- reactive({
    req(trend_data(), nrow(trend_data()) > 2)
    lm(avg_temp ~ as.numeric(date), data = trend_data())
  })
  
  reactive_summary <- reactive({
    tidy(reactive_model(), conf.int = TRUE) |> 
      filter(term == "as.numeric(date)") |> 
      mutate(daily_change = estimate * 1)
  })
  
  output$mean_temp <- renderValueBox({
    valueBox(round(mean(filtered_data()$temp, na.rm = TRUE), 1), "Average Temperature (°F)", color = "blue")
  })
  output$max_temp <- renderValueBox({
    valueBox(round(max(filtered_data()$temp, na.rm = TRUE), 1), "Maximum Temperature (°F)", color = "red")
  })
  output$min_temp <- renderValueBox({
    valueBox(round(min(filtered_data()$temp, na.rm = TRUE), 1), "Minimum Temperature (°F)", color = "aqua")
  })
  
  output$ts_plot <- renderPlotly({
    var <- input$variable_dashboard
    p <- ggplot(filtered_data(), aes(x = datetime, y = .data[[var]])) +
      geom_point(aes(color = .data[[var]]), alpha = 0.4) +
      geom_line(color = "steelblue", alpha = 0.6) +
      labs(title = paste("Hourly", str_to_title(var)), x = "Time", y = str_to_title(var)) +
      theme_minimal() +
      scale_color_gradientn(colors = c("lightblue", "skyblue", "orange", "red"), name = "Temperature (°F)")
    ggplotly(p)
  })
  
  output$dist_plot <- renderPlotly({
    var_name <- input$variable_dashboard
    p <- ggplot(filtered_data(), aes(x = .data[[var_name]], fill = site_name)) +
      geom_density(alpha = 0.5) +
      labs(title = paste("Distribution of", str_to_title(var_name)), x = str_to_title(var_name), y = "Density") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$trend_plot <- renderPlot({
    summ <- reactive_summary()
    ggplot(trend_data(), aes(x = date, y = avg_temp)) +
      geom_point(alpha = 0.5) +
      geom_smooth(method = "lm", color = "steelblue", formula = y ~ x) +
      labs(title = "Daily Temperature Trend", x = "Date", y = "Average Temperature (°F)") +
      theme_minimal() +
      annotate("text", x = mean(trend_data()$date), y = max(trend_data()$avg_temp),
               label = paste0("β = ", round(summ$daily_change, 4), " °F/day"),
               size = 5, color = "red")
  })
  
  output$beta_value <- renderValueBox({
    summ <- reactive_summary()
    valueBox(round(summ$daily_change, 4), "Daily Change (°F/day)", color = "green")
  })
  output$beta_ci <- renderValueBox({
    summ <- reactive_summary()
    ci <- paste0("[", round(summ$conf.low, 4), ", ", round(summ$conf.high, 4), "]")
    valueBox(ci, "95% Confidence Interval", color = "light-blue")
  })
  
  output$model_summary <- renderPrint({
    model <- reactive_model()
    glance_df <- glance(model)
    cat("Daily Temperature Trend Model\n",
        "================================================\n",
        "Date Range:", input$date_range_trend[1], "to", input$date_range_trend[2], "\n",
        "Selected Site:", input$site_select_trend, "\n\n",
        "Model Formula:\nDaily Temperature = β₀ + β₁ * Time\n\n",
        "Coefficients:\n")
    print(tidy(model))
    cat("\nModel Significance:\n",
        "R-squared:", round(glance_df$r.squared, 4), "\n",
        "Adjusted R-squared:", round(glance_df$adj.r.squared, 4), "\n",
        "F-statistic:", round(glance_df$statistic, 2), "on", glance_df$df, "DF\n",
        "p-value:", glance_df$p.value, "\n\n")
    if (glance_df$p.value < 0.05) cat("Statistically significant trend detected (p < 0.05)\n")
    else cat("No significant trend detected (p ≥ 0.05)\n")
    cat("Daily temperature change: β₁ =", round(coef(model)[2], 6), "°F per day\n")
  })
  
  output$total_observations <- renderText({
    paste("Total observations:", nrow(filtered_data()))
  })
  
  output$current_selection <- renderText({
    paste("Currently viewing:", input$site_select_dashboard, 
          "from", input$date_range_dashboard[1], "to", input$date_range_dashboard[2])
  })
}

shinyApp(ui, server)
