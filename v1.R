library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(sf)
library(broom)
library(ggridges)
library(plotly)

# Load site data
sites_sf <- readRDS("data/sites.rds")
sites_complement <- read_csv("data/complement38.csv", show_col_types = FALSE)

site_data <- st_drop_geometry(sites_sf)
site_data <- site_data |>
  left_join(sites_complement, by = "aqs_id_full") |>
  mutate(site_name = coalesce(site_name.x, site_name.y)) |>
  select(-site_name.x, -site_name.y)

# Load weather data
weather_df <- read_csv("data/weather.csv", 
                       show_col_types = FALSE,
                       col_types = cols(datetime = col_character()))

# Process weather data
weather_df <- weather_df |>
  mutate(
    datetime = parse_date_time(
      datetime,
      orders = c("ymd HMS z", "ymd HM z", "ymd H z", "ymd"),
      tz = "UTC"
    )
  ) |>
  filter(
    datetime >= as_datetime("2024-01-01 00:00:00", tz = "UTC"),
    datetime < as_datetime("2025-01-01 00:00:00", tz = "UTC"),
    !is.na(temp),
    !is.na(humidity)
  ) |>
  left_join(site_data, by = "aqs_id_full") |>
  mutate(
    month = month(datetime, label = TRUE, abbr = FALSE),
    hour = hour(datetime),
    date = as_date(datetime)
  ) 

daily_weather <- weather_df |>
  group_by(aqs_id_full, date, site_name) |>
  summarise(
    avg_temp = mean(temp, na.rm = TRUE),
    max_temp = max(temp, na.rm = TRUE),
    min_temp = min(temp, na.rm = TRUE),
    avg_humidity = mean(humidity, na.rm = TRUE),
    .groups = "drop"
  )

# Shiny UI
ui <- dashboardPage(
  dashboardHeader(title = "Weather Monitoring Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Temperature Trend Analysis", tabName = "trend", icon = icon("chart-line"))
    ),
    
    # Date range input
    dateRangeInput("date_range", "Date Range",
                   start = min(weather_df$date),
                   end = min(weather_df$date) + 2,
                   min = min(weather_df$date),
                   max = max(weather_df$date)),
    
    # Site selection
    selectInput("site_select", "Select Sites", 
                choices = unique(weather_df$site_name), 
                multiple = FALSE,
                selected = "Manhattan Bridge"),
    
    # Variable selection
    radioButtons("variable", "Weather Variable",
                 choices = c("Temperature" = "temp",
                             "Humidity" = "humidity"),
                 selected = "temp")
  ),
  dashboardBody(
    # Adding card-style layout for the reactive texts
    fluidRow(
      box(
        width = 12,
        title = "Current Data Info",
        solidHeader = TRUE,
        status = "primary",
        style = "font-size: 20px; text-align: center;",
        textOutput("total_observations"),
        textOutput("current_selection")
      )
    ),
    tabItems(
      # Dashboard tab
      tabItem(tabName = "dashboard",
              fluidRow(
                valueBoxOutput("mean_temp"),
                valueBoxOutput("max_temp"),
                valueBoxOutput("min_temp")
              ),
              fluidRow(
                box(plotlyOutput("ts_plot"), width = 12)
              ),
              fluidRow(
                box(plotlyOutput("dist_plot"), width = 12),
              )
      ),
      
      # Trend analysis tab
      tabItem(tabName = "trend",
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


# Shiny server
server <- function(input, output) {
  
  filtered_data <- reactive({
    req(input$date_range, input$site_select, input$variable)
    
    weather_df |>
      filter(date >= input$date_range[1],
             date <= input$date_range[2],
             site_name %in% input$site_select)
  })
  
  
  trend_data <- reactive({
    req(input$site_select, input$date_range)
    
    daily_weather |>
      filter(date >= input$date_range[1],
             date <= input$date_range[2],
             site_name %in% input$site_select)
  })
  
  reactive_model <- reactive({
    req(trend_data(), nrow(trend_data()) > 2)
    
    lm(avg_temp ~ as.numeric(date), data = trend_data())
  })
  
  reactive_summary <- reactive({
    model <- reactive_model()
    tidy_model <- tidy(model, conf.int = TRUE) |> 
      filter(term == "as.numeric(date)") |>
      mutate(daily_change = estimate * 1)
    tidy_model
  })
  
  # Value boxes
  output$mean_temp <- renderValueBox({
    data <- filtered_data()
    valueBox(
      value = round(mean(data$temp, na.rm = TRUE), 1),
      subtitle = "Average Temperature (°F)",
      color = "blue"
    )
  })
  
  output$max_temp <- renderValueBox({
    data <- filtered_data()
    valueBox(
      value = round(max(data$temp, na.rm = TRUE), 1),
      subtitle = "Maximum Temperature (°F)",
      color = "red"
    )
  })
  
  output$min_temp <- renderValueBox({
    data <- filtered_data()
    valueBox(
      value = round(min(data$temp, na.rm = TRUE), 1),
      subtitle = "Minimum Temperature (°F)",
      color = "aqua"
    )
  })
  
  # Time series plot
  output$ts_plot <- renderPlotly({
    data <- filtered_data()
    var <- input$variable
    p <- ggplot(data, aes(x = datetime, y = .data[[var]])) +
      geom_point(aes(color = .data[[var]]), alpha = 0.4) +
      geom_line(color = "steelblue", alpha = 0.6) +
      labs(
        title = paste("Hourly", str_to_title(var)),
        x = "Time",
        y = str_to_title(var)
      ) + 
      theme_minimal() +
      scale_color_gradientn(
        colors = c("lightblue", "skyblue", "orange", "red"),
        name = "Temperature (°F)"
      )
    ggplotly(p)
  })
  
  # Distribution plot
  output$dist_plot <- renderPlotly({
    data <- filtered_data()
    var_name <- input$variable
    
    p <- ggplot(data, aes(x = .data[[var_name]], fill = site_name)) +
      geom_density(alpha = 0.5) +
      labs(title = paste("Distribution of", str_to_title(input$variable)),
           x = str_to_title(input$variable), y = "Density") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Trend plot
  output$trend_plot <- renderPlot({
    summ <- reactive_summary()
    
    req(trend_data())
    filtered_data <- trend_data()
    
    ggplot(filtered_data, aes(x = date, y = avg_temp)) +
      geom_point(alpha = 0.5) +
      geom_smooth(method = "lm", color = "steelblue", formula = y ~ x) +
      labs(title = "Daily Temperature Trend",
           x = "Date", y = "Average Temperature (°F)") +
      theme_minimal() +
      annotate("text", x = mean(filtered_data$date), y = max(filtered_data$avg_temp),
               label = paste0("β = ", round(summ$daily_change, 4), " °F/day"),
               size = 5, color = "red")
  })
  
  # Beta value box
  output$beta_value <- renderValueBox({
    summ <- reactive_summary()
    valueBox(
      value = round(summ$daily_change, 4),
      subtitle = "Daily Change (°F/day)",
      color = "green"
    )
  })
  
  # Confidence interval value box
  output$beta_ci <- renderValueBox({
    summ <- reactive_summary()
    ci <- paste0("[", round(summ$conf.low, 4), ", ", 
                 round(summ$conf.high, 4), "]")
    
    valueBox(
      value = ci,
      subtitle = "95% Confidence Interval",
      color = "light-blue"
    )
  })
  
  # Model summary
  output$model_summary <- renderPrint({
    req(reactive_model())
    model <- reactive_model()
    
    cat("Daily Temperature Trend Model\n")
    cat("================================================\n")
    cat("Date Range:", format(input$date_range[1]), "to", format(input$date_range[2]), "\n")
    cat("Selected Sites:", paste(input$site_select, collapse = ", "), "\n\n")
    
    cat("Model Formula:\n")
    cat("Daily Temperature = β₀ + β₁ * Time\n\n")
    
    cat("Coefficients:\n")
    print(tidy(model))
    
    cat("\nModel Significance:\n")
    glance_df <- glance(model)
    cat("R-squared:", round(glance_df$r.squared, 4), "\n")
    cat("Adjusted R-squared:", round(glance_df$adj.r.squared, 4), "\n")
    cat("F-statistic:", round(glance_df$statistic, 2), 
        "on", glance_df$df, "DF\n")
    cat("p-value:", glance_df$p.value, "\n")
    
    cat("\nInterpretation:\n")
    if(glance_df$p.value < 0.05) {
      cat("Statistically significant trend detected (p < 0.05)\n")
    } else {
      cat("No significant trend detected (p ≥ 0.05)\n")
    }
    cat("Daily temperature change: β₁ =", round(coef(model)[2], 6), "°F per day\n")
  })
  
  # Total number of observations
  output$total_observations <- renderText({
    data <- filtered_data()
    paste("Total observations: ", nrow(data))
  })
  
  # Current selected data
  output$current_selection <- renderText({
    paste("Currently viewing: ", input$site_select, 
          " from ", input$date_range[1], " to ", input$date_range[2])
  })
}

shinyApp(ui, server)
