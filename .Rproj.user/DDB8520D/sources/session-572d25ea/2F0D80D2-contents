library(shiny)
library(tidyverse)
library(lubridate)
library(sf)
library(broom)
library(plotly)

# Load site data
sites_sf <- readRDS("data/sites.rds")
sites_complement <- read_csv("data/complement38.csv", show_col_types = FALSE)

site_data <- st_drop_geometry(sites_sf) |>
  left_join(sites_complement, by = "aqs_id_full") |>
  mutate(site_name = coalesce(site_name.x, site_name.y)) |>
  select(-site_name.x, -site_name.y)

# Load and process weather data
weather_df <- read_csv("data/weather.csv", show_col_types = FALSE,
                       col_types = cols(datetime = col_character())) |>
  mutate(
    datetime = parse_date_time(datetime,
                               orders = c("ymd HMS z", "ymd HM z", "ymd H z", "ymd"),
                               tz = "UTC"),
    date = as_date(datetime),
    month = month(datetime, label = TRUE, abbr = FALSE),
    hour = hour(datetime)
  ) |>
  filter(
    datetime >= as_datetime("2024-01-01 00:00:00", tz = "UTC"),
    datetime < as_datetime("2025-01-01 00:00:00", tz = "UTC"),
    !is.na(temp),
    !is.na(humidity)
  ) |>
  left_join(site_data, by = "aqs_id_full")

daily_weather <- weather_df |>
  group_by(aqs_id_full, date, site_name) |>
  summarise(
    avg_temp = mean(temp, na.rm = TRUE),
    max_temp = max(temp, na.rm = TRUE),
    min_temp = min(temp, na.rm = TRUE),
    avg_humidity = mean(humidity, na.rm = TRUE),
    .groups = "drop"
  )

# --- UI ---
ui <- fluidPage(
  titlePanel("Daily Weather Viewer"),
  
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("date_range", "Date Range",
                     start = min(weather_df$date),
                     end = max(weather_df$date),
                     min = min(weather_df$date),
                     max = max(weather_df$date)),
      selectInput("site_select", "Select Site",
                  choices = unique(weather_df$site_name),
                  selected = "Manhattan Bridge",
                  multiple = FALSE),
      radioButtons("variable", "Variable",
                   choices = c("Temperature" = "temp", "Humidity" = "humidity"),
                   selected = "temp")
    ),
    
    mainPanel(
      h4("Summary Stats"),
      verbatimTextOutput("summary_text"),
      plotlyOutput("ts_plot"),
      plotlyOutput("dist_plot"),
      
      h4("Trend Analysis"),
      plotOutput("trend_plot"),
      verbatimTextOutput("model_summary")
    )
  )
)

# --- Server ---
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
    if (nrow(trend_data()) < 3) return(NULL)
    model <- reactive_model()
    tidy(model, conf.int = TRUE) |> 
      filter(term == "as.numeric(date)") |>
      mutate(daily_change = estimate * 1)
  })
  
  output$summary_text <- renderPrint({
    data <- filtered_data()
    cat("Avg:", round(mean(data$temp, na.rm = TRUE), 1),
        " Max:", round(max(data$temp, na.rm = TRUE), 1),
        " Min:", round(min(data$temp, na.rm = TRUE), 1), "\n")
  })
  
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
  
  output$dist_plot <- renderPlotly({
    data <- filtered_data()
    var <- input$variable
    p <- ggplot(data, aes(x = .data[[var]])) +
      geom_density(fill = "lightblue", alpha = 0.6) +
      labs(title = paste("Distribution of", str_to_title(var)), x = str_to_title(var), y = "Density") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$trend_plot <- renderPlot({
    data <- trend_data()
    
    if (nrow(data) < 3) {
      plot.new()
      text(0.5, 0.5, "At least 3 days of data are required for trend analysis.", cex = 1.2)
      return()
    }
    
    summ <- reactive_summary()
    
    ggplot(data, aes(x = date, y = avg_temp)) +
      geom_point(alpha = 0.5) +
      geom_smooth(method = "lm", se = TRUE, color = "black") +
      labs(title = "Trend of Daily Temperature", x = "Date", y = "Average Temp") +
      annotate("text", x = mean(data$date), y = max(data$avg_temp),
               label = paste0("β = ", round(summ$daily_change, 4), " °F/day"),
               color = "red", size = 4)
  })
  
  output$model_summary <- renderPrint({
    data <- trend_data()
    
    if (nrow(data) < 3) {
      cat("Trend analysis requires at least 3 days of data.\n")
      return()
    }
    
    model <- reactive_model()
    glance_df <- glance(model)
    cat("R2:", round(glance_df$r.squared, 4), ", p:", round(glance_df$p.value, 4), "\n")
    print(tidy(model))
  })
}

# Run
shinyApp(ui = ui, server = server)
