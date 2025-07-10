library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(sf)
library(broom)
library(tigris)
library(ggspatial)
library(plotly)
library(leaflet)
library(DT)

# --- Load & Prepare Data ---
sites_sf <- readRDS("data/sites.rds")
sites_complement <- read_csv("data/complement38.csv", show_col_types = FALSE)

site_data_base <- st_drop_geometry(sites_sf) |>
  left_join(sites_complement, by = "aqs_id_full") |>
  mutate(site_name = coalesce(site_name.x, site_name.y)) |>
  select(-site_name.x, -site_name.y)

site_data <- cbind(site_data_base, st_coordinates(sites_sf))

weather_df <- read_csv(
  "data/weather.csv",
  show_col_types = FALSE,
  col_types = cols(datetime = col_character())
) |>
  mutate(datetime = parse_date_time(
    datetime,
    orders = c("ymd HMS z", "ymd HM z", "ymd H z", "ymd"),
    tz = "UTC"
  )) |>
  filter(
    datetime >= as_datetime("2024-01-01", tz = "UTC"),
    datetime < as_datetime("2025-01-01", tz = "UTC"),!is.na(temp),
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

all_sites <- weather_df |>
  group_by(aqs_id_full, site_name, X, Y) |>
  summarise(
    avg_temp = mean(temp, na.rm = TRUE),
    max_temp = max(temp, na.rm = TRUE),
    min_temp = min(temp, na.rm = TRUE),
    .groups = "drop"
  )

all_counties <- counties(cb = TRUE, resolution = "20m")

site_hull <- sites_sf |>
  st_union() |>
  st_convex_hull()

site_hull <- st_transform(site_hull, st_crs(all_counties))

ny_boundary <- all_counties |>
  filter(as.vector(st_intersects(geometry, site_hull, sparse = FALSE)))

ref_site <- filter(all_sites, site_name == "Manhattan Bridge")

all_sites$distance_to_ref <- as.numeric(st_distance(
  st_as_sf(
    all_sites,
    coords = c("X", "Y"),
    crs = st_crs(sites_sf)
  ),
  st_as_sf(
    ref_site,
    coords = c("X", "Y"),
    crs = st_crs(sites_sf)
  )
)) / 1000

sites_sf <- st_as_sf(site_data,
                     coords = c("X", "Y"),
                     crs = st_crs(ny_boundary))
ny_sites <- st_join(sites_sf, ny_boundary, left = FALSE)

set.seed(100)

# ---- UI ----
ui <- dashboardPage(
  dashboardHeader(title = "Weather Analytics Dashboard"),
  dashboardSidebar(sidebarMenu(
    id = "tabs",
    menuItem(
      "Dashboard",
      tabName = "dashboard",
      icon = icon("dashboard")
    ),
    menuItem(
      "Temporal Analysis",
      tabName = "trend",
      icon = icon("chart-line")
    ),
    menuItem(
      "Spatial Analysis",
      tabName = "spatial",
      icon = icon("map")
    )
  )),
  dashboardBody(tabItems(
    tabItem(
      tabName = "dashboard",
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
      fluidRow(
        box(
          width = 4,
          dateRangeInput(
            "date_range_dashboard",
            "Date Range",
            start = min(weather_df$date),
            end = min(weather_df$date) + 2,
            min = min(weather_df$date),
            max = max(weather_df$date)
          )
        ),
        box(
          width = 4,
          selectInput(
            "site_select_dashboard",
            "Select Site",
            choices = unique(ny_sites$site_name),
            selected = "Manhattan Bridge"
          )
        ),
        box(
          width = 4,
          radioButtons(
            "variable_dashboard",
            "Weather Variable",
            choices = c("Temperature" = "temp", "Humidity" = "humidity"),
            selected = "temp"
          )
        )
      ),
      fluidRow(
        valueBoxOutput("mean_temp"),
        valueBoxOutput("max_temp"),
        valueBoxOutput("min_temp")
      ),
      fluidRow(box(plotlyOutput("ts_plot"), width = 12)),
      fluidRow(box(plotlyOutput("dist_plot"), width = 12))
    ),
    
    tabItem(
      tabName = "trend",
      fluidRow(box(
        width = 6,
        dateRangeInput(
          "date_range_trend",
          "Date Range",
          start = min(weather_df$date),
          end = min(weather_df$date) + 2,
          min = min(weather_df$date),
          max = max(weather_df$date)
        )
      ), box(
        width = 6,
        selectInput(
          "site_select_trend",
          "Select Site",
          choices = unique(ny_sites$site_name),
          selected = "Manhattan Bridge"
        )
      )),
      fluidRow(
        box(plotOutput("trend_plot"), width = 8),
        valueBoxOutput("beta_value"),
        valueBoxOutput("beta_ci")
      ),
      fluidRow(
        box(
          width = 12,
          title = "Model Interpretation",
          solidHeader = TRUE,
          status = "info",
          tags$div(style = "font-size:18px; line-height:1.6;", textOutput("model_summary"))
        )
      )
    ),
    
    tabItem(
      tabName = "spatial",
      fluidRow(box(
        width = 12,
        selectInput(
          "spatial_site_select",
          "Selected Site(s)",
          choices = unique(ny_sites$site_name),
          selected = unique(ny_sites$site_name),
          multiple = TRUE
        )
      )),
      fluidRow(
        box(
          width = 6,
          title = "Cluster Distribution",
          status = "info",
          solidHeader = TRUE,
          icon = icon("project-diagram"),
          verbatimTextOutput("cluster_summary_text"),
          style = "font-size: 14px; padding: 10px;"
        ),
        box(
          width = 6,
          title = "Cluster Avg Temperatures",
          status = "primary",
          solidHeader = TRUE,
          icon = icon("thermometer-half"),
          verbatimTextOutput("cluster_avg_temp_text"),
          style = "font-size: 14px; padding: 10px;"
        )
      ),
      fluidRow(
        box(
          leafletOutput("interactive_map"),
          width = 6,
          title = "Site Locations and Clusters",
          solidHeader = TRUE,
          status = "success"
        ),
        box(
          plotlyOutput("spatial_plot"),
          width = 6,
          title = "3D Cluster Distribution",
          solidHeader = TRUE,
          status = "warning"
        )
      ),
      fluidRow(
        box(
          DTOutput("spatial_table"),
          width = 12,
          title = "Site Table",
          solidHeader = TRUE,
          status = "info"
        )
      )
    )
  ))
)

# --- SERVER ---
server <- function(input, output, session) {
  cluster_colors <- c("1" = "skyblue",
                      "2" = "orange",
                      "3" = "red")
  
  # === Dashboard Logic ===
  filtered_data <- reactive({
    req(
      input$date_range_dashboard,
      input$site_select_dashboard,
      input$variable_dashboard
    )
    weather_df |>
      filter(
        date >= input$date_range_dashboard[1],
        date <= input$date_range_dashboard[2],
        site_name == input$site_select_dashboard
      )
  })
  
  output$total_observations <- renderText({
    paste("Total observations:", nrow(filtered_data()))
  })
  
  output$current_selection <- renderText({
    paste(
      "Currently viewing:",
      input$site_select_dashboard,
      "from",
      input$date_range_dashboard[1],
      "to",
      input$date_range_dashboard[2]
    )
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
      labs(
        title = paste("Hourly", str_to_title(var)),
        x = "Time",
        y = str_to_title(var)
      ) +
      theme_minimal() +
      scale_color_gradientn(colors = c("lightblue", "skyblue", "orange", "red"),
                            name = "Temperature (°F)")
    ggplotly(p)
  })
  
  output$dist_plot <- renderPlotly({
    data <- filtered_data()
    var <- input$variable_dashboard
    p <- ggplot(data, aes(x = .data[[var]])) +
      geom_density(fill = "skyblue", alpha = 0.5) +
      labs(
        title = paste("Distribution of", str_to_title(var)),
        x = str_to_title(var),
        y = "Density"
      ) +
      theme_minimal()
    ggplotly(p)
  })
  
  # === Temporal Logic ===
  trend_data <- reactive({
    req(input$date_range_trend, input$site_select_trend)
    daily_weather |>
      filter(
        date >= input$date_range_trend[1],
        date <= input$date_range_trend[2],
        site_name == input$site_select_trend
      )
  })
  
  reactive_model <- reactive({
    req(nrow(trend_data()) > 2)
    lm(avg_temp ~ as.numeric(date), data = trend_data())
  })
  
  reactive_summary <- reactive({
    tidy(reactive_model(), conf.int = TRUE) |>
      filter(term == "as.numeric(date)") |>
      mutate(daily_change = estimate)
  })
  
  output$trend_plot <- renderPlot({
    summ <- reactive_summary()
    data <- trend_data()
    ggplot(data, aes(x = date, y = avg_temp)) +
      geom_point(alpha = 0.5) +
      geom_smooth(
        method = "lm",
        color = "steelblue",
        formula = y ~ x
      ) +
      annotate(
        "text",
        x = mean(data$date),
        y = max(data$avg_temp),
        label = paste0("β = ", round(summ$daily_change, 4), " °F/day"),
        size = 5,
        color = "red"
      ) +
      labs(title = "Daily Temperature Trend", x = "Date", y = "Average Temperature (°F)") +
      theme_minimal()
  })
  
  output$beta_value <- renderValueBox({
    summ <- reactive_summary()
    valueBox(round(summ$daily_change, 4),
             "Daily Change (°F/day)",
             color = "green")
  })
  
  output$beta_ci <- renderValueBox({
    summ <- reactive_summary()
    ci <- paste0("[",
                 round(summ$conf.low, 4),
                 ", ",
                 round(summ$conf.high, 4),
                 "]")
    valueBox(ci, "95% Confidence Interval", color = "light-blue")
  })
  
  output$model_summary <- renderText({
    model <- reactive_model()
    glance_df <- glance(model)
    summ <- reactive_summary()
    
    direction <- ifelse(summ$daily_change > 0, "increasing", "decreasing")
    rate <- round(summ$daily_change, 4)
    pval <- glance_df$p.value
    
    if (pval < 0.05) {
      signif_text <- "This change is statistically significant."
    } else {
      signif_text <- "However, this change is not statistically significant."
    }
    
    paste0(
      "Between ",
      input$date_range_trend[1],
      " and ",
      input$date_range_trend[2],
      ", the average daily temperature at ",
      input$site_select_trend,
      " has been ",
      direction,
      " at a rate of ",
      rate,
      " °F per day. ",
      signif_text
    )
  })
  
  # === Spatial Logic ===
  reactive_clusters <- reactive({
    req(input$spatial_site_select)
    
    selected <- all_sites |>
      filter(site_name %in% input$spatial_site_select)
    
    if (nrow(selected) < 3) {
      selected$cluster <- factor(rep(1, nrow(selected)))
    } else {
      set.seed(100)
      k_result <- kmeans(selected[, c("avg_temp")], centers = 3)
      selected$cluster <- as.factor(k_result$cluster)
      
      cluster_avg_temp <- selected |>
        group_by(cluster) |>
        summarise(mean_temp = mean(avg_temp)) |>
        arrange(mean_temp) |>
        mutate(new_cluster = factor(1:3))
      
      selected <- selected |>
        left_join(cluster_avg_temp, by = "cluster") |>
        mutate(cluster = new_cluster) |>
        select(-mean_temp)
    }
    
    selected
  })
  
  spatial_metrics <- reactive({
    req(input$spatial_site_select)
    reactive_clusters() |>
      filter(site_name %in% input$spatial_site_select) |>
      summarise(
        avg_distance = round(mean(distance_to_ref, na.rm = TRUE), 2),
        min_temp = min(avg_temp, na.rm = TRUE),
        max_temp = max(avg_temp, na.rm = TRUE)
      )
  })
  
  output$interactive_map <- renderLeaflet({
    sites <- reactive_clusters()
    sites <- st_as_sf(sites,
                      coords = c("X", "Y"),
                      crs = st_crs(sites_sf))
    pal <- colorFactor(cluster_colors, levels = names(cluster_colors))
    ny_bbox <- st_bbox(ny_sites)
    
    leaflet() |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addPolygons(
        data = ny_boundary,
        fillColor = "transparent",
        color = "#444444",
        weight = 2
      ) |>
      addCircleMarkers(
        data = sites,
        radius = 8,
        color = ~ pal(cluster),
        stroke = FALSE,
        fillOpacity = 0.8,
        label = ~ site_name,
        popup = ~ paste0(
          "<b>",
          site_name,
          "</b><br>",
          "Avg Temp: ",
          round(avg_temp, 1),
          "°F<br>",
          "Distance: ",
          round(distance_to_ref, 1),
          " km"
        )
      ) |>
      addLegend(
        position = "bottomright",
        pal = pal,
        values = sites$cluster,
        title = "Temperature Clusters"
      ) |>
      fitBounds(ny_bbox[["xmin"]], ny_bbox[["ymin"]], ny_bbox[["xmax"]], ny_bbox[["ymax"]])
  })
  
  output$spatial_plot <- renderPlotly({
    sites <- reactive_clusters()
    req(nrow(sites) > 0)
    
    plot_ly(
      data = sites,
      x = ~ X,
      y = ~ Y,
      z = ~ avg_temp,
      type = "scatter3d",
      mode = "markers",
      color = ~ cluster,
      colors = cluster_colors,
      marker = list(size = 6)
    ) |>
      layout(title = "3D Temperature Cluster View",
             scene = list(
               xaxis = list(title = "Longitude"),
               yaxis = list(title = "Latitude"),
               zaxis = list(title = "Average Temp (°F)")
             ))
  })
  
  
  output$spatial_table <- renderDT({
    reactive_clusters() |>
      select(site_name, avg_temp, cluster) |>
      mutate(avg_temp = round(avg_temp, 1))
  }, options = list(pageLength = 5), colnames = c("Site Name", "Avg Temperature (°F)", "Cluster"))
  
  
  
  output$cluster_summary_text <- renderText({
    sites <- reactive_clusters()
    
    if (nrow(sites) == 0)
      return("No sites selected.")
    
    cluster_counts <- sites |>
      count(cluster) |>
      arrange(cluster)
    
    summary_text <- paste0(
      "Selected sites by Cluster:\n",
      paste0(
        "Cluster ",
        cluster_counts$cluster,
        ": ",
        cluster_counts$n,
        " site(s)",
        collapse = "\n"
      )
    )
    return(summary_text)
  })
  
  
  output$cluster_avg_temp_text <- renderText({
    sites <- reactive_clusters()
    
    if (nrow(sites) == 0)
      return("No sites selected.")
    
    avg_temp_by_cluster <- sites |>
      group_by(cluster) |>
      summarise(avg_temp = round(mean(avg_temp, na.rm = TRUE), 2), .groups = "drop")
    
    summary_text <- paste0(
      "Average Temp by Cluster:\n",
      paste0(
        "Cluster ",
        avg_temp_by_cluster$cluster,
        ": ",
        avg_temp_by_cluster$avg_temp,
        " °F",
        collapse = "\n"
      )
    )
    
    return(summary_text)
  })
  
}

# --- Run the App ---
shinyApp(ui, server)
