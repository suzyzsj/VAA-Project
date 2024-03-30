library(gstat)
library(plotly)
pacman::p_load(igraph, tidygraph, ggraph, 
               visNetwork, lubridate, clock,forecast,
               tidyverse,graphlayouts, tmap,bslib, shinyalert,terra,sp,sf,leaflet)

# Read the data

#111
merged_data <- read_csv("data/merged_data.csv")
monthly_data_filtered <- read_csv("data/monthly_data_filtered.csv")
data_with_mean_temp <- read_csv("data/data_with_mean_temp.csv")


#222
data <- read_csv("data/cleaned_data.csv")

rfstations1 <- read.csv("data/rainfall_stations.csv")
# Read shape file
#mpsz2019 <-st_read(dsn = "data/geospatial",layer ="MPSZ-2019") %>%
# st_transform(CRS =3414)

ui <- fluidPage(
  useShinyalert(force = TRUE),
  shinyjs::useShinyjs(),
  titlePanel(title = "SingaClimate Watch: Navigating the Future"),
  navbarPage(
    "@SG",
    id = "tabs",
    
    tabPanel("Temperature in Singapore",
             fluidRow(
               column(6,
                      selectInput("year", "Select Year:",
                                  choices = unique(merged_data$Year))
               ),
               column(6,
                      selectInput("month", "Select Month:",
                                  choices = 1:12) 
               )
             ),
             fluidRow(
               column(6,
                      plotOutput("boxPlot") %>% shinycssloaders::withSpinner(type = 5)
               ),
               column(6,
                      tmapOutput("tempMap") %>% shinycssloaders::withSpinner(type = 5)
               ),
             )
    ),
    tabPanel("Clustering Analysis",
             fluidRow(
               column(6,
                      sliderInput("selectedYear", "Select Year:",
                                  min = min(monthly_data_filtered$Year),
                                  max = max(monthly_data_filtered$Year),
                                  value = max(monthly_data_filtered$Year),
                                  step = 1,
                                  sep = '')
               ),
               column(6,
                      numericInput("selectedK", "Select Number of Clusters (k):",
                                   min = 2, max = 6, value = 3, step = 1)
               )
             ),
             fluidRow(
               column(12,
                      plotOutput("clusterPlot")
               )
             )
    ),
    # Rainfall Analysis
    tabPanel("Rainfall in Singapore",
             fluidRow(
               column(4,
                      selectInput("year_rain", "Select Year:",
                                  choices = unique(data$Year))
               ),
               column(4,
                      selectInput("month_rain", "Select Month:",
                                  choices = 1:12)
               ),
               column(4,
                      selectInput("station_rain", "Select Station:",
                                  choices = unique(data$Station))
               )
             ),
             fluidRow(
               column(6,
                      plotOutput("rainMonthlyPlot")
               ),
               column(6,
                      tmapOutput("rainMap")
               )
             )
    ),
    
    # Prediction
    tabPanel("Rainfall Prediction",
             flowLayout(
               sliderInput("predict_year", "Predict Year", value = 2024, min = 2024, max = 2030, step = 1, sep = ""),
               selectInput("show_forecast", "Show Forecast", choices = c("Yes", "No"), selected = "Yes"),
               selectInput("show_ci", "Show 95% CI", choices = c("Yes", "No"), selected = "Yes")
             ),
             fluidRow(
               column(12,
                      plotOutput("predict_plot")
               )
             )
    )
  ),
  theme = bs_theme(bootswatch = "superhero")
)



server <- function(input, output) {
  
  shinyalert(
    title = "Hi,First Time Here?",
    text = "Welcome to explore the weather. Kindly read the user guide before proceeding.",
    size = "l",
    closeOnEsc = TRUE,
    closeOnClickOutside = FALSE,
    html = FALSE,
    type = "info",
    showConfirmButton = TRUE,
    showCancelButton = FALSE,
    confirmButtonText = "YES. I have read the User Guide",
    confirmButtonCol = "#AEDEF4",
    timer = 0,
    imageUrl = "",
    animation = TRUE
  )
  observe({
    if (input$tabs == "Anomalies") {
      shinyjs::runjs('shinyalert();')
    }
  })
  
  filteredData <- reactive({
    data_filtered <- merged_data[merged_data$Year == as.numeric(input$year), ]
    return(data_filtered)
  })
  
  # 使用reactive表达式创建动态过滤后的数据
  filteredData1 <- reactive({
    # 假设你有一个名为merged_data的数据框，并且其中有一个'Year'列
    merged_data %>%
      filter(Year == input$year) # 使用用户选择的年份进行过滤
  })
  
  ### Page 3 output
  output$boxPlot <- renderPlot({
    filtered_data1 <- merged_data %>%
      filter(Year == input$year) %>%
      group_by(Station, Month) %>%
      summarise(MonthlyAvg = mean(MonthlyAvgTemp, na.rm = TRUE))
    
    p <- ggplot(filtered_data1, aes(x = factor(Month), y = MonthlyAvg, fill = (MonthlyAvg > 27))) +
      geom_boxplot() +
      scale_fill_manual(values = c("FALSE" = "lightblue", "TRUE" = "salmon")) +
      labs(x = "Month", y = "Monthly Average Temperature") +
      theme_minimal()
    
    print(p)
  })
  output$tempMap <- renderTmap({
    # 过滤数据
    selected_data <- merged_data[merged_data$Year == input$year & merged_data$Month == input$month, ]
    
    if(nrow(selected_data) == 0) {
      return(NULL) # 如果没有数据，不渲染地图
    }
    
    selected_sf <- st_as_sf(selected_data, coords = c("Longitude", "Latitude"), crs = 4326, agr = "constant") %>%
      st_transform(crs = 3414)
    
    # 加载地理空间数据
    mpsz2019 <- st_read(dsn = "data/geospatial", layer = "MPSZ-2019") %>%
      st_transform(crs = 3414)
    
    # 检查并修复无效的几何形状
    if(any(!st_is_valid(mpsz2019))) {
      mpsz2019 <- st_make_valid(mpsz2019)
    }
    
    # 创建地图
    pal <- colorNumeric(palette = "YlOrRd", domain = selected_sf$MonthlyAvgTemp)
    tmap_mode("view")
    
    tm <- tm_shape(mpsz2019) +
      tm_borders() +
      tm_shape(selected_sf) +
      tm_dots(col = "MonthlyAvgTemp", palette = "YlOrRd", size = 0.2,
              popup.vars = c("Station" = "Station", "Year" = "Year", "Month" = "Month", "Avg Temp(°C)" = "MonthlyAvgTemp")) +
      tm_layout(title = paste(input$year,"-",input$month, "Average Temperature in Singapore"))
    
    tm
  })
  #page:cluster
  output$clusterPlot <- renderPlot({
    # 根据用户选择的年份过滤数据
    filtered_data <- subset(monthly_data_filtered, Year == input$selectedYear)
    
    # 数据清洗
    clean_data <- na.omit(filtered_data)
    
    # 计算每个站点的平均温度
    station_means <- aggregate(MonthlyAvgTemp ~ Station, clean_data, mean)
    
    # 设置种子以确保可重复性
    set.seed(123)
    
    # 执行k-means聚类,使用用户选择的聚类数
    cluster_result <- kmeans(station_means$MonthlyAvgTemp, centers = input$selectedK)
    
    # 将聚类结果添加到站点平均数据中
    station_means$Cluster <- as.factor(cluster_result$cluster)
    
    # 绘制聚类结果
    ggplot(station_means, aes(x = Station, y = MonthlyAvgTemp, color = Cluster)) +
      geom_point(size = 3) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      labs(title = paste("Clustering Result (k =", input$selectedK, ")"),
           x = "Station", y = "Monthly Average Temperature", color = "Cluster")
  })
  ### Page 4 plot output
  filtered_rain_data_monthly <- reactive({
    data %>%
      filter(Year == input$year_rain,
             Station == input$station_rain) %>%
      group_by(Year, Month) %>%
      summarise(MonthlyRainfall = sum(DailyRainfall, na.rm = TRUE), .groups = "drop")
  })
  
  output$rainMonthlyPlot <- renderPlot({
    ggplot(filtered_rain_data_monthly(), aes(x = factor(Month), y = MonthlyRainfall, fill = MonthlyRainfall)) +
      geom_col() +
      scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Monthly\nRainfall\n(mm)") +
      labs(x = "Month", y = "Monthly Rainfall (mm)") +
      theme_minimal() +
      theme(legend.position = "right", 
            legend.key.height = unit(1, "cm"),
            legend.key.width = unit(0.5, "cm"),
            legend.title = element_text(size = 10, face = "bold"),
            legend.text = element_text(size = 10),
            plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
            axis.title = element_text(size = 12, face = "bold"),
            axis.text = element_text(size = 10)) +
      ggtitle(paste("Monthly Rainfall Distribution for", input$station_rain, "in", input$year_rain))
  })
  
  
  ### Page 4 map output
  filtered_rain_data <- reactive({
    data %>%
      filter(Year == input$year_rain,
             Month == input$month_rain) %>%
      group_by(Station, Year, Month, Longitude, Latitude) %>%
      summarise(MonthlyRainfall = sum(DailyRainfall, na.rm = TRUE), .groups = "drop")
  })
  
  output$rainMap <- renderTmap({
    filtered_data <- filtered_rain_data()
    
    if(nrow(filtered_data) == 0) {
      return(NULL)
    }
    
    filtered_sf <- st_as_sf(filtered_data, coords = c("Longitude", "Latitude"), crs = 4326, agr = "constant") %>%
      st_transform(crs = 3414)
    
    mpsz2019 <- st_read(dsn = "data/geospatial", layer = "MPSZ-2019") %>%
      st_transform(crs = 3414)
    
    if(any(!st_is_valid(mpsz2019))) {
      mpsz2019 <- st_make_valid(mpsz2019)
    }
    
    pal <- colorNumeric(palette = "Blues", domain = filtered_sf$MonthlyRainfall)
    tmap_mode("view")
    
    tm <- tm_shape(mpsz2019) +
      tm_borders() +
      tm_shape(filtered_sf) +
      tm_dots(col = "MonthlyRainfall", palette = "Blues", size = 0.2,
              popup.vars = c("Station", "Year", "Month", "Monthly Rainfall (mm)" = "MonthlyRainfall")) +
      tm_layout(title = paste(input$year_rain, "-", input$month_rain, "Monthly Rainfall in Singapore"))
    
    tm
  })
  
  #time series plot
  output$predict_plot = renderPlot({
    data_filtered <- data %>%
      filter(!is.na(DailyRainfall))
    
    rainfall_sum <- data_filtered %>%
      group_by(Year, Month, Station) %>%
      summarise(Rainfall = sum(DailyRainfall), .groups = "drop")
    
    rainfall_avg <- rainfall_sum %>%
      group_by(Year, Month) %>%
      summarise(AvgRainfall = mean(Rainfall), .groups = "drop")
    
    rain_ts <- rainfall_avg %>%
      ungroup() %>%
      transmute(AvgRainfall) %>%
      ts(start = c(2014, 1), freq = 12)
    
    rain_ts1 <- window(rain_ts, start = c(2014, 1), end = c(2023, 12))
    
    hujan_train <- window(rain_ts1, end = c(2023, 12))
    
    fit1 <- Arima(hujan_train, order = c(1, 0, 1), seasonal = c(1, 0, 1))
    
    forecast_result <- forecast(fit1, h = (input$predict_year - 2023) * 12)
    
    plot(rain_ts1,
         main = "Rainfall Forecast",
         ylab = "Average Rainfall",
         xlab = "Year",
         xlim = c(2014, input$predict_year),
         ylim = c(0, max(rain_ts1, forecast_result$upper[, 2])),
         type = "l",
         col = "black",
         lwd = 2
    )
    
    if (input$show_forecast == "Yes") {
      lines(forecast_result$mean, col = "green", lwd = 2)
    }
    
    if (input$show_ci == "Yes") {
      polygon(c(time(forecast_result$lower), rev(time(forecast_result$upper))),
              c(forecast_result$lower[, 2], rev(forecast_result$upper[, 2])),
              col = "lightskyblue",
              border = NA
      )
    }
    
    legend_items <- c("Observed")
    legend_colors <- c("black")
    legend_lty <- c(1)
    
    if (input$show_forecast == "Yes") {
      legend_items <- c(legend_items, "Forecast")
      legend_colors <- c(legend_colors, "green")
      legend_lty <- c(legend_lty, 1)
    }
    
    if (input$show_ci == "Yes") {
      legend_items <- c(legend_items, "95% CI")
      legend_colors <- c(legend_colors, "lightskyblue")
      legend_lty <- c(legend_lty, 2)
    }
    
    legend("topleft",
           legend = legend_items,
           col = legend_colors,
           lty = legend_lty,
           lwd = 2,
           cex = 0.8
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
