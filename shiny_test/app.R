pacman::p_load(igraph, tidygraph, ggraph, 
               visNetwork, lubridate, clock,forecast,
               tidyverse,graphlayouts, tmap,bslib, shinyalert,terra,gstat,sp,sf,leaflet)


# Read the data
nodes <- read_csv("data/anom_nodes.csv")
nodes1 <- read_csv("data/mc3_shinynodes1.csv")
links <- read_csv("data/mc3_links_new1.csv")
links1 <- read_csv("data/mc3_links_new1.csv")
merged_data <- read_csv("data/merged_data.csv")

#zxy
data <- read_csv("data/weather.csv", na = c("?", "�"))

data <- data %>%
  dplyr::filter(Year >= 2014, Year <= 2023)

colnames(data) <- c(
  'Station', 'Year', 'Month', 'Day', 'DailyRainfall',
  'Highest30minRainfall', 'Highest60minRainfall', 'Highest120minRainfall',
  'MeanTemperature', 'MaxTemperature', 'MinTemperature',
  'MeanWindSpeed', 'MaxWindSpeed'
)

data <- data %>%
  mutate(
    DailyRainfall = as.numeric(DailyRainfall),
    Highest30minRainfall = as.numeric(Highest30minRainfall),
    Highest60minRainfall = as.numeric(Highest60minRainfall),
    Highest120minRainfall = as.numeric(Highest120minRainfall),
    MeanTemperature = as.numeric(MeanTemperature),
    MaxTemperature = as.numeric(MaxTemperature),
    MinTemperature = as.numeric(MinTemperature)
  ) %>%
  suppressWarnings()


# Get the rainfall station dataset
rfstations1 <- read.csv("data/aspatial/RainfallStation.csv")

mpsz2019 <-st_read(dsn = "data/geospatial",layer ="MPSZ-2019") %>%
  st_transform(CRS =3414)


ui <- fluidPage(
  useShinyalert(force = TRUE),
  shinyjs::useShinyjs(),
  titlePanel(title = "SingaClimate Watch: Navigating the Future"),
  navbarPage(
    "@SG",
    id = "tabs",
    tabPanel("Introduction",
             sidebarLayout(
               sidebarPanel = sidebarPanel(
                 radioButtons(
                   inputId = "entity",
                   label = "Select Entity:",
                   choices = c(
                     `Ultimate Beneficial Owner` = "Ultimate Beneficial Owner",
                     `Shareholder` = "Shareholder",
                     `Multi-role Entity` = "Multi-role Entity",
                     `Company Contact` = "Company Contact",
                     `Company` = "Company"
                   ),
                   selected = c("Ultimate Beneficial Owner")
                 ),
                 selectInput(
                   inputId = "revenue",
                   label = "Select Revenue Group:",
                   choices = c(
                     `High` =  "High",
                     `Medium` = "Medium",
                     `Low` = "Low",
                     `Unreported` = "Unreported"
                   ),
                   selected = "Unreported"
                 ),
                 selectInput(
                   inputId = "transboundary",
                   label = "Select Transboundary:",
                   choices = c(
                     `Yes` = "yes",
                     `No` = "no"
                   ),
                   multiple = TRUE,
                   selected = c("yes", "no")
                 )
               ),
               mainPanel = mainPanel(
                 title = "Detecting Anomalies",
                 visNetworkOutput("anomPlot")
               )
             )
    ),
    tabPanel("EDA",
             sidebarLayout(
               sidebarPanel = sidebarPanel(
                 selectInput(
                   inputId = "industry",
                   label = "Select Industry:",
                   choices = c(
                     `All Industries` = "All",
                     `Fishing-related` = "Fishing-related Company",
                     `Industrial` = "Industrial Company",
                     `Food-related` = "Food Company",
                     `Seafood Processing` = "Seafood-processing Company",
                     `Consumer Goods` = "Consumer-goods Company",
                     `Transport & Logistics` = "Transport-logistics Company",
                     `Multi-industry` = "Multi-Industry Company"
                   ),
                   selected = "Fishing-related Company"
                 ),
                 selectInput(
                   inputId = "measure",
                   label = "Select Similarity Measure",
                   choices = c(
                     "Degree Centrality",
                     "Eigenvector_Centrality", "Page_Rank"
                   ),
                   selected = "Degree Centrality"
                 )
               ),
               mainPanel = mainPanel(
                 title = "Industry-based Networks",
                 plotOutput("similarityPlot", height = "800px", width = "800px")
               )
             )
    ),
    tabPanel("Temperature in Singapore",
             sidebarLayout(
               sidebarPanel(
                 selectInput("year", "Select Year:",
                             choices = unique(merged_data$Year)),
                 selectInput("month", "Select Month:",
                             choices = 1:12) # 或者使用月份名称，取决于你的偏好
               ),
               mainPanel(
                 tmapOutput("tempMap")
               )
             )
    ),
    # 添加 Rainfall Analysis 选项卡
    tabPanel("Rainfall in Singapore",
             flowLayout(
               numericInput("year_rain", "Year", value = 2023, min = 2014, max = 2023, step = 1),
               numericInput("month_rain", "Month", value = 1, min = 1, max = 12, step = 1),
             ),
             fluidRow(
               column(6,
                      tmapOutput("map1_plot") %>% shinycssloaders::withSpinner(type = 5)
               ),
               column(6,
                      tmapOutput("map2_plot") %>% shinycssloaders::withSpinner(type = 5)
               ),
             )
    ),
    # 添加 Prediction 选项卡
    tabPanel("Rainfall Prediction",
             flowLayout(
               sliderInput("predict_year", "Predict Year", value = 2024, min = 2024, max = 2026, step = 1, sep = ""),
             ),
             fluidRow(
               column(12,
                      plotOutput("predict_plot") %>% shinycssloaders::withSpinner(type = 5)
               ),
             )
    )
  ),
  theme = bs_theme(bootswatch = "superhero")
)



server <- function(input, output) {

  shinyalert(
    title = "First Time Here?",
    text = "Hi! Welcome to N.E.M.O. Kindly read the user guide before proceeding.",
    size = "l",
    closeOnEsc = TRUE,
    closeOnClickOutside = FALSE,
    html = FALSE,
    type = "info",
    showConfirmButton = TRUE,
    showCancelButton = FALSE,
    confirmButtonText = "I have read the User Guide",
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
  # 添加第二个应用程序的服务器逻辑
  df_year = reactive({
    data %>% dplyr::filter(Year == input$year_rain)
  })

  df_year_month = reactive({
    df_year() %>% dplyr::filter(Month == input$month_rain)
  })

  ### Page 1 output

  output$anomPlot <- renderVisNetwork({

    afilter_nodes <- nodes %>%
      filter(group == input$entity & revenue_group == input$revenue & transboundary == input$transboundary)

    afilter_links <- links %>%
      filter(source %in% afilter_nodes$id | target %in% afilter_nodes$id)

    adistinct_source <- afilter_links %>%
      distinct(source) %>%
      rename("id" = "source")

    adistinct_target <- afilter_links %>%
      distinct(target) %>%
      rename("id" = "target")

    atotal_nodes <- bind_rows(adistinct_source, adistinct_target)
    atotal_links <- afilter_links %>%
      rename("from" = "source",
             "to" = "target")
    # Set node colors based on entity
    atotal_nodes$color <- ifelse(atotal_nodes$id %in% adistinct_target$id, "#F8766D", "#aebbff")
    # Plot network
    visNetwork(
      atotal_nodes,
      atotal_links,
      width = "100%"
    ) %>%
      visIgraphLayout(
        layout = "layout_with_fr"
      ) %>%
      visLegend() %>%
      visGroups(groupname = "Company",
                color = "#aebbff") %>%
      visEdges() %>%
      visOptions(
        # Specify additional Interactive Elements
        highlightNearest = list(enabled = T, degree = 2, hover = T),
        # Add drop-down menu to filter by company name
        nodesIdSelection = TRUE,
        collapse = TRUE
      ) %>%
      visInteraction(navigationButtons = FALSE)
  })

  ### Page 2 Output

  output$similarityPlot <- renderPlot({

    # Filter nodes data from input$industry
    filtered_nodes <- nodes1 %>%
      filter(if (input$industry == "All") TRUE else group == input$industry)

    # Filter links based filtered_nodes to get connections
    filtered_links <- links1 %>%
      filter(source %in% filtered_nodes$id)

    # Get unique source and target
    links_source <- filtered_links %>%
      distinct(source) %>%
      rename("id" = "source")

    links_target <- filtered_links %>%
      distinct(target) %>%
      rename("id" = "target")

    # bind links to get overall nodes dataframe
    filtered_nodes_new <- bind_rows(links_source, links_target) %>%
      left_join(nodes, by = "id") %>%
      select(id, group)

    # Create graph object
    filtered_graph <- tbl_graph(nodes = filtered_nodes_new,
                                edges = filtered_links,
                                directed = FALSE)

    # Calculate all Similarity Measures first
    filtered_graph <- filtered_graph %>%
      activate(nodes) %>%
      mutate(
        degree = degree(filtered_graph, mode = "all"),
        transitivity = transitivity(filtered_graph, type = "global"),
        assortativity = assortativity_degree(filtered_graph, directed = FALSE),
        eigen = eigen_centrality(filtered_graph)$vector,
        closeness = closeness(filtered_graph),
        page_rank = page_rank(filtered_graph)$vector
      )

    set.seed(1234)
    ggraph(filtered_graph,
           layout = "nicely"
    ) +
      geom_edge_fan(
        alpha = .6,
        show.legend = FALSE
      ) +
      scale_edge_width(
        range = c(0.1,4)
      ) +
      geom_node_point(
        aes(size = ifelse(input$measure == "Degree Centrality", degree,
                          ifelse(input$measure == "Eigenvector_Centrality", eigen,
                                 ifelse(input$measure == "Page_Rank", page_rank))),
            color = group),
        alpha = .9
      ) +
      # Remove the legend for "degree"
      guides(color = guide_legend(title = "Role:"),
             size = "none"
      ) +
      geom_node_text(
        aes(label = ifelse(degree > quantile(degree, .75), id, "")),
        size = 2,
        repel = TRUE
      ) +
      theme(
        plot.title = element_text(size = 16,
                                  color = "grey20"),
        legend.title = element_text()
      )
  })

  ### Page 3 output
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
      tm_layout(title = paste(input$year, "Year", month.name[input$month], "Average Temperature in Singapore"))

    tm
  })
  ### Page 4 output

  rfdata_sf1_obj = reactive({
    df = df_year_month()

    rfdata1 <-  df %>%
      group_by(Station, Year, Month) %>%
      dplyr::summarise(MONTHSUM = sum(DailyRainfall, na.rm = TRUE), .groups = "drop") %>%
      ungroup()


    # rfdata1 <- data %>%
    #   dplyr::filter(Year == 2016, Month == 1) %>%
    #   group_by(Station, Year, Month) %>%
    #   dplyr::summarise(MONTHSUM = sum(DailyRainfall, na.rm = TRUE), .groups = "drop") %>%
    #   ungroup()

    rfdata1 <- rfdata1 %>%
      left_join(rfstations1) %>% drop_na()

    rfdata_sf1 <- st_as_sf(rfdata1, coords = c("Longitude", "Latitude"), crs = 4326) %>%
      st_transform(crs = 3414)


  })


  output$map1_plot = renderTmap({

    rfdata_sf1 = rfdata_sf1_obj()
    grid <- terra::rast(mpsz2019, nrows = 690, ncols = 1075)
    xy <- terra::xyFromCell(grid, 1:ncell(grid))
    sf::sf_use_s2(FALSE)
    coop <- st_as_sf(as.data.frame(xy),
                     coords = c("x","y"),
                     crs = st_crs(mpsz2019))
    coop <- st_filter(coop,mpsz2019)
    res <- gstat(formula = MONTHSUM ~ 1,
                 locations = rfdata_sf1,
                 nmax = 15,
                 set = list(idp = 0))

    rfdata_sf_crs1 <- st_crs(rfdata_sf1)

    # print(rfdata_sf_crs1)

    coop <- st_transform(coop, crs = rfdata_sf_crs1)
    resp <- predict(res,coop)
    resp <- st_transform(resp, crs = terra::crs(grid))
    resp$x <- st_coordinates(resp)[,1]
    resp$y <- st_coordinates(resp)[,2]
    resp$pred <- resp$var1.pred

    pred <- terra::rasterize(resp, grid, field = "pred", fun = 'mean')
    #print(terra::values(pred))

    tmap_options(check.and.fix = TRUE)
    tmap_mode("view")
    tm_shape(pred) +
      tm_raster(alpha = 0.6,
                palette = "viridis",
                title = "Total monthly rainfall (mm)") +
      tm_layout(main.title = "Distribution of monthly rainfall",
                main.title.position = "center",
                main.title.size = 1.2,
                legend.height = 0.45,
                legend.width = 0.35,
                frame = TRUE) +
      tm_compass(type="8star", size = 2) +
      tm_scale_bar() +
      tm_grid(alpha =0.2)

  })


  output$map2_plot = renderTmap({


    rfdata_sf1 = rfdata_sf1_obj()

    tmap_options(check.and.fix = TRUE)
    tmap_mode("view")
    tm_shape(mpsz2019)+
      tm_borders()+
      tm_shape(rfdata_sf1)+
      tm_dots(col="MONTHSUM",size = 0.12,
              popup.vars = c("Station" = "Station", "Year" = "Year", "Month" = "Month", "Month Sum" = "MONTHSUM"))


  })

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

    #Plot Time Series Data
    start_year <- start(rain_ts1)[1]
    start_month <- start(rain_ts1)[2]

    dates <- seq(as.Date(paste(start_year, start_month, "01", sep = "-")), by = "month", length.out = length(rain_ts1))

    rain_ts1_df <- data.frame(Date = dates, AvgRainfall = as.vector(rain_ts1))


    hujan_train <- window(rain_ts1, end = c(2023,12))
    # hujan_test <- window(rain_ts1, start = c(2024,1))

    fit1 <- Arima(hujan_train, order=c(1,0,1), seasonal = c(1,0,1))

    forecast_result <- forecast(fit1, h = (input$predict_year - 2023) * 12)

    plot(forecast_result)

  })

}

# Run the application
shinyApp(ui = ui, server = server)
