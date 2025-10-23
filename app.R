# ==============================================================================
# GEOM90007 Assignment 3: Melbourne Tourism Explorer
# 墨尔本游客探索应用
# ==============================================================================
# 基于墨尔本开放数据的交互式旅游指南
# 数据来源：City of Melbourne Open Data Portal (CC BY 4.0)
# ==============================================================================

# 加载必需的库
library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(stringr)
library(htmltools)
library(shinyjs)
library(DT)

# 加载附近设施分析函数
source("scripts/nearby_facilities_simple.R")

# 加载繁忙时段分析函数
source("scripts/busy_hours_analysis.R")

# 加载Tableau集成库
source("tableau-in-shiny-v1.2.R")

# ==============================================================================
# 数据加载
# ==============================================================================

load_data <- function() {
  if (!dir.exists("data/processed")) {
    stop("错误：找不到 data/processed 文件夹！\n请先运行: source('scripts/prepare_data.R')")
  }

  # 加载所有处理后的数据
  data_list <- list(
    cafes = readRDS("data/processed/cafes_restaurants.rds"),
    bars = readRDS("data/processed/bars_pubs.rds"),
    pois = readRDS("data/processed/melbourne_pois.rds"),
    artworks = readRDS("data/processed/public_artworks.rds"),
    toilets = readRDS("data/processed/public_toilets.rds"),
    fountains = readRDS("data/processed/drinking_fountains.rds"),
    pedestrian = readRDS("data/processed/pedestrian_summary.rds"),
    overview = readRDS("data/processed/overview_stats.rds")
  )

  return(data_list)
}

# ==============================================================================
# 用户界面
# ==============================================================================

ui <- dashboardPage(
  skin = "blue",

  # 页面标题
  dashboardHeader(
    title = "墨尔本探索",
    titleWidth = 280
  ),

  # 侧边栏
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      id = "sidebar",
      menuItem("欢迎", tabName = "welcome", icon = icon("home")),
      menuItem("探索地图", tabName = "map", icon = icon("map-marked-alt")),
      menuItem("文化景点", tabName = "attractions", icon = icon("palette")),
      menuItem("餐饮美食", tabName = "dining", icon = icon("utensils")),
      menuItem("酒吧夜生活", tabName = "bars", icon = icon("beer")),
      menuItem("人流分析", tabName = "pedestrian", icon = icon("users"))
    ),

    # 地图筛选器
    conditionalPanel(
      condition = "input.sidebar == 'map'",
      hr(),
      h4("地图图层", style = "padding-left: 15px;"),
      checkboxGroupInput("map_layers", NULL,
                        choices = c("文化景点" = "pois",
                                  "咖啡馆/餐厅" = "cafes",
                                  "酒吧/酒馆" = "bars",
                                  "公共厕所" = "toilets",
                                  "饮水喷泉" = "fountains"),
                        selected = c("pois", "cafes"))
    ),

    # 景点筛选器
    conditionalPanel(
      condition = "input.sidebar == 'attractions'",
      hr(),
      h4("筛选选项", style = "padding-left: 15px;"),
      checkboxGroupInput("attraction_types", "显示类型:",
                        choices = c("艺术品" = "Art",
                                  "喷泉" = "Fountain",
                                  "纪念碑" = "Monument",
                                  "雕塑" = "Memorial"),
                        selected = c("Art", "Fountain", "Monument", "Memorial"))
    )
  ),
  #   # 餐饮筛选器
  #   conditionalPanel(
  #     condition = "input.sidebar == 'dining'",
  #     hr(),
  #     h4("筛选选项", style = "padding-left: 15px;"),
  #     selectInput("dining_type", "类型:",
  #                choices = c("全部" = "all",
  #                          "咖啡馆/餐厅" = "cafes",
  #                          "酒吧/酒馆" = "bars"),
  #                selected = "all"),
  #     checkboxInput("outdoor_only", "仅显示有户外座位", FALSE)
  #   )
  # ),

  # 主内容区
  dashboardBody(
    useShinyjs(),
    setUpTableauInShiny(),

    # 自定义CSS和JavaScript
    tags$head(
      tags$style(HTML("
        .content-wrapper { background-color: #ecf0f5; }
        .info-box { min-height: 90px; cursor: pointer; }
        .info-box:hover { transform: translateY(-2px); box-shadow: 0 4px 12px rgba(0,0,0,0.15); transition: all 0.3s; }
        .welcome-header {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          color: white;
          padding: 40px;
          border-radius: 8px;
          margin-bottom: 20px;
          text-align: center;
        }
        .welcome-header h1 { margin: 0; font-size: 36px; font-weight: 700; }
        .welcome-header p { margin-top: 10px; font-size: 18px; opacity: 0.9; }
        .leaflet-container { border-radius: 5px; }
      ")),
      tags$script(HTML("
        
        // 当切换到人流分析标签时，强制刷新Tableau可视化
        $(document).on('shown.bs.tab', 'a[data-toggle=\"tab\"]', function(e) {
          if($(e.target).parent().find('a').attr('data-value') === 'pedestrian') {
            setTimeout(function() {
              var viz = document.getElementById('pedestrian_viz');
              if(viz && typeof viz.redraw === 'function') {
                viz.redraw();
              }
            }, 100);
          }
        });
      "))
    ),

    tabItems(
      # ===========================================================================
      # 欢迎页面
      # ===========================================================================
      tabItem(
        tabName = "welcome",
        div(class = "welcome-header",
          h1("探索墨尔本"),
          p("Melbourne Explorer - 您的墨尔本互动旅游指南")
        ),

        fluidRow(
          infoBoxOutput("info_pois", width = 3),
          infoBoxOutput("info_dining", width = 3),
          infoBoxOutput("info_artworks", width = 3),
          infoBoxOutput("info_facilities", width = 3)
        ),

        fluidRow(
          box(
            title = "关于本应用", status = "primary", solidHeader = TRUE, width = 6,
            icon = icon("info-circle"),
            h4("欢迎使用墨尔本探索应用！"),
            p("本应用基于墨尔本市开放数据，为游客提供："),
            tags$ul(
              tags$li(strong("文化景点："), "公共艺术品、雕塑、纪念碑"),
              tags$li(strong("餐饮美食："), "咖啡馆、餐厅"),
              tags$li(strong("酒吧夜生活："), "酒吧、酒馆"),
              tags$li(strong("实用设施："), "公共厕所、饮水喷泉"),
              tags$li(strong("人流分析："), "了解热门地点和最佳访问时间")
            ),
            hr(),
            p(tags$small(
              "数据来源: ", tags$a("墨尔本市开放数据门户",
                                 href = "https://data.melbourne.vic.gov.au/",
                                 target = "_blank"),
              " (CC BY 4.0)"
            ))
          ),

          box(
            title = "快速开始", status = "success", solidHeader = TRUE, width = 6,
            icon = icon("rocket"),
            h4("如何使用本应用："),
            tags$ol(
              tags$li(strong("探索地图："), "查看所有景点、餐饮和设施位置"),
              tags$li(strong("文化景点："), "发现墨尔本的艺术品和历史遗迹"),
              tags$li(strong("餐饮美食："), "寻找咖啡馆和餐厅"),
              tags$li(strong("酒吧夜生活："), "探索墨尔本丰富的夜生活文化和酒吧文化"),
              tags$li(strong("人流分析："), "了解不同时段的访客模式")
            ),
            hr(),
            p("点击左侧菜单开始探索！")
          )
        ),

        fluidRow(
          box(
            title = "数据概览 - Tableau仪表板", status = "warning", solidHeader = TRUE,
            width = 12,
            # Tableau仪表板嵌入
            div(style = "width: 100%; height: 650px; overflow: auto;",
              tableauPublicViz(
                id = "overview_dashboard",
                url = "https://public.tableau.com/views/Overview-A3/Dashboard1?:language=zh-CN&publish=yes&:sid=&:redirect=auth&:display_count=n&:origin=viz_share_link",
                height = "650px",
                width = "100%"
              )
            )
          )
        )
      ),

      # ===========================================================================
      # 探索地图
      # ===========================================================================
      tabItem(
        tabName = "map",
        h2("探索墨尔本地图"),
        p("在地图上查看所有景点、餐饮和公共设施。点击地图上的任意位置查看1公里内的附近设施。使用左侧的图层控制选择要显示的内容。"),

        fluidRow(
          box(
            title = "交互式地图", status = "primary", solidHeader = TRUE, width = 9,
            leafletOutput("main_map", height = 500)
          ),
          
          box(
            title = "附近人流时段分析", status = "success", solidHeader = TRUE, width = 3,
            uiOutput("busy_hours_info")
          )
        ),
        
        fluidRow(
          box(
            title = "附近设施分析", status = "info", solidHeader = TRUE, width = 12,
            uiOutput("location_info")
          )
        )
      ),

      # ===========================================================================
      # 文化景点
      # ===========================================================================
      tabItem(
        tabName = "attractions",
        h2("文化景点"),
        p("探索墨尔本的公共艺术、雕塑和历史纪念碑。"),

        fluidRow(
          valueBoxOutput("vbox_art", width = 3),
          valueBoxOutput("vbox_fountains", width = 3),
          valueBoxOutput("vbox_monuments", width = 3),
          valueBoxOutput("vbox_sculptures", width = 3)
        ),

        fluidRow(
          box(
            title = "景点地图", status = "primary", solidHeader = TRUE, width = 12,
            leafletOutput("attractions_map", height = 500)
          )
        ),

        fluidRow(
          box(
            title = "艺术品分布 - Tableau可视化", status = "success", solidHeader = TRUE,
            width = 12,
            # Tableau仪表板嵌入 - 2000x600的仪表板
            div(style = "width: 100%; height: 650px; overflow: auto;",
              tableauPublicViz(
                id = "art_viz",
                url = "https://public.tableau.com/shared/JBKS2Z28K?:display_count=n&:origin=viz_share_link",
                height = "600px",
                width = "2000px",
                device = "desktop",
                toolbar = "hidden"
              )
            )
          )
        )
      ),

      # ===========================================================================
      # 餐饮美食
      # ===========================================================================
      tabItem(
        tabName = "dining",
        h2("餐饮美食"),
        p("发现墨尔本著名的咖啡文化和多样化的餐饮选择。"),

        fluidRow(
          valueBoxOutput("vbox_cafes", width = 3),
          valueBoxOutput("vbox_top_area", width = 3),

          valueBoxOutput("vbox_outdoor", width = 3),
          valueBoxOutput("vbox_dining_year", width = 3)

        ),

        # fluidRow(
        #   box(
        #     title = "餐饮地图", status = "primary", solidHeader = TRUE, width = 12,
        #     leafletOutput("dining_map", height = 500)
        #   )
        # ),

        fluidRow(
          box(
            title = "餐饮分布 - Tableau可视化", status = "warning", solidHeader = TRUE,
            width = 12,
            # Tableau仪表板嵌入 - 自适应尺寸
            div(style = "width: 100%; height: 100vh; min-height: 800px;overflow:scroll",
              tableauPublicViz(
                id = "restaurant_viz",
                url = "https://public.tableau.com/shared/ZQM44P3N9?:display_count=n&:origin=viz_share_link",
                height = "1600px",
                device = "desktop",
                toolbar = "hidden"
              )
            )
          )
        )
      ),

      # ===========================================================================
      # 酒吧夜生活
      # ===========================================================================
      tabItem(
        tabName = "bars",
        h2("酒吧夜生活"),
        p("探索墨尔本丰富的夜生活文化和酒吧文化。"),

        fluidRow(
          valueBoxOutput("vbox_total_bars", width = 3),
          valueBoxOutput("vbox_bars_capacity", width = 3),
          valueBoxOutput("vbox_bars_areas", width = 3),
          valueBoxOutput("vbox_bars_year", width = 3)
        ),

        fluidRow(
          box(
            title = "酒吧地图", status = "primary", solidHeader = TRUE, width = 12,
            leafletOutput("bars_map", height = 500)
          )
        ),

        fluidRow(
          box(
            title = "酒吧分布 - Tableau可视化", status = "warning", solidHeader = TRUE,
            width = 12,
            # Tableau仪表板嵌入
            div(style = "width: 100%; height: 650px; overflow: auto;",
              tableauPublicViz(
                id = "bars_viz",
                url = "https://public.tableau.com/views/Restaurants_in_Melbourne/Restaurants_in_Melbourne?:language=en-US&publish=yes&:sid=&:redirect=auth&:display_count=n&:origin=viz_share_link",
                height = "600px",
                width = "2000px",
                device = "desktop",
                toolbar = "hidden"
              )
            )
          )
        )
      ),

      # ===========================================================================
      # 人流分析
      # ===========================================================================
      tabItem(
        tabName = "pedestrian",
        h2("人流分析"),
        p("基于墨尔本市行人计数系统的数据，了解不同地点的访客模式和最佳访问时间。"),

        fluidRow(
          valueBoxOutput("vbox_sensors", width = 4),
          infoBoxOutput("info_peak_time", width = 4),
          infoBoxOutput("info_quiet_time", width = 4)
        ),

        fluidRow(
          box(
            title = "行人传感器位置", status = "primary", solidHeader = TRUE, width = 12,
            leafletOutput("pedestrian_map", height = 500)
          )
        ),
        
        fluidRow(
          box(
            title = "人流模式 - Tableau可视化", status = "warning", solidHeader = TRUE,
            width = 12,
            # Tableau仪表板嵌入 - 2000x600的仪表板
            div(style = "width: 100%; height: 650px; overflow: auto;",
              tableauPublicViz(
                id = "pedestrian_viz",
                url = "https://public.tableau.com/views/Population_17608525972330/Human_traffic?:language=zh-CN&publish=yes&:sid=&:redirect=auth&:display_count=n&:origin=viz_share_link",
                height = "600px",
                width = "2000px",
                device = "desktop",
                toolbar = "hidden"
              )
            )
          )
        )
      )
    )
  )
)

# ==============================================================================
# 服务器逻辑
# ==============================================================================

server <- function(input, output, session) {

  # 加载数据
  data <- load_data()

  # 响应式值
  rv <- reactiveValues(
    selected_location = NULL,
    nearby_facilities = NULL,
    click_location = NULL,
    location_name = "未命名位置",
    busy_hours_data = NULL
  )

  # 通用繁忙时段分析函数
  analyze_busy_hours_for_location <- function(lat, lng) {
    tryCatch({
      busy_result <- analyze_busy_hours(
        lat, lng, data$pedestrian, max_distance_km = 1.0
      )
      rv$busy_hours_data <- busy_result
    }, error = function(e) {
      rv$busy_hours_data <- NULL
    })
  }
  
  # ===========================================================================
  # 欢迎页面 - 信息框
  # ===========================================================================

  output$info_pois <- renderInfoBox({
    count <- nrow(data$pois)
    infoBox(
      "文化景点",
      count,
      icon = icon("landmark"),
      color = "purple",
      fill = TRUE
    )
  })

  output$info_dining <- renderInfoBox({
    count <- nrow(data$cafes)
    infoBox(
      "餐饮场所",
      count,
      icon = icon("utensils"),
      color = "yellow",
      fill = TRUE
    )
  })

  output$info_artworks <- renderInfoBox({
    count <- nrow(data$artworks)
    infoBox(
      "公共艺术品",
      count,
      icon = icon("palette"),
      color = "green",
      fill = TRUE
    )
  })

  output$info_facilities <- renderInfoBox({
    count <- nrow(data$toilets) + nrow(data$fountains)
    infoBox(
      "公共设施",
      count,
      icon = icon("restroom"),
      color = "aqua",
      fill = TRUE
    )
  })

  # ===========================================================================
  # 探索地图
  # ===========================================================================

  output$main_map <- renderLeaflet({
    map <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 144.9631, lat = -37.8136, zoom = 13)

    # 根据选择的图层添加标记
    layers <- input$map_layers

    # 添加POI（文化景点）
    if ("pois" %in% layers && nrow(data$pois) > 0) {
      map <- map %>%
        addCircleMarkers(
          data = data$pois,
          lng = ~longitude,
          lat = ~latitude,
          radius = 7,
          color = "#e74c3c",
          fillOpacity = 0.7,
          stroke = TRUE,
          weight = 1,
          popup = ~paste0(
            "<strong>", name, "</strong><br/>",
            "类别: ", category, "<br/>",
            ifelse(!is.na(artist), paste0("艺术家: ", artist, "<br/>"), "")
          ),
          layerId = ~paste0("poi_", poi_id),
          group = "文化景点"
        )
    }

    # 添加咖啡馆/餐厅
    if ("cafes" %in% layers && nrow(data$cafes) > 0) {
      # 先添加唯一ID列
      cafes_with_id <- data$cafes %>%
        mutate(cafe_id = row_number())

      map <- map %>%
        addCircleMarkers(
          data = cafes_with_id,
          lng = ~longitude,
          lat = ~latitude,
          radius = 6,
          color = "#3498db",
          fillOpacity = 0.6,
          stroke = TRUE,
          weight = 1,
          popup = ~paste0(
            "<strong>", trading_name, "</strong><br/>",
            "座位数: ", num_seats, "<br/>",
            ifelse(has_outdoor, "有户外座位", "仅室内座位")
          ),
          layerId = ~paste0("cafe_", cafe_id),
          group = "咖啡馆/餐厅"
        )
    }

    # 添加酒吧/酒馆
    if ("bars" %in% layers && nrow(data$bars) > 0) {
      # 先添加唯一ID列
      bars_with_id <- data$bars %>%
        mutate(bar_id = row_number())

      map <- map %>%
        addCircleMarkers(
          data = bars_with_id,
          lng = ~longitude,
          lat = ~latitude,
          radius = 6,
          color = "#f39c12",
          fillOpacity = 0.6,
          stroke = TRUE,
          weight = 1,
          popup = ~paste0(
            "<strong>", trading_name, "</strong><br/>",
            "容量: ", num_patrons, " 人"
          ),
          layerId = ~paste0("bar_", bar_id),
          group = "酒吧/酒馆"
        )
    }

    # 添加公共厕所
    if ("toilets" %in% layers && nrow(data$toilets) > 0) {
      map <- map %>%
        addCircleMarkers(
          data = data$toilets,
          lng = ~longitude,
          lat = ~latitude,
          radius = 5,
          color = "#9b59b6",
          fillOpacity = 0.5,
          stroke = TRUE,
          weight = 1,
          popup = ~paste0(
            "<strong>公共厕所</strong><br/>",
            name, "<br/>",
            "设施: ", facilities
          ),
          group = "公共厕所"
        )
    }

    # 添加饮水喷泉
    if ("fountains" %in% layers && nrow(data$fountains) > 0) {
      map <- map %>%
        addCircleMarkers(
          data = data$fountains,
          lng = ~longitude,
          lat = ~latitude,
          radius = 4,
          color = "#1abc9c",
          fillOpacity = 0.5,
          stroke = TRUE,
          weight = 1,
          popup = ~paste0("<strong>饮水喷泉</strong><br/>", name),
          group = "饮水喷泉"
        )
    }

    return(map)
  })

  # 地图标记点击事件监听器
  observeEvent(input$main_map_marker_click, {
    marker_click <- input$main_map_marker_click
    
    # 更新点击位置
    rv$click_location <- list(
      lat = marker_click$lat,
      lng = marker_click$lng
    )
    
    # 从标记的popup中提取名称
    if (!is.null(marker_click$id)) {
      # 根据标记ID获取对应的设施信息
      marker_id <- marker_click$id
      
      # 从不同数据集中查找对应的设施
      facility_name <- NULL
      
      # 检查咖啡馆/餐厅
      if (grepl("^cafe_", marker_id)) {
        cafe_id <- as.numeric(gsub("cafe_", "", marker_id))
        if (cafe_id <= nrow(data$cafes)) {
          facility_name <- data$cafes$trading_name[cafe_id]
        }
      }
      # 检查酒吧/酒馆
      else if (grepl("^bar_", marker_id)) {
        bar_id <- as.numeric(gsub("bar_", "", marker_id))
        if (bar_id <= nrow(data$bars)) {
          facility_name <- data$bars$trading_name[bar_id]
        }
      }
      # 检查POI/地标
      else if (grepl("^poi_", marker_id)) {
        poi_id <- gsub("poi_", "", marker_id)
        poi_data <- data$pois %>% filter(poi_id == !!poi_id)
        if (nrow(poi_data) > 0) {
          facility_name <- poi_data$name[1]
        }
      }
      
      # 设置位置名称
      if (!is.null(facility_name)) {
        rv$location_name <- facility_name
      } else {
        rv$location_name <- paste0("位置 (", round(marker_click$lat, 4), ", ", round(marker_click$lng, 4), ")")
      }
    }
    
    # 获取附近设施
    tryCatch({
      # 加载所有设施数据
      all_facilities <- load_all_facilities()
      
      # 获取500m内的设施
      nearby <- get_nearby_facilities(
        marker_click$lat, 
        marker_click$lng, 
        all_facilities, 
        radius_km = 0.5
      )
      
      # 去重：按设施名称和类型去重，保留最近的记录
      nearby <- nearby %>%
        group_by(facility_name, facility_type) %>%
        slice_min(distance, n = 1) %>%
        ungroup()
      
      # 排除自身：如果点击的是设施标记，排除该设施本身
      if (!is.null(facility_name)) {
        nearby <- nearby %>%
          filter(!(facility_name == !!facility_name & distance < 0.01))  # 排除距离小于10米的自身
      }
      
      # 更新响应式值
      rv$nearby_facilities <- nearby
      
    }, error = function(e) {
      shiny::showNotification(
        paste0("获取附近设施时出错: ", e$message),
        type = "error",
        duration = 5
      )
    })
    
    # 分析繁忙时段
    analyze_busy_hours_for_location(marker_click$lat, marker_click$lng)
  })
  
  # 地图空白区域点击事件监听器（作为备用）
  observeEvent(input$main_map_click, {
    click <- input$main_map_click
    
    # 更新点击位置
    rv$click_location <- list(
      lat = click$lat,
      lng = click$lng
    )
    
    # 使用坐标作为名称
    rv$location_name <- paste0("位置 (", round(click$lat, 4), ", ", round(click$lng, 4), ")")
    
    # 获取附近设施
    tryCatch({
      # 加载所有设施数据
      all_facilities <- load_all_facilities()
      
      # 获取500m内的设施
      nearby <- get_nearby_facilities(
        click$lat, 
        click$lng, 
        all_facilities, 
        radius_km = 0.5
      )
      
      # 去重：按设施名称和类型去重，保留最近的记录
      nearby <- nearby %>%
        group_by(facility_name, facility_type) %>%
        slice_min(distance, n = 1) %>%
        ungroup()
      
      # 更新响应式值
      rv$nearby_facilities <- nearby
      
    }, error = function(e) {
      shiny::showNotification(
        paste0("获取附近设施时出错: ", e$message),
        type = "error",
        duration = 5
      )
    })
    
    # 分析繁忙时段
    analyze_busy_hours_for_location(click$lat, click$lng)
  })


  # 位置信息面板
  output$location_info <- renderUI({
    if (is.null(rv$click_location)) {
      div(
        style = "text-align: center; padding: 40px;",
        icon("info-circle", class = "fa-3x", style = "color: #3c8dbc; display: block; margin: 20px auto;"),
        h4("点击地图上的任意位置查看附近设施", style = "color: #666; margin-top: 20px;"),
        p("搜索半径: 500米", style = "color: #999; font-size: 14px; margin-top: 10px;")
      )
    } else {
      # 显示点击位置信息
      click_info <- div(
        style = "background-color: #f8f9fa; padding: 20px; border-radius: 5px; margin-bottom: 20px;",
        h4("📍 点击位置", style = "color: #3c8dbc; margin-bottom: 15px;"),
        fluidRow(
          column(4, 
            p(strong("位置名称: "), rv$location_name, style = "font-size: 16px; margin-bottom: 5px;")
          ),
          column(4,
            p(strong("纬度: "), round(rv$click_location$lat, 6), style = "font-size: 14px; margin-bottom: 5px;")
          ),
          column(4,
            p(strong("经度: "), round(rv$click_location$lng, 6), style = "font-size: 14px; margin-bottom: 5px;")
          )
        )
      )
      
      # 显示附近设施
      if (!is.null(rv$nearby_facilities) && nrow(rv$nearby_facilities) > 0) {
        # 获取设施类型选项
        facility_types <- unique(rv$nearby_facilities$facility_type)
        
        facilities_info <- div(
          h4(paste0("🏢 附近设施 (", nrow(rv$nearby_facilities), "个)"), style = "color: #2c3e50; margin-bottom: 20px;"),
          fluidRow(
            column(6,
              selectInput("facility_type_filter", "筛选类型:", 
                         choices = c("全部" = "all", setNames(facility_types, facility_types)),
                         selected = "all", width = "100%")
            ),
            column(6,
              selectInput("distance_filter", "距离筛选:", 
                         choices = c("全部距离" = "all", "0-100m" = "0.1", "100-200m" = "0.2", "200-300m" = "0.3", "300-500m" = "0.5"),
                         selected = "all", width = "100%")
            )
          ),
          div(
            style = "margin-top: 20px;",
            DT::dataTableOutput("nearby_facilities_table")
          )
        )
      } else {
        facilities_info <- div(
          style = "text-align: center; padding: 40px;",
          icon("search", class = "fa-2x", style = "color: #95a5a6; margin-bottom: 15px;"),
          h4("该位置500m内没有找到设施", style = "color: #666;")
        )
      }
      
      div(click_info, facilities_info)
    }
  })
  
  # 繁忙时段分析面板
  output$busy_hours_info <- renderUI({
    if (is.null(rv$busy_hours_data)) {
      div(
        style = "text-align: center; padding: 15px;",
        icon("clock", class = "fa-2x", style = "color: #27ae60; margin-bottom: 10px;"),
        p("点击地图查看繁忙时段", style = "color: #7f8c8d; font-size: 12px;"),
        p("将显示该区域的人流高峰时段", style = "color: #95a5a6; font-size: 10px;"),
        hr(),
        h6("图例", style = "color: #2c3e50; margin-bottom: 8px;"),
        div(
          style = "text-align: left; font-size: 12px;",
          p(tags$span(style = "color: #e74c3c; font-size: 14px;", "●"), " 文化景点"),
          p(tags$span(style = "color: #3498db; font-size: 14px;", "●"), " 咖啡馆/餐厅"),
          p(tags$span(style = "color: #f39c12; font-size: 14px;", "●"), " 酒吧/酒馆"),
          p(tags$span(style = "color: #9b59b6; font-size: 14px;", "●"), " 公共厕所"),
          p(tags$span(style = "color: #1abc9c; font-size: 14px;", "●"), " 饮水喷泉")
        )
      )
    } else {
      busy_data <- rv$busy_hours_data
      ui_data <- busy_data$ui_data
      
      
      if (is.null(ui_data)) {
        # 回退到简单显示
        summary <- busy_data$summary
        div(
          style = "padding: 15px;",
          h5("📊 繁忙时段分析", style = "color: #2c3e50;"),
          div(
            style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; margin-bottom: 10px;",
            p(strong("高峰时段: "), summary$peak_hour, " (", summary$peak_people, "人)", style = "font-size: 12px; margin-bottom: 5px;"),
            p(strong("安静时段: "), summary$quiet_hour, " (", summary$quiet_people, "人)", style = "font-size: 12px; margin-bottom: 5px;"),
            p(strong("平均人流: "), summary$avg_people, "人/小时", style = "font-size: 12px;")
          ),
          hr(),
          h6("图例", style = "color: #2c3e50; margin-bottom: 8px;"),
          div(
            style = "text-align: left; font-size: 12px;",
            p(tags$span(style = "color: #e74c3c; font-size: 14px;", "●"), " 文化景点"),
            p(tags$span(style = "color: #3498db; font-size: 14px;", "●"), " 咖啡馆/餐厅"),
            p(tags$span(style = "color: #f39c12; font-size: 14px;", "●"), " 酒吧/酒馆"),
            p(tags$span(style = "color: #9b59b6; font-size: 14px;", "●"), " 公共厕所"),
            p(tags$span(style = "color: #1abc9c; font-size: 14px;", "●"), " 饮水喷泉")
          )
        )
      } else {
        # Google Maps风格的繁忙时段显示
        div(
          style = "padding: 10px;",
          # 繁忙时段条形图
          div(
            style = "margin-bottom: 15px;",
            div(
              style = "display: flex; align-items: end; height: 40px; gap: 2px;",
              lapply(1:nrow(ui_data$key_data), function(i) {
                row <- ui_data$key_data[i,]
                height <- max(4, row$busy_percentage * 0.3)  # 最小高度4px
                color <- if (row$busy_percentage > 80) "#e74c3c" 
                        else if (row$busy_percentage > 60) "#f39c12"
                        else if (row$busy_percentage > 40) "#f1c40f"
                        else "#95a5a6"
                
                div(
                  style = paste0("flex: 1; background-color: ", color, "; height: ", height, "px; border-radius: 2px; position: relative; cursor: pointer;"),
                  title = paste0("时间: ", row$time_label, "\\n人流量: ", round(row$avg_people), "人\\n繁忙程度: ", row$busy_percentage, "%"),
                  # 时间标签
                  div(
                    style = "position: absolute; bottom: -20px; left: 50%; transform: translateX(-50%); font-size: 10px; color: #7f8c8d;",
                    row$time_label
                  )
                )
              })
            )
          ),
           div(
           style = "padding: 15px;",
      
           div(
             style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px;",
             if (!is.null(ui_data)) {
               list(
                 p(strong("高峰时段: "), ui_data$peak_hour, " (", ui_data$peak_people, "人)", style = "font-size: 12px; margin-bottom: 5px;"),
                 p(strong("安静时段: "), ui_data$quiet_hour, " (", ui_data$quiet_people, "人)", style = "font-size: 12px; margin-bottom: 5px;"),
                 p(strong("平均人流: "), ui_data$avg_people, "人/小时", style = "font-size: 12px;")
               )
             } else {
               list(
                 p(strong("高峰时段: "), "数据加载中...", style = "font-size: 12px; margin-bottom: 5px;"),
                 p(strong("安静时段: "), "数据加载中...", style = "font-size: 12px; margin-bottom: 5px;"),
                 p(strong("平均人流: "), "数据加载中...", style = "font-size: 12px;")
               )
             }
           )
         ),
          hr(),
        h6("图例", style = "color: #2c3e50; margin-bottom: 8px;"),
        div(
          style = "text-align: left; font-size: 12px;",
          p(tags$span(style = "color: #e74c3c; font-size: 14px;", "●"), " 文化景点"),
          p(tags$span(style = "color: #3498db; font-size: 14px;", "●"), " 咖啡馆/餐厅"),
          p(tags$span(style = "color: #f39c12; font-size: 14px;", "●"), " 酒吧/酒馆"),
          p(tags$span(style = "color: #9b59b6; font-size: 14px;", "●"), " 公共厕所"),
          p(tags$span(style = "color: #1abc9c; font-size: 14px;", "●"), " 饮水喷泉")
        )
        )
      }
    }
  })

  # 附近设施表格
  output$nearby_facilities_table <- DT::renderDataTable({
    if (!is.null(rv$nearby_facilities) && nrow(rv$nearby_facilities) > 0) {
      # 根据类型和距离筛选数据
      filtered_data <- rv$nearby_facilities
      
      # 类型筛选
      if (!is.null(input$facility_type_filter) && input$facility_type_filter != "all") {
        filtered_data <- filtered_data %>%
          filter(facility_type == input$facility_type_filter)
      }
      
      # 距离筛选
      if (!is.null(input$distance_filter) && input$distance_filter != "all") {
        max_distance <- as.numeric(input$distance_filter)
        filtered_data <- filtered_data %>%
          filter(distance <= max_distance)
      }
      
      # 准备显示数据
      display_data <- filtered_data %>%
        select(
          "类型" = facility_type,
          "名称" = facility_name,
          "信息" = facility_info,
          "距离(km)" = distance
        ) %>%
        mutate(
          "距离(km)" = round(`距离(km)`, 3)
        )
      
      DT::datatable(
        display_data,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'tip',
          searching = TRUE,
          ordering = TRUE,
          info = TRUE,
          paging = TRUE
        ),
        rownames = FALSE,
        filter = 'none'
      ) %>%
        DT::formatStyle(
          "类型",
          backgroundColor = DT::styleEqual(
            c("Cafe/Restaurant", "Bar/Pub", "Landmark", "Public Toilet", "Drinking Fountain"),
            c("#e3f2fd", "#fff3e0", "#f3e5f5", "#e8f5e8", "#e0f2f1")
          )
        ) %>%
        DT::formatStyle(
          "距离(km)",
          textAlign = "right"
        )
    }
  }, server = FALSE)

  # ===========================================================================
  # 文化景点页面
  # ===========================================================================

  # 艺术品数量（来自artworks数据集，类别为Art）
  output$vbox_art <- renderValueBox({
    count <- data$artworks %>% filter(category == "Art") %>% nrow()
    valueBox(
      value = count,
      subtitle = "艺术品",
      icon = icon("palette"),
      color = "purple"
    )
  })

  # 喷泉数量（来自artworks数据集，类别为Fountain）
  output$vbox_fountains <- renderValueBox({
    count <- data$artworks %>% filter(category == "Fountain") %>% nrow()
    valueBox(
      value = count,
      subtitle = "喷泉",
      icon = icon("fountain"),
      color = "aqua"
    )
  })

  # 纪念碑数量（来自artworks数据集，类别为Monument）
  output$vbox_monuments <- renderValueBox({
    count <- data$artworks %>% filter(category == "Monument") %>% nrow()
    valueBox(
      value = count,
      subtitle = "纪念碑",
      icon = icon("monument"),
      color = "red"
    )
  })

  # 雕塑数量（来自memorials数据集）
  output$vbox_sculptures <- renderValueBox({
    count <- data$pois %>% filter(type == "Memorial") %>% nrow()
    valueBox(
      value = count,
      subtitle = "雕塑",
      icon = icon("user-tie"),
      color = "orange"
    )
  })

  # 景点地图渲染
  output$attractions_map <- renderLeaflet({
    map <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Voyager) %>%
      setView(lng = 144.9631, lat = -37.8136, zoom = 13)

    selected_types <- input$attraction_types

    # 如果没有选择任何类型，返回空地图（不显示任何标记）
    if (is.null(selected_types) || length(selected_types) == 0) {
      return(map)
    }

    # 添加艺术品（紫色）
    if ("Art" %in% selected_types) {
      art_data <- data$artworks %>% filter(category == "Art")
      if (nrow(art_data) > 0) {
        map <- map %>%
          addCircleMarkers(
            data = art_data,
            lng = ~longitude,
            lat = ~latitude,
            radius = 8,
            color = "#9b59b6",  # 紫色
            fillOpacity = 0.7,
            stroke = TRUE,
            weight = 2,
            popup = ~paste0(
              "<strong>", name, "</strong><br/>",
              "类别: 艺术品<br/>",
              ifelse(!is.na(artist), paste0("艺术家: ", artist, "<br/>"), ""),
              ifelse(!is.na(art_date), paste0("年份: ", art_date, "<br/>"), ""),
              ifelse(!is.na(address), paste0("地址: ", address), "")
            )
          )
      }
    }

    # 添加喷泉（青色）
    if ("Fountain" %in% selected_types) {
      fountain_data <- data$artworks %>% filter(category == "Fountain")
      if (nrow(fountain_data) > 0) {
        map <- map %>%
          addCircleMarkers(
            data = fountain_data,
            lng = ~longitude,
            lat = ~latitude,
            radius = 8,
            color = "#1abc9c",  # 青色
            fillOpacity = 0.7,
            stroke = TRUE,
            weight = 2,
            popup = ~paste0(
              "<strong>", name, "</strong><br/>",
              "类别: 喷泉<br/>",
              ifelse(!is.na(artist), paste0("艺术家: ", artist, "<br/>"), ""),
              ifelse(!is.na(art_date), paste0("年份: ", art_date, "<br/>"), ""),
              ifelse(!is.na(address), paste0("地址: ", address), "")
            )
          )
      }
    }

    # 添加纪念碑（红色）
    if ("Monument" %in% selected_types) {
      monument_data <- data$artworks %>% filter(category == "Monument")
      if (nrow(monument_data) > 0) {
        map <- map %>%
          addCircleMarkers(
            data = monument_data,
            lng = ~longitude,
            lat = ~latitude,
            radius = 8,
            color = "#e74c3c",  # 红色
            fillOpacity = 0.7,
            stroke = TRUE,
            weight = 2,
            popup = ~paste0(
              "<strong>", name, "</strong><br/>",
              "类别: 纪念碑<br/>",
              ifelse(!is.na(artist), paste0("艺术家: ", artist, "<br/>"), ""),
              ifelse(!is.na(art_date), paste0("年份: ", art_date, "<br/>"), ""),
              ifelse(!is.na(address), paste0("地址: ", address), "")
            )
          )
      }
    }

    # 添加雕塑（橙色）- 来自memorials数据集
    if ("Memorial" %in% selected_types) {
      sculpture_data <- data$pois %>% filter(type == "Memorial")
      if (nrow(sculpture_data) > 0) {
        map <- map %>%
          addCircleMarkers(
            data = sculpture_data,
            lng = ~longitude,
            lat = ~latitude,
            radius = 8,
            color = "#f39c12",  # 橙色
            fillOpacity = 0.7,
            stroke = TRUE,
            weight = 2,
            popup = ~paste0(
              "<strong>", name, "</strong><br/>",
              "类别: ", category, "<br/>",
              ifelse(!is.na(artist), paste0("艺术家: ", artist, "<br/>"), "")
            )
          )
      }
    }

    return(map)
  })

  # ===========================================================================
  # 餐饮美食页面
  # ===========================================================================

  output$vbox_cafes <- renderValueBox({
    count <- nrow(data$cafes)
    valueBox(
      value = count,
      subtitle = "咖啡馆/餐厅",
      icon = icon("coffee"),
      color = "blue"
    )
  })


  output$vbox_outdoor <- renderValueBox({
    count <- sum(data$cafes$has_outdoor, na.rm = TRUE)
    valueBox(
      value = count,
      subtitle = "有户外座位",
      icon = icon("umbrella-beach"),
      color = "yellow"
    )
  })
output$vbox_top_area <- renderValueBox({
  top_area <- data$cafes %>%
    dplyr::count(clue_area) %>%
    dplyr::arrange(desc(n)) %>%
    dplyr::slice(1) %>%
    dplyr::pull(clue_area)
  
  # 如果区域名称太长，截断显示
  display_area <- ifelse(nchar(top_area) > 15, 
                        paste0(substr(top_area, 1, 12), "..."), 
                        top_area)
  
  valueBox(
    value = display_area,
    subtitle = "餐饮最密集区域",
    icon = icon("map-marker-alt"),
    color = "purple"
  )
})

  output$vbox_dining_year <- renderValueBox({
    cafes_max_year <- max(data$cafes$census_year, na.rm = TRUE)
    valueBox(
      value = cafes_max_year,
      subtitle = "最新数据年份",
      icon = icon("calendar"),
      color = "teal"
    )
  })

  # 筛选餐饮数据
  # filtered_dining <- reactive({
  #   cafes_filtered <- data$cafes
  #   bars_filtered <- data$bars
  #
  #   # 户外座位筛选
  #   if (input$outdoor_only) {
  #     cafes_filtered <- cafes_filtered %>% filter(has_outdoor == TRUE)
  #   }
  #
  #   # 类型筛选
  #   if (input$dining_type == "cafes") {
  #     return(list(cafes = cafes_filtered, bars = data.frame()))
  #   } else if (input$dining_type == "bars") {
  #     return(list(cafes = data.frame(), bars = bars_filtered))
  #   } else {
  #     return(list(cafes = cafes_filtered, bars = bars_filtered))
  #   }
  # })

  # output$dining_map <- renderLeaflet({
  #   dining <- filtered_dining()

  #   map <- leaflet() %>%
  #     addProviderTiles(providers$CartoDB.Positron) %>%
  #     setView(lng = 144.9631, lat = -37.8136, zoom = 13)

  #   # 添加咖啡馆
  #   if (nrow(dining$cafes) > 0) {
  #     map <- map %>%
  #       addCircleMarkers(
  #         data = dining$cafes,
  #         lng = ~longitude,
  #         lat = ~latitude,
  #         radius = 7,
  #         color = "#3498db",
  #         fillOpacity = 0.7,
  #         stroke = TRUE,
  #         weight = 2,
  #         popup = ~paste0(
  #           "<strong>", trading_name, "</strong><br/>",
  #           "类型: ", industry_desc, "<br/>",
  #           "座位数: ", num_seats, "<br/>",
  #           "座位类型: ", seating_type, "<br/>",
  #           ifelse(has_outdoor, "✓ 有户外座位", "仅室内座位"), "<br/>",
  #           "区域: ", clue_area
  #         ),
  #         group = "咖啡馆/餐厅"
  #       )
  #   }

  #   # 添加酒吧
  #   if (nrow(dining$bars) > 0) {
  #     map <- map %>%
  #       addCircleMarkers(
  #         data = dining$bars,
  #         lng = ~longitude,
  #         lat = ~latitude,
  #         radius = 7,
  #         color = "#f39c12",
  #         fillOpacity = 0.7,
  #         stroke = TRUE,
  #         weight = 2,
  #         popup = ~paste0(
  #           "<strong>", trading_name, "</strong><br/>",
  #           "容量: ", num_patrons, " 人<br/>",
  #           "区域: ", clue_area
  #         ),
  #         group = "酒吧/酒馆"
  #       )
  #   }

  #   # 添加图层控制
  #   map <- map %>%
  #     addLayersControl(
  #       overlayGroups = c("咖啡馆/餐厅", "酒吧/酒馆"),
  #       options = layersControlOptions(collapsed = FALSE)
  #     )

  #   return(map)
  # })

  # ===========================================================================
  # 酒吧夜生活页面
  # ===========================================================================

  output$vbox_total_bars <- renderValueBox({
    count <- nrow(data$bars)
    valueBox(
      value = count,
      subtitle = "酒吧总数",
      icon = icon("beer"),
      color = "orange"
    )
  })

  output$vbox_bars_capacity <- renderValueBox({
    total_capacity <- sum(data$bars$num_patrons, na.rm = TRUE)
    valueBox(
      value = total_capacity,
      subtitle = "总容量",
      icon = icon("users"),
      color = "red"
    )
  })

  output$vbox_bars_areas <- renderValueBox({
    unique_areas <- n_distinct(data$bars$clue_area, na.rm = TRUE)
    valueBox(
      value = unique_areas,
      subtitle = "覆盖区域",
      icon = icon("map-marker-alt"),
      color = "purple"
    )
  })

  output$vbox_bars_year <- renderValueBox({
    max_year <- max(data$bars$census_year, na.rm = TRUE)
    valueBox(
      value = max_year,
      subtitle = "最新数据年份",
      icon = icon("calendar"),
      color = "teal"
    )
  })

  output$bars_map <- renderLeaflet({
    map <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 144.9631, lat = -37.8136, zoom = 13)

    # 添加酒吧
    if (nrow(data$bars) > 0) {
      map <- map %>%
        addCircleMarkers(
          data = data$bars,
          lng = ~longitude,
          lat = ~latitude,
          radius = 8,
          color = "#f39c12",
          fillOpacity = 0.7,
          stroke = TRUE,
          weight = 2,
          popup = ~paste0(
            "<strong>", trading_name, "</strong><br/>",
            "容量: ", num_patrons, " 人<br/>",
            "区域: ", clue_area, "<br/>",
            "数据年份: ", census_year
          ),
          group = "酒吧/酒馆"
        )
    }

    return(map)
  })

  # ===========================================================================
  # 人流分析页面
  # ===========================================================================

  output$vbox_sensors <- renderValueBox({
    count <- n_distinct(data$pedestrian$sensor_name)
    valueBox(
      value = count,
      subtitle = "行人传感器",
      icon = icon("broadcast-tower"),
      color = "light-blue"
    )
  })

  output$info_peak_time <- renderInfoBox({
    # 计算最繁忙时段
    peak <- data$pedestrian %>%
      group_by(hour) %>%
      summarise(avg = mean(avg_count, na.rm = TRUE)) %>%
      arrange(desc(avg)) %>%
      slice(1)

    infoBox(
      "最繁忙时段",
      paste0(peak$hour, ":00"),
      icon = icon("clock"),
      color = "red",
      fill = TRUE
    )
  })

  output$info_quiet_time <- renderInfoBox({
    # 计算最安静时段
    quiet <- data$pedestrian %>%
      group_by(hour) %>%
      summarise(avg = mean(avg_count, na.rm = TRUE)) %>%
      arrange(avg) %>%
      slice(1)

    infoBox(
      "最安静时段",
      paste0(quiet$hour, ":00"),
      icon = icon("moon"),
      color = "navy",
      fill = TRUE
    )
  })

  output$pedestrian_map <- renderLeaflet({
    # 获取每个传感器的唯一位置和平均流量
    sensor_data <- data$pedestrian %>%
      filter(!is.na(latitude), !is.na(longitude)) %>%
      group_by(sensor_name, latitude, longitude) %>%
      summarise(
        avg_flow = mean(avg_count, na.rm = TRUE),
        max_flow = max(max_count, na.rm = TRUE),
        .groups = "drop"
      )

    # 根据流量大小设置标记大小
    sensor_data <- sensor_data %>%
      mutate(
        marker_size = case_when(
          avg_flow < 500 ~ 8,
          avg_flow < 1500 ~ 12,
          TRUE ~ 16
        )
      )

    map <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 144.9631, lat = -37.8136, zoom = 13)

    if (nrow(sensor_data) > 0) {
      map <- map %>%
        addCircleMarkers(
          data = sensor_data,
          lng = ~longitude,
          lat = ~latitude,
          radius = ~marker_size,
          color = "#e67e22",
          fillOpacity = 0.6,
          stroke = TRUE,
          weight = 2,
          popup = ~paste0(
            "<strong>", sensor_name, "</strong><br/>",
            "平均人流: ", round(avg_flow), " 人/小时<br/>",
            "最高人流: ", round(max_flow), " 人/小时"
          )
        )
    }

    return(map)
  })

}

# ==============================================================================
# 运行应用
# ==============================================================================

shinyApp(ui = ui, server = server)
