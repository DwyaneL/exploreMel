# ==============================================================================
# Simple Nearby Facilities Functions for Shiny Integration
# ==============================================================================
# 简化版本，专门用于Shiny应用集成
# ==============================================================================

library(dplyr)

# ==============================================================================
# 核心函数
# ==============================================================================

#' 使用Haversine公式计算两点间距离
calculate_distance <- function(lat1, lon1, lat2, lon2) {
  R <- 6371 # 地球半径(km)
  dlat <- (lat2 - lat1) * pi/180
  dlon <- (lon2 - lon1) * pi/180
  a <- sin(dlat/2)^2 + cos(lat1*pi/180) * cos(lat2*pi/180) * sin(dlon/2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  return(R * c)
}

#' 获取指定半径内的设施
get_nearby_facilities <- function(click_lat, click_lon, facilities_data, radius_km = 1) {
  facilities_data %>%
    mutate(distance = calculate_distance(click_lat, click_lon, latitude, longitude)) %>%
    filter(distance <= radius_km) %>%
    arrange(distance)
}

# ==============================================================================
# 数据整合函数
# ==============================================================================

#' 整合所有设施数据
load_all_facilities <- function() {
  # 加载各种设施数据
  cafes <- readRDS("data/processed/cafes_restaurants.rds") %>%
    mutate(
      facility_type = "Cafe/Restaurant",
      facility_name = trading_name,
      facility_info = paste0("座位数: ", num_seats, 
                            ifelse(has_outdoor, " (有户外座位)", " (仅室内)")),
      industry_category = industry_desc
    )
  
  bars <- readRDS("data/processed/bars_pubs.rds") %>%
    mutate(
      facility_type = "Bar/Pub", 
      facility_name = trading_name,
      facility_info = paste0("容量: ", num_patrons, " 人"),
      industry_category = "Entertainment"
    )
  
  landmarks <- readRDS("data/processed/melbourne_pois.rds") %>%
    mutate(
      facility_type = "Landmark",
      facility_name = name,
      facility_info = paste0("类别: ", category, " | 类型: ", type),
      industry_category = category
    )
  
  toilets <- readRDS("data/processed/public_toilets.rds") %>%
    mutate(
      facility_type = "Public Toilet",
      facility_name = name,
      facility_info = paste0("设施: ", facilities),
      industry_category = "Public Service"
    )
  
  fountains <- readRDS("data/processed/drinking_fountains.rds") %>%
    mutate(
      facility_type = "Drinking Fountain",
      facility_name = name,
      facility_info = paste0("类型: ", type),
      industry_category = "Public Service"
    )
  
  # 合并所有设施数据，只保留必要列
  all_facilities <- bind_rows(cafes, bars, landmarks, toilets, fountains) %>%
    select(facility_type, facility_name, facility_info, industry_category,
           latitude, longitude, clue_area) %>%
    # 去重：按设施名称和类型去重，保留第一个记录
    group_by(facility_name, facility_type) %>%
    slice_head(n = 1) %>%
    ungroup()
  
  return(all_facilities)
}

# ==============================================================================
# 快速测试函数
# ==============================================================================

#' 快速测试功能
quick_test <- function(lat = -37.8136, lon = 144.9631, radius = 1.0) {
  cat("快速测试附近设施功能...\n")
  cat("测试位置: (", lat, ", ", lon, ")\n")
  cat("搜索半径: ", radius, "km\n\n")
  
  # 加载数据
  all_facilities <- load_all_facilities()
  cat("总设施数:", nrow(all_facilities), "\n")
  
  # 获取附近设施
  nearby <- get_nearby_facilities(lat, lon, all_facilities, radius)
  cat("找到设施:", nrow(nearby), "个\n\n")
  
  # 按类型统计
  type_summary <- nearby %>% count(facility_type, sort = TRUE)
  cat("按类型统计:\n")
  print(type_summary)
  
  # 显示最近的几个设施
  cat("\n最近的5个设施:\n")
  print(head(nearby, 5))
  
  return(nearby)
}

# 如果直接运行，执行快速测试
if (!interactive()) {
  quick_test()
}
