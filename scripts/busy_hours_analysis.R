# ==============================================================================
# Busy Hours Analysis Functions
# ==============================================================================
# 实现繁忙时段分析功能
# ==============================================================================

library(dplyr)
library(readr)
library(plotly)
library(stringr)

# ==============================================================================
# 数据加载和预处理
# ==============================================================================

#' 加载行人数据
load_pedestrian_data <- function() {
  cat("正在加载行人数据...\n")
  
  # 尝试加载RDS文件（应用中使用）
  if (file.exists("data/processed/pedestrian_summary.rds")) {
    pedestrian_data <- readRDS("data/processed/pedestrian_summary.rds")
    cat("从RDS文件加载行人数据\n")
  } else {
    # 回退到CSV文件
    pedestrian_data <- read_csv("data/pedestrian-counting-system-monthly-counts-per-hour.csv", 
                               show_col_types = FALSE)
    
    # 解析Location坐标
    pedestrian_data <- pedestrian_data %>%
      mutate(
        # 从Location字符串中提取坐标
        latitude = as.numeric(str_extract(Location, "-?[0-9]+\\.[0-9]+")),
        longitude = as.numeric(str_extract(Location, "(?<=, )-?[0-9]+\\.[0-9]+"))
      ) %>%
      filter(!is.na(latitude), !is.na(longitude))
    cat("从CSV文件加载行人数据\n")
  }
  
  cat("行人数据加载完成！\n")
  cat("  总记录数:", nrow(pedestrian_data), "\n")
  cat("  列名:", paste(names(pedestrian_data), collapse = ", "), "\n")
  
  return(pedestrian_data)
}

# ==============================================================================
# 距离计算函数
# ==============================================================================

#' 计算两点间距离
calculate_distance <- function(lat1, lon1, lat2, lon2) {
  R <- 6371 # 地球半径(km)
  dlat <- (lat2 - lat1) * pi/180
  dlon <- (lon2 - lon1) * pi/180
  a <- sin(dlat/2)^2 + cos(lat1*pi/180) * cos(lat2*pi/180) * sin(dlon/2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  return(R * c)
}

# ==============================================================================
# 繁忙时段分析函数
# ==============================================================================

#' 获取最近的行人监测点
get_nearest_sensor <- function(click_lat, click_lon, pedestrian_data, max_distance_km = 1.0) {
  # 检查数据结构
  if ("sensor_name" %in% names(pedestrian_data)) {
    # RDS文件结构
    pedestrian_data %>%
      mutate(distance = calculate_distance(click_lat, click_lon, latitude, longitude)) %>%
      filter(distance <= max_distance_km) %>%
      slice_min(distance, n = 1)
  } else {
    # CSV文件结构
    pedestrian_data %>%
      mutate(distance = calculate_distance(click_lat, click_lon, latitude, longitude)) %>%
      filter(distance <= max_distance_km) %>%
      slice_min(distance, n = 1)
  }
}

#' 计算繁忙时段
calculate_busy_hours <- function(sensor_data, pedestrian_data) {
  if (nrow(sensor_data) == 0) {
    return(data.frame())
  }
  
  # 检查数据结构并获取传感器信息
  if ("sensor_name" %in% names(sensor_data)) {
    # RDS文件结构
    sensor_name <- sensor_data$sensor_name[1]
    location_id <- sensor_name  # 使用sensor_name作为ID
    
    # 获取该监测点的所有数据
    all_sensor_data <- pedestrian_data %>%
      filter(sensor_name == !!sensor_name) %>%
      group_by(hour) %>%
      summarise(
        avg_people = mean(avg_count, na.rm = TRUE),
        max_people = max(max_count, na.rm = TRUE),
        min_people = min(min_count, na.rm = TRUE),
        count = n(),
        .groups = 'drop'
      ) %>%
      arrange(hour) %>%
      mutate(
        hour_label = paste0(hour, ":00"),
        busy_level = case_when(
          avg_people > quantile(avg_people, 0.8, na.rm = TRUE) ~ "Very Busy",
          avg_people > quantile(avg_people, 0.6, na.rm = TRUE) ~ "Busy",
          avg_people > quantile(avg_people, 0.4, na.rm = TRUE) ~ "Moderate",
          TRUE ~ "Quiet"
        )
      )
  } else {
    # CSV文件结构
    location_id <- sensor_data$Location_ID[1]
    sensor_name <- sensor_data$Sensor_Name[1]
    
    # 获取该监测点的所有数据
    all_sensor_data <- pedestrian_data %>%
      filter(Location_ID == location_id) %>%
      group_by(HourDay) %>%
      summarise(
        avg_people = mean(Total_of_Directions, na.rm = TRUE),
        max_people = max(Total_of_Directions, na.rm = TRUE),
        min_people = min(Total_of_Directions, na.rm = TRUE),
        count = n(),
        .groups = 'drop'
      ) %>%
      arrange(HourDay) %>%
      mutate(
        hour_label = paste0(HourDay, ":00"),
        busy_level = case_when(
          avg_people > quantile(avg_people, 0.8, na.rm = TRUE) ~ "Very Busy",
          avg_people > quantile(avg_people, 0.6, na.rm = TRUE) ~ "Busy",
          avg_people > quantile(avg_people, 0.4, na.rm = TRUE) ~ "Moderate",
          TRUE ~ "Quiet"
        )
      )
  }
  
  return(list(
    sensor_info = sensor_data,
    hourly_data = all_sensor_data,
    location_id = location_id,
    sensor_name = sensor_name
  ))
}

# ==============================================================================
# 可视化函数
# ==============================================================================

#' 创建繁忙时段图表
create_busy_hours_plot <- function(busy_data) {
  if (is.null(busy_data) || nrow(busy_data$hourly_data) == 0) {
    return(NULL)
  }
  
  hourly_data <- busy_data$hourly_data
  
  # 创建颜色映射
  color_map <- c(
    "Very Busy" = "#e74c3c",
    "Busy" = "#f39c12", 
    "Moderate" = "#f1c40f",
    "Quiet" = "#95a5a6"
  )
  
  # 创建plotly图表
  p <- plot_ly(
    hourly_data,
    x = ~HourDay,
    y = ~avg_people,
    type = 'bar',
    marker = list(
      color = ~busy_level,
      colors = color_map,
      line = list(color = 'white', width = 1)
    ),
    text = ~paste0(
      "时间: ", hour_label, "<br>",
      "平均人流: ", round(avg_people), "<br>",
      "繁忙程度: ", busy_level
    ),
    hoverinfo = 'text'
  ) %>%
    layout(
      title = list(
        text = paste0("繁忙时段分析 - ", busy_data$sensor_name),
        font = list(size = 16)
      ),
      xaxis = list(
        title = "小时",
        tickmode = 'linear',
        tick0 = 0,
        dtick = 2
      ),
      yaxis = list(title = "平均人流"),
      showlegend = FALSE,
      margin = list(l = 50, r = 50, t = 50, b = 50)
    )
  
  return(p)
}

#' 获取繁忙时段数据（用于UI显示）
get_busy_hours_data <- function(busy_data) {
  if (is.null(busy_data) || nrow(busy_data$hourly_data) == 0) {
    return(NULL)
  }
  
  hourly_data <- busy_data$hourly_data
  
  # 计算当前时间段的繁忙程度
  current_hour <- as.numeric(format(Sys.time(), "%H"))
  
  # 检查数据结构
  if ("hour" %in% names(hourly_data)) {
    current_data <- hourly_data %>% filter(hour == current_hour)
  } else {
    current_data <- hourly_data %>% filter(HourDay == current_hour)
  }
  
  # 计算相对繁忙程度
  max_people <- max(hourly_data$avg_people, na.rm = TRUE)
  min_people <- min(hourly_data$avg_people, na.rm = TRUE)
  
  # 获取6个关键时间点的数据
  key_hours <- c(6, 9, 12, 15, 18, 21)
  
  # 检查数据结构
  if ("hour" %in% names(hourly_data)) {
    key_data <- hourly_data %>% 
      filter(hour %in% key_hours) %>%
      arrange(hour) %>%
      mutate(
        relative_busy = (avg_people - min_people) / (max_people - min_people),
        busy_percentage = round(relative_busy * 100),
        time_label = paste0(hour, "时")
      )
  } else {
    key_data <- hourly_data %>% 
      filter(HourDay %in% key_hours) %>%
      arrange(HourDay) %>%
      mutate(
        relative_busy = (avg_people - min_people) / (max_people - min_people),
        busy_percentage = round(relative_busy * 100),
        time_label = paste0(HourDay, "时")
      )
  }
  
  # 当前状态判断
  if (nrow(current_data) > 0) {
    current_relative <- (current_data$avg_people - min_people) / (max_people - min_people)
    if (current_relative > 0.8) {
      status <- "比平时繁忙"
      status_color <- "#e74c3c"
    } else if (current_relative > 0.6) {
      status <- "正常繁忙"
      status_color <- "#f39c12"
    } else if (current_relative > 0.4) {
      status <- "不如平时繁忙"
      status_color <- "#95a5a6"
    } else {
      status <- "非常安静"
      status_color <- "#95a5a6"
    }
  } else {
    status <- "数据不可用"
    status_color <- "#95a5a6"
  }
  
  # 计算高峰和安静时段
  peak_hour_data <- hourly_data %>% slice_max(avg_people, n = 1)
  quiet_hour_data <- hourly_data %>% slice_min(avg_people, n = 1)
  
  return(list(
    key_data = key_data,
    current_status = status,
    status_color = status_color,
    peak_hour = peak_hour_data$hour_label[1],
    peak_people = round(peak_hour_data$avg_people[1]),
    quiet_hour = quiet_hour_data$hour_label[1],
    quiet_people = round(quiet_hour_data$avg_people[1]),
    avg_people = round(mean(hourly_data$avg_people, na.rm = TRUE))
  ))
}

# ==============================================================================
# 主分析函数
# ==============================================================================

#' 分析指定位置的繁忙时段
analyze_busy_hours <- function(click_lat, click_lon, pedestrian_data, max_distance_km = 1.0) {
  cat("分析繁忙时段...\n")
  cat("点击位置: (", click_lat, ", ", click_lon, ")\n")
  
  # 获取最近的监测点
  nearest_sensor <- get_nearest_sensor(click_lat, click_lon, pedestrian_data, max_distance_km)
  
  if (nrow(nearest_sensor) == 0) {
    cat("未找到附近的监测点\n")
    return(NULL)
  }
  
  cat("最近监测点:", nearest_sensor$Sensor_Name[1], "\n")
  cat("距离:", round(nearest_sensor$distance[1], 3), "km\n")
  
  # 计算繁忙时段
  busy_data <- calculate_busy_hours(nearest_sensor, pedestrian_data)
  
  if (nrow(busy_data$hourly_data) == 0) {
    cat("该监测点无数据\n")
    return(NULL)
  }
  
  # 显示统计信息
  hourly_data <- busy_data$hourly_data
  peak_hour <- hourly_data %>% slice_max(avg_people, n = 1)
  quiet_hour <- hourly_data %>% slice_min(avg_people, n = 1)
  
  cat("\n=== 繁忙时段分析结果 ===\n")
  cat("监测点:", busy_data$sensor_name, "\n")
  cat("高峰时段:", peak_hour$hour_label[1], "(", round(peak_hour$avg_people[1]), "人)\n")
  cat("安静时段:", quiet_hour$hour_label[1], "(", round(quiet_hour$avg_people[1]), "人)\n")
  cat("平均人流:", round(mean(hourly_data$avg_people, na.rm = TRUE)), "人/小时\n")
  
  # 创建图表
  plot <- create_busy_hours_plot(busy_data)
  
  # 获取UI显示数据
  ui_data <- get_busy_hours_data(busy_data)
  
  return(list(
    busy_data = busy_data,
    plot = plot,
    ui_data = ui_data,
    summary = list(
      peak_hour = peak_hour$hour_label[1],
      peak_people = round(peak_hour$avg_people[1]),
      quiet_hour = quiet_hour$hour_label[1],
      quiet_people = round(quiet_hour$avg_people[1]),
      avg_people = round(mean(hourly_data$avg_people, na.rm = TRUE))
    )
  ))
}

# ==============================================================================
# 测试函数
# ==============================================================================

#' 测试繁忙时段分析
test_busy_hours <- function() {
  cat("测试繁忙时段分析功能...\n")
  
  # 加载数据
  pedestrian_data <- load_pedestrian_data()
  
  # 测试CBD中心
  result <- analyze_busy_hours(-37.8136, 144.9631, pedestrian_data, 1.0)
  
  if (!is.null(result)) {
    cat("\n✅ 繁忙时段分析测试成功！\n")
    return(result)
  } else {
    cat("\n❌ 繁忙时段分析测试失败\n")
    return(NULL)
  }
}

# 如果直接运行，执行测试
if (!interactive()) {
  test_result <- test_busy_hours()
}
