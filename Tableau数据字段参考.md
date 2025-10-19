# Tableau 数据字段快速参考

这份文档列出了所有导出CSV文件的实际字段名称，方便在Tableau中使用。

---

## 📁 文件 1: cafes_restaurants.csv

**用途**：咖啡馆和餐厅数据

| 字段名 | 类型 | 说明 | 示例值 |
|--------|------|------|--------|
| `trading_name` | 文本 | 商户名称 | "Café Vue" |
| `business_address` | 文本 | 营业地址 | "430 Little Collins Street" |
| `industry_desc` | 文本 | 行业描述 | "Cafes and Restaurants" |
| `seating_type` | 文本 | 座位类型 | "Indoor and Outdoor" |
| `num_seats` | 数字 | 座位数量 | 85 |
| `longitude` | 数字 | 经度 | 144.9631 |
| `latitude` | 数字 | 纬度 | -37.8136 |
| `clue_area` | 文本 | CLUE小区域 | "Melbourne (CBD)" |
| `seating_category` | 文本 | 座位类别 | "Medium (30-99 seats)" |
| `has_outdoor` | 布尔 | 是否有户外座位 | TRUE/FALSE |
| `census_year` | 数字 | 普查年份 | 2023 |

**推荐可视化**：
- 地图（使用 longitude, latitude）
- 按区域统计（使用 clue_area）
- 座位容量分布（使用 num_seats）

---

## 📁 文件 2: bars_pubs.csv

**用途**：酒吧和酒馆数据

| 字段名 | 类型 | 说明 | 示例值 |
|--------|------|------|--------|
| `trading_name` | 文本 | 商户名称 | "The Hotel Windsor" |
| `business_address` | 文本 | 营业地址 | "111 Spring Street" |
| `num_patrons` | 数字 | 容纳人数 | 120 |
| `longitude` | 数字 | 经度 | 144.9631 |
| `latitude` | 数字 | 纬度 | -37.8136 |
| `clue_area` | 文本 | CLUE小区域 | "Melbourne (CBD)" |
| `capacity_category` | 文本 | 容量类别 | "Medium (50-149)" |
| `census_year` | 数字 | 普查年份 | 2023 |

**推荐可视化**：
- 地图（使用 longitude, latitude）
- 容量分布（使用 num_patrons）

---

## 📁 文件 3: dining_combined.csv ⭐

**用途**：合并的餐饮数据（咖啡馆 + 酒吧）

| 字段名 | 类型 | 说明 | 示例值 |
|--------|------|------|--------|
| `trading_name` | 文本 | 商户名称 | "Café Vue" |
| `business_address` | 文本 | 营业地址 | "430 Little Collins Street" |
| `venue_type` | 文本 | **场所类型** | "Cafe/Restaurant" 或 "Bar/Pub" |
| `capacity` | 数字 | 容量（座位数或顾客数） | 85 |
| `longitude` | 数字 | 经度 | 144.9631 |
| `latitude` | 数字 | 纬度 | -37.8136 |
| `clue_area` | 文本 | CLUE小区域 | "Melbourne (CBD)" |
| `census_year` | 数字 | 普查年份 | 2023 |

**推荐可视化**：
- 按类型着色的地图（使用 venue_type）
- 类型对比（使用 venue_type）
- 这是**最推荐用于仪表板**的餐饮数据文件！

---

## 📁 文件 4: area_summary.csv ⭐

**用途**：按区域统计餐饮场所

| 字段名 | 类型 | 说明 | 示例值 |
|--------|------|------|--------|
| `clue_area` | 文本 | CLUE小区域 | "Melbourne (CBD)" |
| `type` | 文本 | **类型** | "Cafes" 或 "Bars" |
| `count` | 数字 | **数量** | 150 |
| `total_capacity` | 数字 | 总容量 | 5420 |
| `avg_capacity` | 数字 | 平均容量 | 36.13 |

**推荐可视化**：
- 堆叠条形图（clue_area vs count，按 type 着色）
- 区域对比（使用 total_capacity）

**注意**：
- ⚠️ 字段名是 `count`，不是 `total_venues`
- ⚠️ `type` 有两个值："Cafes" 和 "Bars"（不是 "Cafe/Restaurant"）

---

## 📁 文件 5: melbourne_pois.csv

**用途**：景点（艺术品 + 纪念碑）合并数据

| 字段名 | 类型 | 说明 | 示例值 |
|--------|------|------|--------|
| `poi_id` | 数字 | 景点ID | 1 |
| `name` | 文本 | 名称 | "Three Businessmen Who..." |
| `category` | 文本 | 类别 | "Art", "Sculpture", "Fountain", "Monument", "Memorial/Sculpture" |
| `type` | 文本 | 类型 | "Artwork" 或 "Memorial" |
| `artist` | 文本 | 艺术家 | "Deborah Halpern" |
| `address` | 文本 | 地址 | "Federation Square" |
| `description` | 文本 | 描述 | "Artist: Unknown" |
| `latitude` | 数字 | 纬度 | -37.8136 |
| `longitude` | 数字 | 经度 | 144.9631 |

**推荐可视化**：
- 按类别着色的地图（使用 category）
- 类别分布（使用 category）
- 按类型筛选（使用 type）

---

## 📁 文件 6: public_artworks.csv

**用途**：公共艺术品详细数据

| 字段名 | 类型 | 说明 | 示例值 |
|--------|------|------|--------|
| `name` | 文本 | 作品名称 | "Angel" |
| `artist` | 文本 | 艺术家 | "Deborah Halpern" |
| `art_date` | 文本 | 创作年份 | "1982" |
| `address` | 文本 | 地址 | "Melbourne Aquarium" |
| `category` | 文本 | 类别 | "Art", "Sculpture", "Fountain", "Monument" |
| `latitude` | 数字 | 纬度 | -37.8136 |
| `longitude` | 数字 | 经度 | 144.9631 |

**推荐可视化**：
- 按年份的时间线（使用 art_date）
- 艺术家作品数（使用 artist）
- 类别分布（使用 category）

---

## 📁 文件 7: pedestrian_summary.csv ⭐

**用途**：行人流量汇总（按传感器、小时、工作日/周末）

| 字段名 | 类型 | 说明 | 示例值 |
|--------|------|------|--------|
| `sensor_name` | 文本 | 传感器名称 | "Bourke Street Mall" |
| `hour` | 数字 | 一天中的小时（0-23） | 14 |
| `is_weekend` | 布尔 | 是否周末 | TRUE/FALSE |
| `avg_count` | 数字 | 平均人流量 | 1250.5 |
| `max_count` | 数字 | 最高人流量 | 3500 |
| `min_count` | 数字 | 最低人流量 | 150 |
| `latitude` | 数字 | 纬度 | -37.8136 |
| `longitude` | 数字 | 经度 | 144.9631 |
| `day_type` | 文本 | 日期类型 | "Weekday" 或 "Weekend" |

**推荐可视化**：
- 按小时折线图（hour vs avg_count）
- 工作日vs周末对比（使用 day_type）
- 热门地点排名（按 avg_count 排序）

**这是人流分析的核心数据！**

---

## 📁 文件 8: hourly_activity_patterns.csv ⭐

**用途**：每小时活动模式（已聚合）

| 字段名 | 类型 | 说明 | 示例值 |
|--------|------|------|--------|
| `hour` | 数字 | 一天中的小时 | 14 |
| `is_weekend` | 布尔 | 是否周末 | TRUE/FALSE |
| `avg_count` | 数字 | 平均人流量 | 850.3 |
| `total_sensors` | 数字 | 传感器总数 | 25 |
| `day_type` | 文本 | 日期类型 | "Weekday" 或 "Weekend" |
| `time_of_day` | 文本 | 时段 | "Morning", "Afternoon", "Evening", "Night" |

**推荐可视化**：
- 24小时人流折线图（最简单！）
- 工作日vs周末双线对比
- 按时段分组（使用 time_of_day）

**这是最适合入门的人流数据！**

---

## 📁 文件 9: pedestrian_recent.csv

**用途**：最近6个月行人数据（时间序列）

| 字段名 | 类型 | 说明 | 示例值 |
|--------|------|------|--------|
| `sensor_name` | 文本 | 传感器名称 | "Bourke Street Mall" |
| `sensing_date` | 日期 | 感应日期 | "2023-10-15" |
| `year` | 数字 | 年份 | 2023 |
| `month` | 数字 | 月份 | 10 |
| `day_of_week` | 文本 | 星期几 | "Mon", "Tue", ... |
| `hour` | 数字 | 小时 | 14 |
| `total` | 数字 | 总人流量 | 2500 |
| `is_weekend` | 布尔 | 是否周末 | FALSE |
| `latitude` | 数字 | 纬度 | -37.8136 |
| `longitude` | 数字 | 经度 | 144.9631 |

**推荐可视化**：
- 时间趋势折线图（sensing_date vs total）
- 按月汇总
- 添加趋势线

---

## 📁 文件 10: overview_stats.csv

**用途**：概览统计数据

| 字段名 | 类型 | 说明 | 示例值 |
|--------|------|------|--------|
| `metric` | 文本 | 指标名称 | "Total Cafes & Restaurants" |
| `value` | 数字 | 指标值 | 850 |

**可用的指标**：
- Total Cafes & Restaurants
- Total Bars & Pubs
- Total Public Artworks
- Total Memorials
- Total POIs
- Public Toilets
- Drinking Fountains
- Pedestrian Sensors
- Avg Cafe Seating
- Cafes with Outdoor Seating

**推荐可视化**：
- 横向条形图（metric vs value）
- KPI仪表板

---

## 📁 文件 11: public_toilets.csv

**用途**：公共厕所位置

| 字段名 | 类型 | 说明 |
|--------|------|------|
| `name` | 文本 | 厕所名称/位置 |
| `female` | 文本 | 是否有女厕 ("yes"/"no") |
| `male` | 文本 | 是否有男厕 |
| `wheelchair` | 文本 | 是否无障碍 |
| `operator` | 文本 | 运营方 |
| `baby_facil` | 文本 | 是否有婴儿设施 |
| `latitude` | 数字 | 纬度 |
| `longitude` | 数字 | 经度 |
| `location` | 文本 | 位置描述 |
| `facilities` | 文本 | 设施汇总 |

---

## 📁 文件 12: drinking_fountains.csv

**用途**：饮水喷泉位置

| 字段名 | 类型 | 说明 |
|--------|------|------|
| `description` | 文本 | 描述 |
| `coordinates` | 文本 | 坐标字符串 |
| `latitude` | 数字 | 纬度 |
| `longitude` | 数字 | 经度 |
| `name` | 文本 | 名称（同description） |
| `type` | 文本 | 类型（固定为"Drinking Fountain"） |

---

## 🎯 Tableau 制作建议

### 仪表板 1: 数据概览
**推荐使用的文件**：
- ✅ `overview_stats.csv` - 关键指标
- ✅ `area_summary.csv` - 区域分布（使用 count 和 type）
- ✅ `dining_combined.csv` - 餐饮类型分布（使用 venue_type）

### 仪表板 2: 人流模式 ⭐
**推荐使用的文件**：
- ✅ `hourly_activity_patterns.csv` - **最简单！**（24小时折线图）
- ✅ `pedestrian_summary.csv` - 传感器排名
- ✅ `pedestrian_recent.csv` - 时间趋势

### 仪表板 3: 餐饮分布
**推荐使用的文件**：
- ✅ `dining_combined.csv` - 地图（使用 venue_type 着色）
- ✅ `cafes_restaurants.csv` - 座位容量分析
- ✅ `area_summary.csv` - 区域对比

### 仪表板 4: 艺术品分布
**推荐使用的文件**：
- ✅ `public_artworks.csv` - 主要数据
- ✅ `melbourne_pois.csv` - 包含纪念碑

---

## ⚠️ 常见错误

### 错误 1: 找不到字段 total_venues
**正确做法**：使用 `count` 字段（在 area_summary.csv 中）

### 错误 2: 无法区分咖啡馆和酒吧
**正确做法**：
- 在 `dining_combined.csv` 中使用 `venue_type` 字段
- 在 `area_summary.csv` 中使用 `type` 字段

### 错误 3: 日期字段无法识别
**解决方案**：
- 在 Tableau 中右键字段 → "更改数据类型" → "日期"
- 或在导入时选择正确的数据类型

### 错误 4: 布尔值显示为 TRUE/FALSE
**美化方法**：
- 右键字段 → "别名"
- TRUE → "是" 或 "周末"
- FALSE → "否" 或 "工作日"

---

## 📚 快速检索

**需要制作地图？**
→ 使用包含 `latitude` 和 `longitude` 的文件

**需要按区域统计？**
→ 使用 `area_summary.csv`（字段：clue_area, type, count）

**需要时间趋势？**
→ 使用 `pedestrian_recent.csv` 或 `hourly_activity_patterns.csv`

**需要区分咖啡馆和酒吧？**
→ 使用 `dining_combined.csv`（字段：venue_type）

**需要最简单的人流数据？**
→ 使用 `hourly_activity_patterns.csv` ⭐

---

希望这份参考文档对你的Tableau制作有帮助！
