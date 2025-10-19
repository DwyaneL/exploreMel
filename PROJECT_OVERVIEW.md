# Melbourne Explorer - Project Overview

## 项目概述

**项目名称**: Melbourne Explorer - Interactive Tourism Interface
**课程**: GEOM90007 Information Visualisation - Assignment 3
**目标用户**: 访问墨尔本的游客
**技术栈**: R Shiny (主界面) + Tableau Public (嵌入式可视化)

## 项目目标

创建一个交互式网页应用,帮助游客:
1. 探索墨尔本的景点和兴趣点
2. 发现餐饮和咖啡馆
3. 了解公共交通系统
4. 规划个性化行程

## 当前项目结构

```
D:\ChromeDownload\A3\
├── app.R                          # 主Shiny应用 ✓
├── tableau-in-shiny-v1.2.R       # Tableau集成库 ✓
├── README.txt                     # 使用说明 ✓
├── DATA_GUIDE.md                  # 数据获取指南 ✓
├── TABLEAU_GUIDE.md               # Tableau可视化指南 ✓
├── PROJECT_OVERVIEW.md            # 本文件 ✓
│
├── data/
│   ├── raw/                      # 原始下载数据 (待创建)
│   ├── processed/                # 处理后的数据 (待创建)
│   └── tableau/                  # Tableau用CSV (待创建)
│
├── scripts/
│   ├── prepare_data.R            # 数据处理脚本 ✓
│   └── export_for_tableau.R      # Tableau导出脚本 ✓
│
└── www/                          # 静态资源文件夹 (可选)
```

## 已完成的工作

### ✅ 第一阶段: 项目搭建
1. **需求分析** - 确定目标用户(游客)和功能需求
2. **技术选型** - R Shiny + Tableau Public
3. **项目结构** - 创建文件夹和基础框架
4. **主应用开发** - app.R包含6个主要功能模块:
   - Welcome (欢迎页)
   - Explore Map (探索地图)
   - Attractions (景点)
   - Dining & Cafes (餐饮)
   - Transportation (交通)
   - Plan Your Visit (行程规划)

### ✅ 第二阶段: 文档和指南
5. **README.txt** - 详细的安装和运行说明
6. **DATA_GUIDE.md** - 数据源和获取步骤
7. **TABLEAU_GUIDE.md** - Tableau可视化创建指南
8. **数据处理脚本** - 自动化数据准备流程
9. **导出脚本** - 为Tableau准备CSV格式数据

## 下一步行动计划

### 📋 第三阶段: 数据准备

#### 1. 下载真实数据 (优先级: 高)

访问以下数据源并下载数据:

**必需数据集**:
- [ ] **POIs**: https://data.melbourne.vic.gov.au/
  - 搜索: "Points of Interest" 或 "Landmarks"
  - 格式: GeoJSON 或 CSV
  - 保存到: `data/raw/pois.geojson`

- [ ] **Cafes**: https://data.melbourne.vic.gov.au/
  - 搜索: "Cafes and restaurants with seating"
  - 保存到: `data/raw/cafes.csv`

- [ ] **Transport**: https://data.vic.gov.au/
  - 搜索: "PTV Metro Train Stations" 和 "Tram Stops"
  - 保存到: `data/raw/transport_stops.geojson`

**可选但推荐**:
- [ ] **Pedestrian Counts**: 用于分析访客模式
  - 搜索: "Pedestrian Counting System"
  - 保存到: `data/raw/pedestrian_counts.csv`

#### 2. 处理数据

```r
# 运行数据处理脚本
source("scripts/prepare_data.R")

# 导出给Tableau使用
source("scripts/export_for_tableau.R")
```

#### 3. 更新app.R以加载真实数据

修改`app.R`中的`load_data()`函数:
```r
load_data <- function() {
  list(
    pois = readRDS("data/processed/melbourne_pois.rds"),
    cafes = readRDS("data/processed/cafes_restaurants.rds"),
    transport = readRDS("data/processed/transport_network.rds"),
    pedestrian = readRDS("data/processed/pedestrian_counts.rds")
  )
}
```

### 📊 第四阶段: Tableau可视化

#### 4. 创建Tableau仪表板 (优先级: 高)

需要创建3-4个Tableau Public仪表板:

**Dashboard 1: Welcome Overview**
- [ ] 创建工作簿: "Melbourne Explorer - Welcome"
- [ ] 添加可视化:
  - 顶级景点条形图
  - POI密度热力图
  - 关键指标卡片
- [ ] 发布到Tableau Public
- [ ] 记录URL

**Dashboard 2: Attractions Analysis**
- [ ] 创建工作簿: "Melbourne Explorer - Attractions"
- [ ] 添加可视化:
  - 景点类别树状图
  - 访客评分散点图
  - 时间趋势线图
- [ ] 添加交互式筛选器
- [ ] 发布到Tableau Public
- [ ] 记录URL

**Dashboard 3: Dining Trends**
- [ ] 创建工作簿: "Melbourne Explorer - Dining"
- [ ] 添加可视化:
  - 美食类型分布
  - 评分vs价格散点图
  - 地理分布地图
- [ ] 发布并记录URL

**Dashboard 4: Transport Patterns** (可选)
- [ ] 创建工作簿: "Melbourne Explorer - Transport"
- [ ] 添加可视化:
  - 使用模式热图
  - 高峰时段线图
- [ ] 发布并记录URL

#### 5. 嵌入Tableau到Shiny

在`app.R`中,找到并替换注释掉的代码:

```r
# 替换这里:
# tableauPublicViz(
#   id = "welcome_dashboard",
#   url = "YOUR_TABLEAU_PUBLIC_URL_HERE",
#   height = "350px"
# )

# 改为:
tableauPublicViz(
  id = "welcome_dashboard",
  url = "https://public.tableau.com/views/MelbourneExplorer-Welcome/Dashboard1",
  height = "600px"
)
```

对所有仪表板重复此操作。

### 🔧 第五阶段: 高级功能

#### 6. 实现高级交互 (优先级: 中)

- [ ] **Shiny → Tableau**: 当用户在Shiny中选择筛选器时,更新Tableau可视化
- [ ] **Tableau → Shiny**: 当用户在Tableau中选择标记时,更新Shiny地图
- [ ] **行程规划算法**: 基于用户偏好生成推荐路线
- [ ] **实时数据**: 如果可能,集成实时交通数据

示例代码已在`app.R`中提供(被注释)。

#### 7. 改进用户界面

- [ ] 添加自定义CSS样式 (www/custom.css)
- [ ] 添加图标和图片
- [ ] 优化移动端响应式设计
- [ ] 添加加载动画

### 🧪 第六阶段: 测试和优化

#### 8. 功能测试

- [ ] 测试所有交互功能
- [ ] 验证数据加载正确
- [ ] 检查Tableau可视化显示
- [ ] 测试筛选器和用户输入
- [ ] 验证地图标记点击事件

#### 9. 性能优化

- [ ] 优化数据加载速度
- [ ] 减少不必要的响应式计算
- [ ] 测试不同浏览器兼容性

### 📦 第七阶段: 最终提交

#### 10. 准备提交文件

- [ ] 确保README.txt准确描述如何运行
- [ ] 检查所有数据文件包含在内
- [ ] 验证tableau-in-shiny-v1.2.R存在
- [ ] 删除不必要的临时文件

#### 11. 创建ZIP文件

```r
# 可以用R创建ZIP
files_to_zip <- c(
  "app.R",
  "tableau-in-shiny-v1.2.R",
  "README.txt",
  "data/",
  "scripts/",
  "www/"
)

zip("MelbourneExplorer_Submission.zip", files_to_zip)
```

或手动压缩整个项目文件夹。

#### 12. 撰写报告 (Assignment要求)

创建PDF报告包含:

**Section I: Design Summary** (最多2页)
- 界面功能说明
- 工作原理
- 设计亮点(希望获得分数的功能)
- 设计决策理由(配色、图表类型选择等)

**Section II: Pattern/Use Case Summary** (最多2页)
- 界面帮助发现的有趣信息或模式
- 工具如何帮助发现这些模式
- 融入课程概念的说明

**Section III: Sources and References**
- 数据源列表
- 引用资料

**Section IV: Group Member Contribution Table**
- 每位成员的贡献
- 参与质量描述
- 贡献百分比

## 技术创新点 (超越基础要求)

为了获得高分,以下技术挑战已实现或计划实现:

### 已实现
1. ✅ **Tableau API v3集成**: 使用最新的嵌入API
2. ✅ **模块化设计**: 清晰的数据处理流程
3. ✅ **多页面仪表板**: 6个功能模块

### 待实现
4. ⏳ **双向通信**: Shiny ↔ Tableau实时交互
5. ⏳ **高级地图功能**: 自定义标记、聚类、热力图
6. ⏳ **智能推荐算法**: 基于用户偏好的行程规划
7. ⏳ **数据可视化创新**: 自定义ggplot2/ggiraph可视化
8. ⏳ **响应式设计**: 适配手机/平板/桌面

## 时间估算

| 阶段 | 任务 | 预计时间 |
|------|------|----------|
| 3 | 下载和处理数据 | 2-3小时 |
| 4 | 创建Tableau可视化 | 4-6小时 |
| 5 | 高级交互功能 | 3-4小时 |
| 6 | 测试和优化 | 2-3小时 |
| 7 | 报告撰写 | 3-4小时 |
| 总计 | | 14-20小时 |

## 关键资源

### 数据源
- City of Melbourne: https://data.melbourne.vic.gov.au/
- Victorian Government: https://data.vic.gov.au/
- Tableau Public: https://public.tableau.com/

### 学习资源
- Tableau Embedding API v3: https://help.tableau.com/current/api/embedding_api/en-us/
- Shiny Documentation: https://shiny.rstudio.com/
- Leaflet for R: https://rstudio.github.io/leaflet/

## 快速开始指南

如果你现在就想开始工作:

### 立即可做
1. **测试示例应用**:
   ```r
   # 首先安装必需的包
   install.packages(c("shiny", "shinydashboard", "leaflet", "dplyr",
                      "sf", "htmltools", "shinyjs"))

   # 运行应用(使用示例数据)
   shiny::runApp()
   ```

2. **生成示例数据**:
   ```r
   source("scripts/prepare_data.R")
   ```

3. **开始下载真实数据** (见上文第三阶段)

### 本周目标
- [ ] 下载所有必需数据集
- [ ] 运行数据处理脚本
- [ ] 创建至少1个Tableau仪表板
- [ ] 测试应用基本功能

### 下周目标
- [ ] 完成所有Tableau可视化
- [ ] 实现高级交互功能
- [ ] 开始撰写报告

## 常见问题

### Q: 我需要Tableau Desktop吗?
A: 不一定。Tableau Public (免费)足够完成此作业。但Desktop版本更强大。

### Q: 数据必须是真实的吗?
A: 是的,应该使用真实的Melbourne数据。但开发过程中可以先用示例数据测试。

### Q: 如果Tableau可视化不显示怎么办?
A: 检查:
1. URL是否正确
2. 仪表板是否设为Public
3. 浏览器控制台是否有错误
4. 网络连接是否正常

### Q: 需要多少个Tableau可视化?
A: 要求是"one or more"，建议3-4个仪表板,每个包含多个图表。

## 联系和协作

如果你是团队成员:
- 定期开会讨论进度
- 使用Git进行版本控制
- 明确分工(数据/Tableau/Shiny/报告)
- 及时沟通遇到的问题

## 提交前检查清单

- [ ] 应用可以成功运行
- [ ] 所有Tableau可视化正常显示
- [ ] README.txt准确无误
- [ ] 数据文件完整
- [ ] 代码有适当注释
- [ ] 报告已完成
- [ ] ZIP文件已创建
- [ ] 在另一台电脑上测试过

## 最后的话

这个项目框架为你提供了一个坚实的起点。关键是:
1. **尽早开始** - 特别是数据收集和Tableau可视化
2. **循序渐进** - 先让基本功能工作,再添加高级特性
3. **频繁测试** - 每次修改后都测试
4. **寻求帮助** - 遇到问题及时在Ed Discussion或Lab中提问

祝你作业顺利! 🚀
