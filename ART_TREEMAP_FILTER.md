在 Shiny 中用 Tableau 树状图联动筛选“文化景点”地图

本文档说明如何将“艺术品分布 - Tableau 可视化”替换为一个树状图仪表盘，并实现：在树状图中点击艺术家后，联动筛选“景点地图”（Leaflet）中的点。

适用环境：本仓库的 `app.R` + `tableau-in-shiny-v1.2.R`。

---

目标

- 将“艺术品分布”区域替换为新的 Tableau 树状图（按艺术家聚合）。
- 当用户在树状图中选择某位艺术家（一个或多个），`attractions_map` 只显示该艺术家创作的公共艺术（以及可选的喷泉/纪念碑）。

前置条件

- 新的树状图已发布到 Tableau Public，且 treemap 的标记包含“艺术家名字”字段（如 `Artist` / `artist` / `Artist Name` / `作者` / `艺术家` 等）。
- `tableau-in-shiny-v1.2.R` 已在 `ui` 中通过 `setUpTableauInShiny()` 启用（仓库已配置）。

---

第一步：替换“艺术品分布”仪表盘为树状图

在 `app.R` 的“文化景点”页签中找到以下 UI 片段（`id` 必须是 `art_viz`，保持不变）：

```
tableauPublicViz(
  id = "art_viz",
  url = "https://public.tableau.com/shared/JBKS2Z28K?:display_count=n&:origin=viz_share_link",
  height = "600px",
  width  = "2000px",
  device = "desktop",
  toolbar = "hidden"
)
```

将其中的 `url` 换成你新的“树状图”Workbook/Sheet 的嵌入地址。其它参数可按需要调整。`id` 不要改动，因为后续联动依赖该 `id`。

---

第二步：接收 Tableau 的选中事件

`tableau-in-shiny-v1.2.R` 已把 Tableau 事件（如 MarkSelectionChanged）桥接到 Shiny Input。

- 对于上面的 `id = "art_viz"`，在服务器端会收到 `input$art_viz_mark_selection_changed` 事件。
- 事件负载是“选中标记”的表格数据（列名来自 Tableau 字段名）。

在 `server` 中增加一个用于保存选中艺术家的响应式变量，并监听该事件：

```
# server 内，初始化响应式变量
rv <- reactiveValues(
  # ...已有内容...
  selected_artists = NULL
)

# 监听 Tableau 树状图的选中变化
observeEvent(input$art_viz_mark_selection_changed, {
  sel <- input$art_viz_mark_selection_changed

  # 空选择 => 清除筛选
  if (is.null(sel) || length(sel) == 0) {
    rv$selected_artists <- NULL
    return()
  }

  # 将负载转为 data.frame（兼容 list/data.frame 两种形态）
  df <- tryCatch(as.data.frame(sel, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(df) || nrow(df) == 0) {
    rv$selected_artists <- NULL
    return()
  }

  # 尝试从常见列名中识别“艺术家”字段
  artistFieldCandidates <- c("Artist", "artist", "Artist Name", "ARTIST", "作者", "艺术家")
  artist_col <- intersect(artistFieldCandidates, colnames(df))

  # 若未匹配到，兜底：选择第一个字符型列
  if (length(artist_col) == 0) {
    char_cols <- names(df)[vapply(df, is.character, logical(1))]
    artist_col <- if (length(char_cols) > 0) char_cols[1] else character(0)
  }

  if (length(artist_col) == 0) {
    rv$selected_artists <- NULL
    return()
  }

  # 提取去重后的艺术家名向量
  artists <- unique(na.omit(df[[artist_col[1]]]))
  rv$selected_artists <- if (length(artists) > 0) artists else NULL
}, ignoreInit = TRUE)
```

说明：上述代码会把 Tableau 选中的艺术家名列表放进 `rv$selected_artists`。清空选中（在 Tableau 中点空白或按 Esc）则取消筛选。

---

第三步：让“景点地图”按艺术家筛选

在 `output$attractions_map <- renderLeaflet({ ... })` 中，引入 `rv$selected_artists`，并在为各类别添加标记前做过滤：

```
output$attractions_map <- renderLeaflet({
  map <- leaflet() %>%
    addProviderTiles(providers$CartoDB.Voyager) %>%
    setView(lng = 144.9631, lat = -37.8136, zoom = 13)

  selected_types <- input$attraction_types
  selected_artists <- rv$selected_artists   # <- 新增：联动筛选

  if (is.null(selected_types) || length(selected_types) == 0) {
    return(map)
  }

  # Art（来自 data$artworks）
  if ("Art" %in% selected_types) {
    art_data <- dplyr::filter(data$artworks, category == "Art")
    if (!is.null(selected_artists) && length(selected_artists) > 0 && "artist" %in% names(art_data)) {
      art_data <- dplyr::filter(art_data, artist %in% selected_artists)
    }
    if (nrow(art_data) > 0) {
      map <- map %>% addCircleMarkers(
        data = art_data,
        lng = ~longitude, lat = ~latitude, radius = 8,
        color = "#9b59b6", fillOpacity = 0.7, stroke = TRUE, weight = 2,
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

  # Fountain（来自 data$artworks）
  if ("Fountain" %in% selected_types) {
    fountain_data <- dplyr::filter(data$artworks, category == "Fountain")
    if (!is.null(selected_artists) && length(selected_artists) > 0 && "artist" %in% names(fountain_data)) {
      fountain_data <- dplyr::filter(fountain_data, artist %in% selected_artists)
    }
    if (nrow(fountain_data) > 0) {
      map <- map %>% addCircleMarkers(
        data = fountain_data,
        lng = ~longitude, lat = ~latitude, radius = 8,
        color = "#1abc9c", fillOpacity = 0.7, stroke = TRUE, weight = 2,
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

  # Monument（来自 data$artworks）
  if ("Monument" %in% selected_types) {
    monument_data <- dplyr::filter(data$artworks, category == "Monument")
    if (!is.null(selected_artists) && length(selected_artists) > 0 && "artist" %in% names(monument_data)) {
      monument_data <- dplyr::filter(monument_data, artist %in% selected_artists)
    }
    if (nrow(monument_data) > 0) {
      map <- map %>% addCircleMarkers(
        data = monument_data,
        lng = ~longitude, lat = ~latitude, radius = 8,
        color = "#e74c3c", fillOpacity = 0.7, stroke = TRUE, weight = 2,
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

  # Memorial（来自 data$pois，可选按 artist 过滤，若有该列）
  if ("Memorial" %in% selected_types) {
    sculpture_data <- dplyr::filter(data$pois, type == "Memorial")
    if (!is.null(selected_artists) && length(selected_artists) > 0 && "artist" %in% names(sculpture_data)) {
      sculpture_data <- dplyr::filter(sculpture_data, artist %in% selected_artists)
    }
    if (nrow(sculpture_data) > 0) {
      map <- map %>% addCircleMarkers(
        data = sculpture_data,
        lng = ~longitude, lat = ~latitude, radius = 8,
        color = "#f39c12", fillOpacity = 0.7, stroke = TRUE, weight = 2,
        popup = ~paste0(
          "<strong>", name, "</strong><br/>",
          "类别: ", category, "<br/>",
          ifelse(!is.na(artist), paste0("艺术家: ", artist, "<br/>"), "")
        )
      )
    }
  }

  map
})
```

注意：我们只在“对应数据集存在 `artist` 列”时才应用艺术家过滤，避免无该列时报错。

---

第四步（可选）：在 UI 上提示当前过滤状态

你可以在“艺术品分布”区域或地图上方加个小提示，显示当前选中的艺术家，并提供“清除筛选”的引导（在 Tableau 图上点空白或按 Esc）。这一步不影响功能，可按需增加。

---

验证

1. 运行 Shiny 应用，打开“文化景点”。
2. 树状图中点击某位艺术家的方块，观察地图上仅保留其作品位置。
3. 在 Tableau 中清空选择（点空白或 Esc），地图恢复显示。
4. 同时勾选/取消左侧“显示类型”，确保与艺术家筛选共同生效。

---

常见问题

- 树状图字段名不是 `Artist` 怎么办？
  - 上述代码对常见中文/英文字段名都做了兼容；若仍不匹配，在 `artistFieldCandidates` 中增加你的字段名即可。

- 只能筛选“Art”，不能筛选“Fountain/Monument”？
  - 请确认这些类别在源数据中也有 `artist` 列。如果没有，就不会套用艺术家过滤；你可以按需要只在 Art 上应用过滤。

- 没有触发联动？
  - 确认 `id = "art_viz"` 未改动；确认 `setUpTableauInShiny()` 已在 `ui` 中调用；确认 treemap 的标记是可选中的（不是纯注释或标题）。

---

最小改动清单

1) 在 UI 中用新的 `url` 替换 `art_viz` 所在的 `tableauPublicViz()`。

2) 在 `server`：

```
# rv 新增字段
rv <- reactiveValues(
  # ...
  selected_artists = NULL
)

# 监听 art_viz 选中事件（解析艺术家）
observeEvent(input$art_viz_mark_selection_changed, { ... 同上 ... })

# output$attractions_map 中引用 rv$selected_artists 做过滤（各类别）
```

完成以上三步即可实现“树状图点选艺术家 => 地图筛选对应点”的联动效果。

