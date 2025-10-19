# ==============================================================================
# Data Preparation Script for Melbourne Explorer
# ==============================================================================
# This script processes the actual Melbourne Open Data files
# Data source: City of Melbourne Open Data Portal
# ==============================================================================

library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(stringr)

# ==============================================================================
# CONFIGURATION
# ==============================================================================

cat("\n========================================\n")
cat("Melbourne Explorer - Data Preparation\n")
cat("Processing actual Melbourne datasets\n")
cat("========================================\n\n")

# Create output directory
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# 1. PROCESS CAFES AND RESTAURANTS
# ==============================================================================

cat("1. Processing cafes and restaurants data...\n")

cafes_raw <- read_csv("data/cafes-and-restaurants-with-seating-capacity.csv",
                      show_col_types = FALSE)

# Clean and process cafe data
cafes_processed <- cafes_raw %>%
  # Get most recent year for each location
  group_by(`Base property ID`) %>%
  filter(`Census year` == max(`Census year`)) %>%
  ungroup() %>%
  # Clean column names
  select(
    census_year = `Census year`,
    trading_name = `Trading name`,
    business_address = `Business address`,
    industry_code = `Industry (ANZSIC4) code`,
    industry_desc = `Industry (ANZSIC4) description`,
    seating_type = `Seating type`,
    num_seats = `Number of seats`,
    longitude = Longitude,
    latitude = Latitude,
    clue_area = `CLUE small area`
  ) %>%
  # Filter valid coordinates
  filter(!is.na(longitude), !is.na(latitude)) %>%
  filter(longitude != 0, latitude != 0) %>%
  # Add category based on seating
  mutate(
    seating_category = case_when(
      num_seats < 30 ~ "Small (< 30 seats)",
      num_seats < 100 ~ "Medium (30-99 seats)",
      TRUE ~ "Large (100+ seats)"
    ),
    has_outdoor = str_detect(seating_type, "Outdoor")
  ) %>%
  # Remove duplicates
  distinct(trading_name, business_address, .keep_all = TRUE)

cat("  Processed:", nrow(cafes_processed), "cafes/restaurants\n")
cat("  With outdoor seating:", sum(cafes_processed$has_outdoor), "\n")

saveRDS(cafes_processed, "data/processed/cafes_restaurants.rds")

# ==============================================================================
# 2. PROCESS BARS AND PUBS
# ==============================================================================

cat("\n2. Processing bars and pubs data...\n")

bars_raw <- read_csv("data/bars-and-pubs-with-patron-capacity.csv",
                     show_col_types = FALSE)

bars_processed <- bars_raw %>%
  group_by(`Base property ID`) %>%
  filter(`Census year` == max(`Census year`)) %>%
  ungroup() %>%
  select(
    census_year = `Census year`,
    trading_name = `Trading name`,
    business_address = `Business address`,
    num_patrons = `Number of patrons`,
    longitude = Longitude,
    latitude = Latitude,
    clue_area = `CLUE small area`
  ) %>%
  filter(!is.na(longitude), !is.na(latitude)) %>%
  filter(longitude != 0, latitude != 0) %>%
  mutate(
    capacity_category = case_when(
      num_patrons < 50 ~ "Small (< 50)",
      num_patrons < 150 ~ "Medium (50-149)",
      TRUE ~ "Large (150+)"
    )
  ) %>%
  distinct(trading_name, business_address, .keep_all = TRUE)

cat("  Processed:", nrow(bars_processed), "bars/pubs\n")

saveRDS(bars_processed, "data/processed/bars_pubs.rds")

# ==============================================================================
# 3. PROCESS PUBLIC ARTWORKS
# ==============================================================================

cat("\n3. Processing public artworks data...\n")

artworks_raw <- read_csv("data/public-artworks-fountains-and-monuments.csv",
                        show_col_types = FALSE)

artworks_processed <- artworks_raw %>%
  select(
    asset_type = `Asset Type`,
    name = Name,
    artist = Artist,
    art_date = `Art Date`,
    address = `Address Point`,
    coordinates = `Co-ordinates`
  ) %>%
  # Extract lat/long from coordinates
  separate(coordinates, into = c("latitude", "longitude"), sep = ", ", remove = FALSE) %>%
  mutate(
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude)
  ) %>%
  filter(!is.na(longitude), !is.na(latitude)) %>%
  mutate(
    category = case_when(
      str_detect(tolower(asset_type), "sculpture") ~ "Sculpture",
      str_detect(tolower(asset_type), "fountain") ~ "Fountain",
      str_detect(tolower(asset_type), "monument") ~ "Monument",
      TRUE ~ "Art"
    )
  )

cat("  Processed:", nrow(artworks_processed), "artworks\n")

saveRDS(artworks_processed, "data/processed/public_artworks.rds")

# ==============================================================================
# 4. PROCESS PUBLIC MEMORIALS AND SCULPTURES
# ==============================================================================

cat("\n4. Processing memorials and sculptures...\n")

memorials_raw <- read_csv("data/public-memorials-and-sculptures.csv",
                         show_col_types = FALSE)

# 实际列名: Description, Title, Co-ordinates
memorials_processed <- memorials_raw %>%
  select(
    description = Description,
    name = Title,
    coordinates = `Co-ordinates`
  ) %>%
  separate(coordinates, into = c("latitude", "longitude"), sep = ", ", remove = FALSE) %>%
  mutate(
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude),
    category = "Memorial/Sculpture",
    artist = NA_character_,  # 该文件中没有艺术家信息
    address = NA_character_  # 该文件中没有地址信息
  ) %>%
  filter(!is.na(longitude), !is.na(latitude))

cat("  Processed:", nrow(memorials_processed), "memorials/sculptures\n")

saveRDS(memorials_processed, "data/processed/memorials_sculptures.rds")

# ==============================================================================
# 5. COMBINE ALL ATTRACTIONS (POIs)
# ==============================================================================

cat("\n5. Creating combined attractions dataset...\n")

# Combine artworks and memorials into a single POI dataset
pois_combined <- bind_rows(
  artworks_processed %>%
    select(name, category, artist, latitude, longitude, address) %>%
    mutate(type = "Artwork"),
  memorials_processed %>%
    select(name, category, artist, latitude, longitude, address) %>%
    mutate(type = "Memorial")
) %>%
  mutate(
    poi_id = row_number(),
    description = paste0("Artist: ", coalesce(artist, "Unknown"))
  )

cat("  Combined POIs:", nrow(pois_combined), "\n")

saveRDS(pois_combined, "data/processed/melbourne_pois.rds")

# ==============================================================================
# 6. PROCESS PUBLIC TOILETS
# ==============================================================================

cat("\n6. Processing public toilets...\n")

toilets_raw <- read_csv("data/public-toilets.csv", show_col_types = FALSE)

# 实际列名: name, female, male, wheelchair, operator, baby_facil, lat, lon, location
toilets_processed <- toilets_raw %>%
  select(
    name,
    female,
    male,
    wheelchair,
    operator,
    baby_facil,
    latitude = lat,
    longitude = lon,
    location
  ) %>%
  filter(!is.na(longitude), !is.na(latitude)) %>%
  mutate(
    # 创建设施类型描述
    facilities = paste(
      ifelse(female == "yes", "Female", ""),
      ifelse(male == "yes", "Male", ""),
      ifelse(wheelchair == "yes", "Wheelchair", ""),
      ifelse(baby_facil == "yes", "Baby Change", "")
    ) %>% str_trim() %>% str_squish()
  )

cat("  Processed:", nrow(toilets_processed), "public toilets\n")

saveRDS(toilets_processed, "data/processed/public_toilets.rds")

# ==============================================================================
# 7. PROCESS DRINKING FOUNTAINS
# ==============================================================================

cat("\n7. Processing drinking fountains...\n")

fountains_raw <- read_csv("data/drinking-fountains.csv", show_col_types = FALSE)

# 实际列名: Description, Co-ordinates, lat, lon
fountains_processed <- fountains_raw %>%
  select(
    description = Description,
    coordinates = `Co-ordinates`,
    latitude = lat,
    longitude = lon
  ) %>%
  filter(!is.na(longitude), !is.na(latitude)) %>%
  mutate(
    name = description,  # 使用描述作为名称
    type = "Drinking Fountain"
  )

cat("  Processed:", nrow(fountains_processed), "drinking fountains\n")

saveRDS(fountains_processed, "data/processed/drinking_fountains.rds")

# ==============================================================================
# 8. PROCESS PEDESTRIAN COUNTING DATA
# ==============================================================================

cat("\n8. Processing pedestrian counting data...\n")

pedestrian_raw <- read_csv("data/pedestrian-counting-system-monthly-counts-per-hour.csv",
                           show_col_types = FALSE)

# Process pedestrian data
pedestrian_processed <- pedestrian_raw %>%
  select(
    id = ID,
    location_id = Location_ID,
    sensing_date = Sensing_Date,
    hour = HourDay,
    direction_1 = Direction_1,
    direction_2 = Direction_2,
    total = Total_of_Directions,
    sensor_name = Sensor_Name,
    location = Location
  ) %>%
  mutate(
    sensing_date = ymd(sensing_date),
    year = year(sensing_date),
    month = month(sensing_date),
    # Use English labels for day of week - map numbers to ensure consistency
    day_of_week = factor(
      wday(sensing_date),
      levels = 1:7,
      labels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
    ),
    # Use numeric day of week to avoid locale issues (1=Sunday, 7=Saturday)
    is_weekend = wday(sensing_date) %in% c(1, 7)
  ) %>%
  # Extract coordinates from location if available
  separate(location, into = c("latitude", "longitude"), sep = ", ", remove = FALSE) %>%
  mutate(
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude)
  )

cat("  Processed:", nrow(pedestrian_processed), "pedestrian count records\n")

# Create summary by location and hour
pedestrian_summary <- pedestrian_processed %>%
  filter(year >= 2023) %>%  # Focus on recent data
  group_by(sensor_name, hour, is_weekend) %>%
  summarise(
    avg_count = mean(total, na.rm = TRUE),
    max_count = max(total, na.rm = TRUE),
    min_count = min(total, na.rm = TRUE),
    latitude = first(latitude),
    longitude = first(longitude),
    .groups = "drop"
  )

cat("  Created summary by hour and location\n")

saveRDS(pedestrian_processed, "data/processed/pedestrian_counts.rds")
saveRDS(pedestrian_summary, "data/processed/pedestrian_summary.rds")

# ==============================================================================
# 9. CREATE OVERVIEW STATISTICS
# ==============================================================================

cat("\n9. Creating overview statistics...\n")

overview_stats <- tibble(
  metric = c(
    "Total Cafes & Restaurants",
    "Total Bars & Pubs",
    "Total Public Artworks",
    "Total Memorials",
    "Total POIs",
    "Public Toilets",
    "Drinking Fountains",
    "Pedestrian Sensors",
    "Avg Cafe Seating",
    "Cafes with Outdoor Seating"
  ),
  value = c(
    nrow(cafes_processed),
    nrow(bars_processed),
    nrow(artworks_processed),
    nrow(memorials_processed),
    nrow(pois_combined),
    nrow(toilets_processed),
    nrow(fountains_processed),
    n_distinct(pedestrian_processed$sensor_name),
    round(mean(cafes_processed$num_seats, na.rm = TRUE)),
    sum(cafes_processed$has_outdoor)
  )
)

saveRDS(overview_stats, "data/processed/overview_stats.rds")

# ==============================================================================
# 10. CREATE METADATA
# ==============================================================================

metadata <- list(
  processed_date = Sys.time(),
  data_sources = list(
    cafes = list(
      file = "cafes-and-restaurants-with-seating-capacity.csv",
      count = nrow(cafes_processed),
      date_range = paste(min(cafes_processed$census_year), "-",
                        max(cafes_processed$census_year))
    ),
    bars = list(
      file = "bars-and-pubs-with-patron-capacity.csv",
      count = nrow(bars_processed)
    ),
    artworks = list(
      file = "public-artworks-fountains-and-monuments.csv",
      count = nrow(artworks_processed)
    ),
    memorials = list(
      file = "public-memorials-and-sculptures.csv",
      count = nrow(memorials_processed)
    ),
    pois = list(
      file = "combined from artworks and memorials",
      count = nrow(pois_combined)
    ),
    pedestrian = list(
      file = "pedestrian-counting-system-monthly-counts-per-hour.csv",
      count = nrow(pedestrian_processed),
      sensors = n_distinct(pedestrian_processed$sensor_name)
    )
  )
)

saveRDS(metadata, "data/processed/metadata.rds")

# ==============================================================================
# SUMMARY
# ==============================================================================

cat("\n========================================\n")
cat("Data Processing Complete!\n")
cat("========================================\n\n")

cat("Processed datasets:\n")
cat("  Cafes & Restaurants:", nrow(cafes_processed), "\n")
cat("  Bars & Pubs:", nrow(bars_processed), "\n")
cat("  Public Artworks:", nrow(artworks_processed), "\n")
cat("  Memorials:", nrow(memorials_processed), "\n")
cat("  Combined POIs:", nrow(pois_combined), "\n")
cat("  Public Toilets:", nrow(toilets_processed), "\n")
cat("  Drinking Fountains:", nrow(fountains_processed), "\n")
cat("  Pedestrian Records:", nrow(pedestrian_processed), "\n")
cat("  Pedestrian Sensors:", n_distinct(pedestrian_processed$sensor_name), "\n\n")

cat("Files saved to: data/processed/\n")
cat("Next step: Run source('scripts/export_for_tableau.R')\n\n")

cat("Key insights:\n")
cat("  - Avg cafe seating:", round(mean(cafes_processed$num_seats, na.rm = TRUE)), "seats\n")
cat("  - Cafes with outdoor seating:", sum(cafes_processed$has_outdoor),
    "(", round(100 * sum(cafes_processed$has_outdoor) / nrow(cafes_processed)), "%)\n")
cat("  - Most popular CLUE area:", names(sort(table(cafes_processed$clue_area), decreasing = TRUE))[1], "\n")
cat("\n")
