# ==============================================================================
# Export Data for Tableau
# ==============================================================================
# This script exports processed R data to CSV format for use in Tableau
# Based on actual Melbourne Open Data
# ==============================================================================

library(dplyr)
library(readr)
library(lubridate)

cat("\n========================================\n")
cat("Exporting Data for Tableau\n")
cat("========================================\n\n")

# Create output directory
dir.create("data/tableau", recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# 1. Export Cafes and Restaurants
# ==============================================================================

cat("1. Exporting cafes and restaurants...\n")

if (file.exists("data/processed/cafes_restaurants.rds")) {
  cafes <- readRDS("data/processed/cafes_restaurants.rds")

  cafes_export <- cafes %>%
    select(
      trading_name,
      business_address,
      industry_desc,
      seating_type,
      num_seats,
      longitude,
      latitude,
      clue_area,
      seating_category,
      has_outdoor,
      census_year
    )

  write_csv(cafes_export, "data/tableau/cafes_restaurants.csv")
  cat("  Exported:", nrow(cafes_export), "cafes/restaurants\n")
} else {
  cat("  WARNING: Cafe data not found. Run prepare_data.R first.\n")
}

# ==============================================================================
# 2. Export Bars and Pubs
# ==============================================================================

cat("\n2. Exporting bars and pubs...\n")

if (file.exists("data/processed/bars_pubs.rds")) {
  bars <- readRDS("data/processed/bars_pubs.rds")

  bars_export <- bars %>%
    select(
      trading_name,
      business_address,
      num_patrons,
      longitude,
      latitude,
      clue_area,
      capacity_category,
      census_year
    )

  write_csv(bars_export, "data/tableau/bars_pubs.csv")
  cat("  Exported:", nrow(bars_export), "bars/pubs\n")
} else {
  cat("  WARNING: Bar data not found. Run prepare_data.R first.\n")
}

# ==============================================================================
# 3. Export Combined Dining (Cafes + Bars)
# ==============================================================================

cat("\n3. Creating combined dining dataset...\n")

if (exists("cafes") && exists("bars")) {
  dining_combined <- bind_rows(
    cafes %>%
      mutate(
        venue_type = "Cafe/Restaurant",
        capacity = num_seats
      ) %>%
      select(trading_name, business_address, venue_type, capacity,
             longitude, latitude, clue_area, census_year),

    bars %>%
      mutate(
        venue_type = "Bar/Pub",
        capacity = num_patrons
      ) %>%
      select(trading_name, business_address, venue_type, capacity,
             longitude, latitude, clue_area, census_year)
  )

  write_csv(dining_combined, "data/tableau/dining_combined.csv")
  cat("  Exported combined dining dataset:", nrow(dining_combined), "venues\n")
}

# ==============================================================================
# 4. Export Public Artworks
# ==============================================================================

cat("\n4. Exporting public artworks...\n")

if (file.exists("data/processed/public_artworks.rds")) {
  artworks <- readRDS("data/processed/public_artworks.rds")

  artworks_export <- artworks %>%
    select(
      name,
      artist,
      art_date,
      address,
      category,
      latitude,
      longitude
    )

  write_csv(artworks_export, "data/tableau/public_artworks.csv")
  cat("  Exported:", nrow(artworks_export), "artworks\n")
}

# ==============================================================================
# 5. Export POIs (Combined Attractions)
# ==============================================================================

cat("\n5. Exporting combined POIs...\n")

if (file.exists("data/processed/melbourne_pois.rds")) {
  pois <- readRDS("data/processed/melbourne_pois.rds")

  pois_export <- pois %>%
    select(
      poi_id,
      name,
      category,
      type,
      artist,
      address,
      description,
      latitude,
      longitude
    )

  write_csv(pois_export, "data/tableau/melbourne_pois.csv")
  cat("  Exported:", nrow(pois_export), "POIs\n")
}

# ==============================================================================
# 6. Export Pedestrian Count Summary
# ==============================================================================

cat("\n6. Exporting pedestrian count data...\n")

if (file.exists("data/processed/pedestrian_summary.rds")) {
  ped_summary <- readRDS("data/processed/pedestrian_summary.rds")

  ped_export <- ped_summary %>%
    select(
      sensor_name,
      hour,
      is_weekend,
      avg_count,
      max_count,
      min_count,
      latitude,
      longitude
    ) %>%
    mutate(
      day_type = ifelse(is_weekend, "Weekend", "Weekday")
    )

  write_csv(ped_export, "data/tableau/pedestrian_summary.csv")
  cat("  Exported pedestrian summary data\n")
}

# Also export a sample of raw pedestrian data for time series
if (file.exists("data/processed/pedestrian_counts.rds")) {
  ped_raw <- readRDS("data/processed/pedestrian_counts.rds")

  # Export recent data only (last 6 months) to keep file size manageable
  ped_recent <- ped_raw %>%
    filter(sensing_date >= max(sensing_date) - months(6)) %>%
    select(
      sensor_name,
      sensing_date,
      year,
      month,
      day_of_week,
      hour,
      total,
      is_weekend,
      latitude,
      longitude
    )

  write_csv(ped_recent, "data/tableau/pedestrian_recent.csv")
  cat("  Exported recent pedestrian counts:", nrow(ped_recent), "records\n")
}

# ==============================================================================
# 7. Export Public Facilities (Toilets + Fountains)
# ==============================================================================

cat("\n7. Exporting public facilities...\n")

if (file.exists("data/processed/public_toilets.rds")) {
  toilets <- readRDS("data/processed/public_toilets.rds")
  write_csv(toilets, "data/tableau/public_toilets.csv")
  cat("  Exported:", nrow(toilets), "public toilets\n")
}

if (file.exists("data/processed/drinking_fountains.rds")) {
  fountains <- readRDS("data/processed/drinking_fountains.rds")
  write_csv(fountains, "data/tableau/drinking_fountains.csv")
  cat("  Exported:", nrow(fountains), "drinking fountains\n")
}

# ==============================================================================
# 8. Create Overview/Summary Dataset
# ==============================================================================

cat("\n8. Creating overview summary...\n")

if (file.exists("data/processed/overview_stats.rds")) {
  overview <- readRDS("data/processed/overview_stats.rds")
  write_csv(overview, "data/tableau/overview_stats.csv")
  cat("  Exported overview statistics\n")
}

# Create area summaries
if (exists("cafes") && exists("bars")) {
  area_summary <- bind_rows(
    cafes %>%
      group_by(clue_area) %>%
      summarise(
        type = "Cafes",
        count = n(),
        total_capacity = sum(num_seats, na.rm = TRUE),
        avg_capacity = mean(num_seats, na.rm = TRUE),
        .groups = "drop"
      ),

    bars %>%
      group_by(clue_area) %>%
      summarise(
        type = "Bars",
        count = n(),
        total_capacity = sum(num_patrons, na.rm = TRUE),
        avg_capacity = mean(num_patrons, na.rm = TRUE),
        .groups = "drop"
      )
  )

  write_csv(area_summary, "data/tableau/area_summary.csv")
  cat("  Exported area summary statistics\n")
}

# ==============================================================================
# 9. Create Hourly Activity Patterns
# ==============================================================================

cat("\n9. Creating hourly activity patterns...\n")

if (file.exists("data/processed/pedestrian_summary.rds")) {
  ped_summary <- readRDS("data/processed/pedestrian_summary.rds")

  hourly_patterns <- ped_summary %>%
    group_by(hour, is_weekend) %>%
    summarise(
      avg_count = mean(avg_count, na.rm = TRUE),
      total_sensors = n(),
      .groups = "drop"
    ) %>%
    mutate(
      day_type = ifelse(is_weekend, "Weekend", "Weekday"),
      time_of_day = case_when(
        hour >= 6 & hour < 12 ~ "Morning",
        hour >= 12 & hour < 17 ~ "Afternoon",
        hour >= 17 & hour < 22 ~ "Evening",
        TRUE ~ "Night"
      )
    )

  write_csv(hourly_patterns, "data/tableau/hourly_activity_patterns.csv")
  cat("  Exported hourly activity patterns\n")
}

# ==============================================================================
# SUMMARY
# ==============================================================================

cat("\n========================================\n")
cat("Export Complete!\n")
cat("========================================\n\n")

cat("Files exported to data/tableau/:\n")
tableau_files <- list.files("data/tableau", pattern = "\\.csv$")
for (file in tableau_files) {
  cat("  -", file, "\n")
}

cat("\n========================================\n")
cat("Next Steps:\n")
cat("========================================\n")
cat("1. Open Tableau Desktop or Tableau Public\n")
cat("2. Connect to these CSV files\n")
cat("3. Create visualizations and dashboards\n")
cat("4. Publish to Tableau Public\n")
cat("5. Copy URLs and embed in app.R\n\n")

cat("Recommended Tableau Workbooks to Create:\n")
cat("----------------------------------------\n")
cat("1. 'Melbourne Dining Scene'\n")
cat("   - Use: dining_combined.csv, area_summary.csv\n")
cat("   - Viz: Map of cafes/bars, capacity by area, seating type distribution\n\n")

cat("2. 'Visitor Patterns & Activity'\n")
cat("   - Use: pedestrian_summary.csv, hourly_activity_patterns.csv\n")
cat("   - Viz: Hourly heatmap, popular locations, weekend vs weekday\n\n")

cat("3. 'Melbourne Attractions'\n")
cat("   - Use: melbourne_pois.csv, public_artworks.csv\n")
cat("   - Viz: Map of attractions, artwork categories, artist list\n\n")

cat("4. 'Tourist Amenities'\n")
cat("   - Use: public_toilets.csv, drinking_fountains.csv\n")
cat("   - Viz: Facility locations map, distribution by area\n\n")
