# ==============================================================================
# Process All Melbourne Data Files for Tableau
# ==============================================================================
# This script processes all Melbourne Open Data files and standardizes them
# for use in Tableau visualizations
# ==============================================================================

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(purrr)

cat("\n========================================\n")
cat("Processing All Melbourne Data Files\n")
cat("========================================\n\n")

# Create output directory
dir.create("data/tableau2", recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

# Function to standardize coordinate columns
standardize_coordinates <- function(df) {
  # Check if lat/lon columns exist
  has_lat <- any(grepl("^lat$|^latitude$", names(df), ignore.case = TRUE))
  has_lon <- any(grepl("^lon$|^longitude$", names(df), ignore.case = TRUE))
  
  if (has_lat && has_lon) {
    # Rename to standard names
    names(df)[grepl("^lat$|^latitude$", names(df), ignore.case = TRUE)] <- "latitude"
    names(df)[grepl("^lon$|^longitude$", names(df), ignore.case = TRUE)] <- "longitude"
  } else if (any(grepl("co-ordinates|location", names(df), ignore.case = TRUE))) {
    # Extract coordinates from string
    coord_col <- names(df)[grepl("co-ordinates|location", names(df), ignore.case = TRUE)][1]
    
    if (!is.null(coord_col)) {
      # Parse coordinates from string format "-37.xxx, 144.xxx"
      coord_data <- str_extract_all(df[[coord_col]], "-?\\d+\\.\\d+")
      
      df$latitude <- map_dbl(coord_data, ~ {
        if (length(.x) >= 2) as.numeric(.x[1]) else NA_real_
      })
      
      df$longitude <- map_dbl(coord_data, ~ {
        if (length(.x) >= 2) as.numeric(.x[2]) else NA_real_
      })
    }
  }
  
  return(df)
}

# Function to add CLUE area based on coordinates
add_clue_area <- function(df) {
  if (!"clue_area" %in% names(df)) {
    df$clue_area <- case_when(
      # Melbourne CBD
      df$latitude >= -37.82 & df$latitude <= -37.80 & 
      df$longitude >= 144.95 & df$longitude <= 144.98 ~ "Melbourne (CBD)",
      
      # Southbank
      df$latitude >= -37.83 & df$latitude <= -37.81 & 
      df$longitude >= 144.95 & df$longitude <= 144.99 ~ "Southbank",
      
      # Docklands
      df$latitude >= -37.82 & df$latitude <= -37.81 & 
      df$longitude >= 144.93 & df$longitude <= 144.96 ~ "Docklands",
      
      # Parkville
      df$latitude >= -37.80 & df$latitude <= -37.78 & 
      df$longitude >= 144.95 & df$longitude <= 144.97 ~ "Parkville",
      
      # Carlton
      df$latitude >= -37.81 & df$latitude <= -37.79 & 
      df$longitude >= 144.96 & df$longitude <= 144.98 ~ "Carlton",
      
      # East Melbourne
      df$latitude >= -37.82 & df$latitude <= -37.80 & 
      df$longitude >= 144.97 & df$longitude <= 144.99 ~ "East Melbourne",
      
      # North Melbourne
      df$latitude >= -37.81 & df$latitude <= -37.79 & 
      df$longitude >= 144.94 & df$longitude <= 144.96 ~ "North Melbourne",
      
      # West Melbourne
      df$latitude >= -37.81 & df$latitude <= -37.79 & 
      df$longitude >= 144.93 & df$longitude <= 144.95 ~ "West Melbourne",
      
      # South Yarra
      df$latitude >= -37.85 & df$latitude <= -37.83 & 
      df$longitude >= 144.98 & df$longitude <= 145.00 ~ "South Yarra",
      
      # Kensington
      df$latitude >= -37.80 & df$latitude <= -37.78 & 
      df$longitude >= 144.92 & df$longitude <= 144.94 ~ "Kensington",
      
      TRUE ~ "Other"
    )
  }
  return(df)
}

# ==============================================================================
# 1. PROCESS DRINKING FOUNTAINS
# ==============================================================================

cat("1. Processing drinking fountains...\n")

drinking_fountains <- read_csv("data/drinking-fountains.csv", show_col_types = FALSE) %>%
  standardize_coordinates() %>%
  add_clue_area() %>%
  mutate(
    # Remove "Drinking Fountain - " prefix from description
    description = str_replace(Description, "^Drinking Fountain - ", "")
  ) %>%
  select(
    description,
    latitude,
    longitude,
    clue_area
  ) %>%
  filter(!is.na(latitude), !is.na(longitude))

write_csv(drinking_fountains, "data/tableau2/drinking_fountains.csv")
cat("  Processed:", nrow(drinking_fountains), "drinking fountains\n")

# ==============================================================================
# 2. PROCESS LANDMARKS AND PLACES OF INTEREST
# ==============================================================================

cat("2. Processing landmarks and places of interest...\n")

landmarks <- read_csv("data/landmarks-and-places-of-interest-including-schools-theatres-health-services-spor.csv", 
                     show_col_types = FALSE) %>%
  standardize_coordinates() %>%
  add_clue_area() %>%
  select(
    theme = Theme,
    sub_theme = `Sub Theme`,
    feature_name = `Feature Name`,
    latitude,
    longitude,
    clue_area
  ) %>%
  filter(!is.na(latitude), !is.na(longitude))

write_csv(landmarks, "data/tableau2/landmarks.csv")
cat("  Processed:", nrow(landmarks), "landmarks\n")

# ==============================================================================
# 3. PROCESS PUBLIC ARTWORKS, FOUNTAINS AND MONUMENTS
# ==============================================================================

cat("3. Processing public artworks, fountains and monuments...\n")

artworks <- read_csv("data/public-artworks-fountains-and-monuments.csv", 
                    show_col_types = FALSE) %>%
  standardize_coordinates() %>%
  add_clue_area() %>%
  select(
    asset_type = `Asset Type`,
    name = Name,
    artist = Artist,
    art_date = `Art Date`,
    structure = Structure,
    latitude,
    longitude,
    clue_area
  ) %>%
  # Remove Easting and Northing columns as requested
  filter(!is.na(latitude), !is.na(longitude))

write_csv(artworks, "data/tableau2/public_artworks.csv")
cat("  Processed:", nrow(artworks), "public artworks\n")

# ==============================================================================
# 4. PROCESS PUBLIC MEMORIALS AND SCULPTURES
# ==============================================================================

cat("4. Processing public memorials and sculptures...\n")

memorials <- read_csv("data/public-memorials-and-sculptures.csv", 
                     show_col_types = FALSE) %>%
  standardize_coordinates() %>%
  add_clue_area() %>%
  select(
    description = Description,
    title = Title,
    latitude,
    longitude,
    clue_area
  ) %>%
  filter(!is.na(latitude), !is.na(longitude))

write_csv(memorials, "data/tableau2/public_memorials.csv")
cat("  Processed:", nrow(memorials), "public memorials\n")

# ==============================================================================
# 5. PROCESS PUBLIC TOILETS
# ==============================================================================

cat("5. Processing public toilets...\n")

toilets <- read_csv("data/public-toilets.csv", show_col_types = FALSE) %>%
  standardize_coordinates() %>%
  add_clue_area() %>%
  mutate(
    # Remove "Public Toilet - " prefix from name
    name = str_replace(name, "^Public Toilet - ", "")
  ) %>%
  select(
    name,
    female,
    male,
    wheelchair,
    operator,
    baby_facil,
    latitude,
    longitude,
    clue_area
  ) %>%
  filter(!is.na(latitude), !is.na(longitude))

write_csv(toilets, "data/tableau2/public_toilets.csv")
cat("  Processed:", nrow(toilets), "public toilets\n")

# ==============================================================================
# 6. PROCESS BARS AND PUBS (already has CLUE area)
# ==============================================================================

cat("6. Processing bars and pubs...\n")

bars_pubs <- read_csv("data/bars-and-pubs-with-patron-capacity.csv", 
                     show_col_types = FALSE) %>%
  standardize_coordinates() %>%
  select(
    base_property_id = `Base property ID`,
    census_year = `Census year`,
    trading_name = `Trading name`,
    business_address = `Business address`,
    number_of_patrons = `Number of patrons`,
    latitude,
    longitude,
    clue_area = `CLUE small area`
  ) %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  filter(latitude != 0, longitude != 0) %>%
  # 按Base property ID分组，只保留每个物理位置的最新记录
  group_by(base_property_id) %>%
  filter(census_year == max(census_year)) %>%
  ungroup() %>%
  # 去重：按trading_name和business_address去重
  distinct(trading_name, business_address, .keep_all = TRUE) %>%
  # 重命名trading_name为name
  rename(name = trading_name)

write_csv(bars_pubs, "data/tableau2/bars_pubs.csv")
cat("  Processed:", nrow(bars_pubs), "bars and pubs (by Base property ID, latest year only)\n")

# ==============================================================================
# 7. PROCESS CAFES AND RESTAURANTS (already has CLUE area)
# ==============================================================================

cat("7. Processing cafes and restaurants...\n")

cafes_restaurants <- read_csv("data/cafes-and-restaurants-with-seating-capacity.csv", 
                             show_col_types = FALSE) %>%
  standardize_coordinates() %>%
  select(
    base_property_id = `Base property ID`,
    census_year = `Census year`,
    trading_name = `Trading name`,
    business_address = `Business address`,
    industry_code = `Industry (ANZSIC4) code`,
    industry_desc = `Industry (ANZSIC4) description`,
    seating_type = `Seating type`,
    number_of_seats = `Number of seats`,
    latitude,
    longitude,
    clue_area = `CLUE small area`
  ) %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  filter(latitude != 0, longitude != 0) %>%
  # 按Base property ID分组，只保留每个物理位置的最新记录
  group_by(base_property_id) %>%
  filter(census_year == max(census_year)) %>%
  ungroup() %>%
  # 去重：按trading_name和business_address去重
  distinct(trading_name, business_address, .keep_all = TRUE) %>%
  # 重命名trading_name为name
  rename(name = trading_name)

write_csv(cafes_restaurants, "data/tableau2/cafes_restaurants.csv")
cat("  Processed:", nrow(cafes_restaurants), "cafes and restaurants (by Base property ID, latest year only)\n")

# ==============================================================================
# CREATE SUMMARY STATISTICS
# ==============================================================================

cat("\n8. Creating summary statistics...\n")

summary_stats <- data.frame(
  dataset = c("Drinking Fountains", "Landmarks", "Public Artworks", 
             "Public Memorials", "Public Toilets", "Bars & Pubs", "Cafes & Restaurants"),
  records = c(nrow(drinking_fountains), nrow(landmarks), nrow(artworks),
             nrow(memorials), nrow(toilets), nrow(bars_pubs), nrow(cafes_restaurants)),
  stringsAsFactors = FALSE
)

write_csv(summary_stats, "data/tableau2/summary_stats.csv")

# Create detailed area summary with industry breakdown
cat("Creating detailed area summary with industry breakdown...\n")

# Create industry breakdown for cafes and restaurants
cafes_industry_breakdown <- cafes_restaurants %>%
  mutate(
    # Create main industry categories
    main_industry = case_when(
      industry_desc == "Cafes and Restaurants" ~ "Cafes and Restaurants",
      industry_desc == "Takeaway Food Services" ~ "Takeaway Food Services", 
      industry_desc == "Pubs, Taverns and Bars" ~ "Pubs, Taverns and Bars",
      industry_desc == "Accommodation" ~ "Accommodation",
      industry_desc == "Bakery Product Manufacturing (Non-factory based)" ~ "Bakery Manufacturing",
      # Merge these three categories
      industry_desc == "Other Specialised Food Retailing" ~ "Other Food & Hospitality",
      industry_desc == "Clubs (Hospitality)" ~ "Other Food & Hospitality",
      industry_desc == "Catering Services" ~ "Other Food & Hospitality",
      industry_desc == "Sports and Physical Recreation Venues, Grounds and Facilities Operation" ~ "Sports & Recreation",
      industry_desc == "Internet Service Providers and Web Search Portals" ~ "Internet Services",
      TRUE ~ "Other Services"
    ),
    # For Cafes and Restaurants, add seating type breakdown
    detailed_category = case_when(
      industry_desc == "Cafes and Restaurants" & seating_type == "Seats - Indoor" ~ "Cafes_Restaurants_Indoor",
      industry_desc == "Cafes and Restaurants" & seating_type == "Seats - Outdoor" ~ "Cafes_Restaurants_Outdoor",
      industry_desc == "Cafes and Restaurants" & is.na(seating_type) ~ "Cafes_Restaurants_Unknown",
      TRUE ~ main_industry
    )
  ) %>%
  count(clue_area, detailed_category, name = "count")

# Create area summary with detailed breakdown
area_summary_detailed <- bind_rows(
  drinking_fountains %>% count(clue_area, name = "drinking_fountains"),
  landmarks %>% count(clue_area, name = "landmarks"),
  artworks %>% count(clue_area, name = "artworks"),
  memorials %>% count(clue_area, name = "memorials"),
  toilets %>% count(clue_area, name = "toilets"),
  bars_pubs %>% count(clue_area, name = "bars_pubs"),
  # Use the detailed breakdown for cafes and restaurants
  cafes_industry_breakdown %>% 
    pivot_wider(names_from = detailed_category, values_from = count, values_fill = 0)
) %>%
  group_by(clue_area) %>%
  summarise_all(sum, na.rm = TRUE) %>%
  mutate(total = rowSums(select(., -clue_area), na.rm = TRUE)) %>%
  arrange(desc(total))

# Create simplified area summary (original)
area_summary <- bind_rows(
  drinking_fountains %>% count(clue_area, name = "drinking_fountains"),
  landmarks %>% count(clue_area, name = "landmarks"),
  artworks %>% count(clue_area, name = "artworks"),
  memorials %>% count(clue_area, name = "memorials"),
  toilets %>% count(clue_area, name = "toilets"),
  bars_pubs %>% count(clue_area, name = "bars_pubs"),
  cafes_restaurants %>% count(clue_area, name = "cafes_restaurants")
) %>%
  group_by(clue_area) %>%
  summarise_all(sum, na.rm = TRUE) %>%
  mutate(total = rowSums(select(., -clue_area), na.rm = TRUE)) %>%
  arrange(desc(total))

write_csv(area_summary, "data/tableau2/area_summary.csv")
write_csv(area_summary_detailed, "data/tableau2/area_summary_detailed.csv")

cat("\n========================================\n")
cat("Data Processing Complete!\n")
cat("Files saved to data/tableau2/\n")
cat("========================================\n\n")

cat("Summary:\n")
print(summary_stats)
cat("\nTop areas by total facilities:\n")
print(head(area_summary, 10))
