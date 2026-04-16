library(dplyr)
library(lubridate)
library(readr)
library(stringr)

clean_hobo_temp <- function(file, depth_m) {
  raw <- read.csv(file, skip = 1, header = TRUE)
  
  datetime_col <- names(raw)[str_detect(names(raw), "Date.Time")]
  temp_col     <- names(raw)[str_detect(names(raw), "Temp")]
  
  cleaned <- raw %>%
    transmute(
      datetime_local = mdy_hms(.data[[datetime_col]], tz = "America/Denver"),
      datetime_utc   = with_tz(datetime_local, "UTC"),
      temp_c         = (.data[[temp_col]] - 32) * 5/9
    ) %>%
    select(datetime_utc, temp_c)
  
  names(cleaned)[2] <- paste0("temp_", depth_m, "m")
  
  cleaned
}

temp_2m_clean  <- clean_hobo_temp("~/Desktop/Project/Brooks_lake_2025/data_raw/buoy/lowerjade/LJ_2m.csv",  2)
temp_4m_clean  <- clean_hobo_temp("~/Desktop/Project/Brooks_lake_2025/data_raw/buoy/lowerjade/LJ_4m.csv",  4)
temp_5m_clean  <- clean_hobo_temp("~/Desktop/Project/Brooks_lake_2025/data_raw/buoy/lowerjade/LJ_5m.csv",  5)
temp_6m_clean  <- clean_hobo_temp("~/Desktop/Project/Brooks_lake_2025/data_raw/buoy/lowerjade/LJ_6m.csv",  6)
temp_7m_clean  <- clean_hobo_temp("~/Desktop/Project/Brooks_lake_2025/data_raw/buoy/lowerjade/LJ_7m.csv",  7)
temp_8m_clean  <- clean_hobo_temp("~/Desktop/Project/Brooks_lake_2025/data_raw/buoy/lowerjade/LJ_8m.csv",  8)
temp_10m_clean <- clean_hobo_temp("~/Desktop/Project/Brooks_lake_2025/data_raw/buoy/lowerjade/LJ_10m.csv", 10)
temp_11m_clean <- clean_hobo_temp("~/Desktop/Project/Brooks_lake_2025/data_raw/buoy/lowerjade/LJ_11m.csv", 11)
temp_12m_clean <- clean_hobo_temp("~/Desktop/Project/Brooks_lake_2025/data_raw/buoy/lowerjade/LJ_12m.csv", 12)
temp_13m_clean <- clean_hobo_temp("~/Desktop/Project/Brooks_lake_2025/data_raw/buoy/lowerjade/LJ_13m.csv", 13)

lower_jade_hobo <- temp_2m_clean %>%
  full_join(temp_4m_clean,  by = "datetime_utc") %>%
  full_join(temp_5m_clean,  by = "datetime_utc") %>%
  full_join(temp_6m_clean,  by = "datetime_utc") %>%
  full_join(temp_7m_clean,  by = "datetime_utc") %>%
  full_join(temp_8m_clean,  by = "datetime_utc") %>%
  full_join(temp_10m_clean, by = "datetime_utc") %>%
  full_join(temp_11m_clean, by = "datetime_utc") %>%
  full_join(temp_12m_clean, by = "datetime_utc") %>%
  full_join(temp_13m_clean, by = "datetime_utc") %>%
  arrange(datetime_utc)


do_surface <- read.table("~/Desktop/Project/Brooks_lake_2025/data_raw/buoy/lowerjade/LJ_Surface.TXT", sep=",", header=TRUE, skip = 7) 

do_surface_clean <- do_surface %>%
  slice(-1) %>%
  transmute(
    datetime_utc = ymd_hms(trimws(UTC_Date_._Time), tz = "UTC"),
    temp_1m      = as.numeric(trimws(Temperature)),
    do_mgl_1m    = as.numeric(trimws(Dissolved.Oxygen)),
    do_sat_1m    = as.numeric(trimws(Dissolved.Oxygen.Saturation))
  ) %>%
  arrange(datetime_utc)


head(do_surface_clean)
str(do_surface_clean)
range(do_surface_clean$datetime_utc, na.rm = TRUE)
summary(do_surface_clean$temp_1m)
summary(do_surface_clean$do_mgl_1m)
summary(do_surface_clean$do_sat_1m)


do_depth <- read.table(
  "~/Desktop/Project/Brooks_lake_2025/data_raw/buoy/lowerjade/LJ_Depth.TXT",
  sep = ",",
  header = TRUE,
  skip = 7
)

do_depth_clean <- do_depth %>%
  slice(-1) %>%
  transmute(
    datetime_utc = ymd_hms(trimws(UTC_Date_._Time), tz = "UTC"),
    temp_14m     = as.numeric(trimws(Temperature)),
    do_mgl_14m   = as.numeric(trimws(Dissolved.Oxygen)),
    do_sat_14m   = as.numeric(trimws(Dissolved.Oxygen.Saturation))
  ) %>%
  arrange(datetime_utc)


head(do_depth_clean)
str(do_depth_clean)
range(do_depth_clean$datetime_utc, na.rm = TRUE)
summary(do_depth_clean$temp_14m)
summary(do_depth_clean$do_mgl_14m)
summary(do_depth_clean$do_sat_14m)


library(dplyr)
library(purrr)

hobo_list <- list(
  temp_2m_clean,
  temp_4m_clean,
  temp_5m_clean,
  temp_6m_clean,
  temp_7m_clean,
  temp_8m_clean,
  temp_10m_clean,
  temp_11m_clean,
  temp_12m_clean,
  temp_13m_clean
)

lower_jade_hobo <- reduce(hobo_list, full_join, by = "datetime_utc") %>%
  arrange(datetime_utc)

names(lower_jade_hobo)
head(lower_jade_hobo)
range(lower_jade_hobo$datetime_utc, na.rm = TRUE)

lower_jade_raw_merged <- lower_jade_hobo %>%
  full_join(do_surface_clean, by = "datetime_utc") %>%
  full_join(do_depth_clean,   by = "datetime_utc") %>%
  arrange(datetime_utc)

lower_jade_raw_merged <- lower_jade_raw_merged %>%
  select(
    datetime_utc,
    temp_1m, do_mgl_1m, do_sat_1m,
    temp_2m,
    temp_4m,
    temp_5m,
    temp_6m,
    temp_7m,
    temp_8m,
    temp_10m,
    temp_11m,
    temp_12m,
    temp_13m,
    temp_14m, do_mgl_14m, do_sat_14m
  )



lower_jade_raw_merged <- lower_jade_raw_merged %>%
  mutate(datetime_mst = with_tz(datetime_utc, "America/Denver")) %>%
  relocate(datetime_mst, .after = datetime_utc)


lower_jade_raw_merged %>%
  count(datetime_utc) %>%
  filter(n > 1)

time_gaps <- lower_jade_raw_merged %>%
  arrange(datetime_utc) %>%
  mutate(dt_mins = as.numeric(difftime(datetime_utc, lag(datetime_utc), units = "mins")))

table(time_gaps$dt_mins, useNA = "ifany")

colSums(is.na(lower_jade_raw_merged))



library(dplyr)
library(lubridate)

lower_jade_hourly <- lower_jade_raw_merged %>%
  mutate(datetime_hour_utc = floor_date(datetime_utc, unit = "hour")) %>%
  group_by(datetime_hour_utc) %>%
  summarise(
    across(-c(datetime_utc, datetime_mst), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    across(-datetime_hour_utc, ~ ifelse(is.nan(.x), NA, .x)),
    datetime_hour_mst = with_tz(datetime_hour_utc, "America/Denver")
  ) %>%
  relocate(datetime_hour_mst, .after = datetime_hour_utc)



head(lower_jade_hourly)
colSums(is.na(lower_jade_hourly))

ggplot(lower_jade_hourly, aes(x = datetime_hour_mst)) +
  geom_line(aes(y = temp_1m), linewidth = 0.4) +
  geom_line(aes(y = temp_2m), linewidth = 0.4) +
  geom_line(aes(y = temp_14m), linewidth = 0.4) +
  labs(
    title = "Lower Jade Hourly Temperatures",
    x = "Date",
    y = "Temperature (°C)"
  ) +
  theme_minimal()





# =========================================================
# Script: 01_build_lower_jade_buoy.R
# Purpose: Import and merge raw Lower Jade buoy files into:
#   1) a raw merged dataset
#   2) an hourly aggregated dataset
#
# Notes:
# - HOBO temperature loggers were exported in local Mountain time
#   (header labeled relative to GMT), so they are parsed as
#   America/Denver and then converted to UTC.
# - miniDOT files contain both UTC and Mountain time columns;
#   UTC is used as the master time column.
# - HOBO temperature files are recorded every 15 min.
# - miniDOT files are recorded every 10 min.
# - We keep UTC as the master merge time and add local time
#   columns for easier plotting and interpretation.
# =========================================================

# -------------------------------
# 1. Load packages
# -------------------------------
library(dplyr)
library(lubridate)
library(stringr)
library(purrr)
library(ggplot2)

# -------------------------------
# 2. Define file paths
# -------------------------------
base_dir <- "~/Desktop/Project/Brooks_lake_2025/data_raw/buoy/lowerjade"

hobo_files <- c(
  "2"  = file.path(base_dir, "LJ_2m.csv"),
  "4"  = file.path(base_dir, "LJ_4m.csv"),
  "5"  = file.path(base_dir, "LJ_5m.csv"),
  "6"  = file.path(base_dir, "LJ_6m.csv"),
  "7"  = file.path(base_dir, "LJ_7m.csv"),
  "8"  = file.path(base_dir, "LJ_8m.csv"),
  "10" = file.path(base_dir, "LJ_10m.csv"),
  "11" = file.path(base_dir, "LJ_11m.csv"),
  "12" = file.path(base_dir, "LJ_12m.csv"),
  "13" = file.path(base_dir, "LJ_13m.csv")
)

surface_file <- file.path(base_dir, "LJ_Surface.TXT")
depth_file   <- file.path(base_dir, "LJ_Depth.TXT")

# -------------------------------
# 3. Helper function: clean HOBO temperature file
# -------------------------------
# HOBO files:
# - skip first line of metadata
# - contain local Mountain time in 12-hour format with AM/PM
# - temperature exported in Fahrenheit, so convert to Celsius
clean_hobo_temp <- function(file, depth_m) {
  raw <- read.csv(file, skip = 1, header = TRUE)
  
  datetime_col <- names(raw)[str_detect(names(raw), "Date.Time")]
  temp_col     <- names(raw)[str_detect(names(raw), "Temp")]
  
  cleaned <- raw %>%
    transmute(
      # Parse as local Mountain time, then convert to UTC
      datetime_local = mdy_hms(.data[[datetime_col]], tz = "America/Denver"),
      datetime_utc   = with_tz(datetime_local, "UTC"),
      
      # Convert Fahrenheit to Celsius
      temp_c = (.data[[temp_col]] - 32) * 5/9
    ) %>%
    select(datetime_utc, temp_c) %>%
    arrange(datetime_utc)
  
  names(cleaned)[2] <- paste0("temp_", depth_m, "m")
  cleaned
}

# -------------------------------
# 4. Helper function: clean miniDOT file
# -------------------------------
# miniDOT files:
# - skip 7 metadata/header lines
# - first imported row is a units row, so remove it
# - use UTC column as master time
# - keep temperature, DO (mg/L), and DO saturation (%)

clean_minidot <- function(file, depth_m) {
  raw <- read.table(
    file,
    sep = ",",
    header = TRUE,
    skip = 7
  )
  
  cleaned <- raw %>%
    slice(-1) %>%   # remove units row
    transmute(
      datetime_utc = ymd_hms(trimws(UTC_Date_._Time), tz = "UTC"),
      temp         = as.numeric(trimws(Temperature)),
      do_mgl       = as.numeric(trimws(Dissolved.Oxygen)),
      do_sat       = as.numeric(trimws(Dissolved.Oxygen.Saturation))
    ) %>%
    arrange(datetime_utc)
  
  names(cleaned) <- c(
    "datetime_utc",
    paste0("temp_", depth_m, "m"),
    paste0("do_mgl_", depth_m, "m"),
    paste0("do_sat_", depth_m, "m")
  )
  
  cleaned
}

# -------------------------------
# 5. Import and clean HOBO files
# -------------------------------
hobo_list <- imap(hobo_files, ~ clean_hobo_temp(file = .x, depth_m = .y))

# Merge all HOBO temperature files into one wide table
lower_jade_hobo <- reduce(hobo_list, full_join, by = "datetime_utc") %>%
  arrange(datetime_utc)

# -------------------------------
# 6. Import and clean miniDOT files
# -------------------------------
do_surface_clean <- clean_minidot(surface_file, depth_m = 1)
do_depth_clean   <- clean_minidot(depth_file,   depth_m = 14)

# -------------------------------
# 7. Merge HOBO + miniDOT into raw merged dataset
# -------------------------------
lower_jade_raw_merged <- lower_jade_hobo %>%
  full_join(do_surface_clean, by = "datetime_utc") %>%
  full_join(do_depth_clean,   by = "datetime_utc") %>%
  arrange(datetime_utc) %>%
  select(
    datetime_utc,
    temp_1m, do_mgl_1m, do_sat_1m,
    temp_2m,
    temp_4m,
    temp_5m,
    temp_6m,
    temp_7m,
    temp_8m,
    temp_10m,
    temp_11m,
    temp_12m,
    temp_13m,
    temp_14m, do_mgl_14m, do_sat_14m
  ) %>%
  mutate(
    # Add local Mountain time for easy plotting / visual checks
    datetime_mst = with_tz(datetime_utc, "America/Denver")
  ) %>%
  relocate(datetime_mst, .after = datetime_utc)

# -------------------------------
# 8. Structural checks on raw merged dataset
# -------------------------------

# A. Confirm there are no duplicate timestamps
duplicate_times <- lower_jade_raw_merged %>%
  count(datetime_utc) %>%
  filter(n > 1)

print(duplicate_times)

# B. Inspect time gaps between consecutive rows
time_gaps <- lower_jade_raw_merged %>%
  arrange(datetime_utc) %>%
  mutate(
    dt_mins = as.numeric(difftime(datetime_utc, lag(datetime_utc), units = "mins"))
  )

print(table(time_gaps$dt_mins, useNA = "ifany"))

# C. Inspect missingness by column
print(colSums(is.na(lower_jade_raw_merged)))

# -------------------------------
# 9. Aggregate to hourly dataset
# -------------------------------
# This creates a common hourly time grid that is much easier
# to use for plotting, heatmaps, and later analysis.
lower_jade_hourly <- lower_jade_raw_merged %>%
  mutate(datetime_hour_utc = floor_date(datetime_utc, unit = "hour")) %>%
  group_by(datetime_hour_utc) %>%
  summarise(
    across(-c(datetime_utc, datetime_mst), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    # mean(..., na.rm = TRUE) returns NaN when all values are missing;
    # replace those with NA
    across(-datetime_hour_utc, ~ ifelse(is.nan(.x), NA, .x)),
    datetime_hour_mst = with_tz(datetime_hour_utc, "America/Denver")
  ) %>%
  relocate(datetime_hour_mst, .after = datetime_hour_utc)

# Quick checks on hourly dataset
print(head(lower_jade_hourly))
print(colSums(is.na(lower_jade_hourly)))

# -------------------------------
# 10. Quick sanity-check plot
# -------------------------------
ggplot(lower_jade_hourly, aes(x = datetime_hour_mst)) +
  geom_line(aes(y = temp_1m), linewidth = 0.4) +
  geom_line(aes(y = temp_2m), linewidth = 0.4) +
  geom_line(aes(y = temp_4m), linewidth = 0.4) +
  geom_line(aes(y = temp_6m), linewidth = 0.4) +
  geom_line(aes(y = temp_7m), linewidth = 0.4) +
  geom_line(aes(y = temp_8m), linewidth = 0.4) +
  geom_line(aes(y = temp_10m), linewidth = 0.4) +
  geom_line(aes(y = temp_11m), linewidth = 0.4) +
  geom_line(aes(y = temp_12m), linewidth = 0.4) +
  geom_line(aes(y = temp_13m), linewidth = 0.4) +
  geom_line(aes(y = temp_14m), linewidth = 0.4) +
  labs(
    title = "Lower Jade hourly temperatures",
    x = "Date",
    y = "Temperature (°C)"
  ) +
  theme_minimal()

# -------------------------------
# 11. Optional: save outputs
# -------------------------------
# saveRDS(lower_jade_raw_merged,
#         "~/Desktop/Project/Brooks_lake_2025/data_processed/buoy/lower_jade_raw_merged.rds")
#
# saveRDS(lower_jade_hourly,
#         "~/Desktop/Project/Brooks_lake_2025/data_processed/buoy/lower_jade_hourly.rds")
#
# write.csv(lower_jade_raw_merged,
#           "~/Desktop/Project/Brooks_lake_2025/data_processed/buoy/lower_jade_raw_merged.csv",
#           row.names = FALSE)
#
# write.csv(lower_jade_hourly,
#           "~/Desktop/Project/Brooks_lake_2025/data_processed/buoy/lower_jade_hourly.csv",
#           row.names = FALSE)




# =========================================================
# Script: 01_build_lower_jade_buoy.R
# Purpose:
#   Import raw Lower Jade buoy files and build:
#     1) lower_jade_raw_merged  = merged raw-resolution dataset
#     2) lower_jade_hourly      = hourly aggregated dataset
#
# Design choices:
#   - UTC is the master time zone for merging and storage.
#   - Local Mountain time is added only for interpretation/plotting.
#   - HOBO temperature files are recorded in local Mountain time
#     and exported with a GMT-offset style header, so they are
#     parsed as America/Denver and then converted to UTC.
#   - miniDOT files contain both UTC and local time columns;
#     UTC is used directly.
#
# Notes:
#   - HOBO files are 15-minute temperature records.
#   - miniDOT files are 10-minute records for temp + DO.
#   - Some sensors start and end at different times.
#   - This script does NOT apply QA/QC corrections yet.
# =========================================================

# -------------------------------
# 1) Load packages
# -------------------------------
library(dplyr)
library(lubridate)
library(stringr)
library(purrr)
library(readr)
library(xts)
library(dygraphs)
library(RColorBrewer)

# -------------------------------
# 2) Define file paths
# -------------------------------
base_dir <- "~/Desktop/Project/Brooks_lake_2025/data_raw/buoy/lowerjade"

hobo_files <- c(
  "2"  = file.path(base_dir, "LJ_2m.csv"),
  "4"  = file.path(base_dir, "LJ_4m.csv"),
  "5"  = file.path(base_dir, "LJ_5m.csv"),
  "6"  = file.path(base_dir, "LJ_6m.csv"),
  "7"  = file.path(base_dir, "LJ_7m.csv"),
  "8"  = file.path(base_dir, "LJ_8m.csv"),
  "10" = file.path(base_dir, "LJ_10m.csv"),
  "11" = file.path(base_dir, "LJ_11m.csv"),
  "12" = file.path(base_dir, "LJ_12m.csv"),
  "13" = file.path(base_dir, "LJ_13m.csv")
)

surface_file <- file.path(base_dir, "LJ_Surface.TXT")
depth_file   <- file.path(base_dir, "LJ_Depth.TXT")

# -------------------------------
# 3) Helper: clean HOBO temperature file
# -------------------------------
# HOBO exports:
#   - include 1 metadata/header line to skip
#   - store local Mountain time in 12-hour format with AM/PM
#   - temperature is exported in Fahrenheit
clean_hobo_temp <- function(file, depth_m) {
  
  raw <- read.csv(file, skip = 1, header = TRUE)
  
  datetime_col <- names(raw)[str_detect(names(raw), "Date.Time")]
  temp_col     <- names(raw)[str_detect(names(raw), "Temp")]
  
  if (length(datetime_col) != 1) {
    stop("Could not uniquely identify HOBO datetime column in: ", file)
  }
  
  if (length(temp_col) != 1) {
    stop("Could not uniquely identify HOBO temperature column in: ", file)
  }
  
  cleaned <- raw %>%
    transmute(
      # Parse HOBO clock as local Mountain time, then convert to UTC
      datetime_local = mdy_hms(.data[[datetime_col]], tz = "America/Denver"),
      datetime_utc   = with_tz(datetime_local, "UTC"),
      
      # Convert Fahrenheit to Celsius
      temp_c = (.data[[temp_col]] - 32) * 5 / 9
    ) %>%
    select(datetime_utc, temp_c) %>%
    arrange(datetime_utc)
  
  names(cleaned)[2] <- paste0("temp_", depth_m, "m")
  
  cleaned
}

# -------------------------------
# 4) Helper: clean miniDOT file
# -------------------------------
# miniDOT exports:
#   - contain 7 metadata lines before the table
#   - include a units row as the first imported data row
#   - include both UTC and local time columns
#   - use UTC as the merge time
clean_minidot <- function(file, depth_m) {
  
  raw <- read.table(
    file,
    sep = ",",
    header = TRUE,
    skip = 7
  )
  
  cleaned <- raw %>%
    slice(-1) %>%  # remove units row
    transmute(
      datetime_utc = ymd_hms(trimws(UTC_Date_._Time), tz = "UTC"),
      temp         = as.numeric(trimws(Temperature)),
      do_mgl       = as.numeric(trimws(Dissolved.Oxygen)),
      do_sat       = as.numeric(trimws(Dissolved.Oxygen.Saturation))
    ) %>%
    arrange(datetime_utc)
  
  names(cleaned) <- c(
    "datetime_utc",
    paste0("temp_", depth_m, "m"),
    paste0("do_mgl_", depth_m, "m"),
    paste0("do_sat_", depth_m, "m")
  )
  
  cleaned
}

# -------------------------------
# 5) Import and clean all HOBO temperature files
# -------------------------------
hobo_list <- imap(hobo_files, ~ clean_hobo_temp(file = .x, depth_m = .y))

# Merge all HOBO depths into one wide table
lower_jade_hobo <- reduce(hobo_list, full_join, by = "datetime_utc") %>%
  arrange(datetime_utc)

# -------------------------------
# 6) Import and clean miniDOT surface + deep files
# -------------------------------
do_surface_clean <- clean_minidot(surface_file, depth_m = 1)
do_depth_clean   <- clean_minidot(depth_file,   depth_m = 14)

# -------------------------------
# 7) Merge all buoy components into one raw dataset
# -------------------------------
lower_jade_raw_merged <- lower_jade_hobo %>%
  full_join(do_surface_clean, by = "datetime_utc") %>%
  full_join(do_depth_clean,   by = "datetime_utc") %>%
  arrange(datetime_utc) %>%
  select(
    datetime_utc,
    temp_1m, do_mgl_1m, do_sat_1m,
    temp_2m,
    temp_4m,
    temp_5m,
    temp_6m,
    temp_7m,
    temp_8m,
    temp_10m,
    temp_11m,
    temp_12m,
    temp_13m,
    temp_14m, do_mgl_14m, do_sat_14m
  ) %>%
  mutate(
    datetime_local = with_tz(datetime_utc, "America/Denver")
  ) %>%
  relocate(datetime_local, .after = datetime_utc)

# -------------------------------
# 8) Structural checks on raw merged dataset
# -------------------------------

# Check for duplicate timestamps
duplicate_times <- lower_jade_raw_merged %>%
  count(datetime_utc) %>%
  filter(n > 1)

print(duplicate_times)

# Check time gaps between consecutive rows
time_gaps <- lower_jade_raw_merged %>%
  arrange(datetime_utc) %>%
  mutate(
    dt_mins = as.numeric(difftime(datetime_utc, lag(datetime_utc), units = "mins"))
  )

print(table(time_gaps$dt_mins, useNA = "ifany"))

# Summarize missingness by column
missing_counts <- colSums(is.na(lower_jade_raw_merged))
print(missing_counts)

# -------------------------------
# 9) Aggregate to hourly dataset
# -------------------------------
# This makes the mixed 10-min and 15-min data much easier to use
# for heatmaps, time-series plots, and later analysis.
lower_jade_hourly <- lower_jade_raw_merged %>%
  mutate(datetime_hour_utc = floor_date(datetime_utc, unit = "hour")) %>%
  group_by(datetime_hour_utc) %>%
  summarise(
    across(-c(datetime_utc, datetime_local), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    across(-datetime_hour_utc, ~ ifelse(is.nan(.x), NA, .x)),
    datetime_hour_local = with_tz(datetime_hour_utc, "America/Denver")
  ) %>%
  relocate(datetime_hour_local, .after = datetime_hour_utc)

print(head(lower_jade_hourly))
print(colSums(is.na(lower_jade_hourly)))

# -------------------------------
# 10) Interactive temperature QC plot with dygraphs
# -------------------------------
# Create a temperature-only table for interactive plotting
temp_hourly <- lower_jade_hourly %>%
  select(
    datetime_hour_local,
    temp_1m,
    temp_2m,
    temp_4m,
    temp_5m,
    temp_6m,
    temp_7m,
    temp_8m,
    temp_10m,
    temp_11m,
    temp_12m,
    temp_13m,
    temp_14m
  )

# Convert to xts for dygraphs
temp_xts <- xts(
  x = temp_hourly %>% select(-datetime_hour_local),
  order.by = temp_hourly$datetime_hour_local
)

# Distinct color palette for depths
temp_cols <- c(
  "temp_1m"  = "#D73027",
  "temp_2m"  = "#FC8D59",
  "temp_4m"  = "#FEE08B",
  "temp_5m"  = "#D9EF8B",
  "temp_6m"  = "#91CF60",
  "temp_7m"  = "#66C2A5",
  "temp_8m"  = "#3288BD",
  "temp_10m" = "#5E4FA2",
  "temp_11m" = "#7B3294",
  "temp_12m" = "#C2A5CF",
  "temp_13m" = "#A6CEE3",
  "temp_14m" = "#1F78B4"
)

dygraph(temp_xts, main = "Lower Jade hourly temperatures") %>%
  dyOptions(
    drawPoints = FALSE,
    strokeWidth = 1.2,
    colors = unname(temp_cols)
  ) %>%
  dyAxis("y", label = "Temperature (°C)") %>%
  dyAxis("x", label = "Date") %>%
  dyRangeSelector() %>%
  dyHighlight(
    highlightCircleSize = 3,
    highlightSeriesBackgroundAlpha = 0.2,
    hideOnMouseOut = TRUE
  ) %>%
  dyLegend(show = "follow") %>%
  dySeries("temp_1m",  label = "1 m") %>%
  dySeries("temp_2m",  label = "2 m") %>%
  dySeries("temp_4m",  label = "4 m") %>%
  dySeries("temp_5m",  label = "5 m") %>%
  dySeries("temp_6m",  label = "6 m") %>%
  dySeries("temp_7m",  label = "7 m") %>%
  dySeries("temp_8m",  label = "8 m") %>%
  dySeries("temp_10m", label = "10 m") %>%
  dySeries("temp_11m", label = "11 m") %>%
  dySeries("temp_12m", label = "12 m") %>%
  dySeries("temp_13m", label = "13 m") %>%
  dySeries("temp_14m", label = "14 m")

# -------------------------------
# 11) Optional save outputs
# -------------------------------
saveRDS(lower_jade_raw_merged,"~/Desktop/Project/Brooks_lake_2025/data_raw/buoy/lowerjade/lower_jade_raw_merged.rds"
 )
#
saveRDS(lower_jade_hourly,"~/Desktop/Project/Brooks_lake_2025/data_raw/buoy/lowerjade/lower_jade_raw_hourly_merged.rds")