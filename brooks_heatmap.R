#brooks heatmap


library(tidyverse)
library(lubridate)
library(scico)
library(rLakeAnalyzer)
library(zoo)

theme_set(theme_bw(base_family = "Helvetica"))

# Load data ---------------------------------------------------------------

brooks <- read_csv("/Users/samanthapena/Desktop/Project/Brooks_lake_2025/data_clean/buoy/brooks_wq_2025_cleaned.csv")
view(brooks)
 colnames(brooks)


 brooks <- brooks |>
   select(-...1) |>
   rename(WTemp0.75m = WTempC) |>
   mutate(
     datetime = DATETIME_MST
   )
 
 # Convert temperature data to long format --------------------------------
 
 wtemp <- brooks |>
   select(datetime, matches("^WTemp")) |>
   pivot_longer(
     cols = matches("^WTemp"),
     names_to = "depth",
     values_to = "temp_c"
   ) |>
   mutate(
     depth = str_remove(depth, "WTemp"),
     depth = str_remove(depth, "m"),
     depth = as.numeric(depth)
   ) |>
   filter(!is.na(temp_c))
 
 # Summarize to hourly means -----------------------------------------------
 
 wtemp_hourly <- wtemp |>
   mutate(datetime_hour = round_date(datetime, unit = "hour")) |>
   group_by(datetime_hour, depth) |>
   summarise(
     temp_c = mean(temp_c, na.rm = TRUE),
     .groups = "drop"
   )
 
 # Interpolate between sensor depths --------------------------------------
 
 interp_hourly <- wtemp_hourly |>
   group_by(datetime_hour) |>
   nest() |>
   mutate(n_depths = map_dbl(data, nrow)) |>
   filter(n_depths > 1) |>
   mutate(data = map(data, ~ arrange(.x, depth))) |>
   mutate(
     interp_fun = map(data, ~ approxfun(.x$depth, .x$temp_c)),
     raster = map2(data, interp_fun, function(df, func) {
       tibble(
         depth = seq(min(df$depth), max(df$depth), by = 0.2),
         temp_c = func(depth)
       )
     })
   ) |>
   select(datetime_hour, raster) |>
   unnest(raster) |>
   filter(!is.na(temp_c))
 
 # Plot heatmap ------------------------------------------------------------

 brooks_heatmap <- ggplot(interp_hourly, aes(x = datetime_hour, y = depth, fill = temp_c)) +
   geom_tile(
     width = 3600,
     height = 0.2
   ) +
   scale_y_reverse(
     breaks = seq(0, 15, by = 2)
   ) +
   scale_fill_scico(
     palette = "romaO",
     limits = c(4, 19),
     oob = scales::squish
   ) +
   scale_x_datetime(
     date_breaks = "1 month",
     date_labels = "%b"
   ) +
   coord_cartesian(
     ylim = c(14.9, 0.6),
     expand = FALSE
   ) +
   labs(
     x = "Date",
     y = "Depth (m)",
     title = ""
   ) +
   theme(
     panel.grid = element_blank(),
     axis.text = element_text(size = 10),
     axis.title = element_text(size = 12),
     plot.title = element_text(size = 14, face = "bold")
   )
 
 brooks_heatmap
 
 
 # Create output folder if it doesn't exist
 brooks_heatmap <- here("figures/heatmaps")
 
 if (!dir.exists(out_path)) {
   dir.create(out_path, recursive = TRUE)
 }
 ggsave(
   filename = file.path(out_path, "brooks_temperature_heatmap_2025.pdf"),
   plot = brooks_heatmap,
   width = 10,
   height = 5.5,
   units = "in"
 )
 
 
 # ########################
 
 # Temperature columns in correct depth order ------------------------------
 
 temp_cols <- c(
   "wtr_0.75",
   "wtr_1",
   "wtr_4",
   "wtr_7",
   "wtr_9",
   "wtr_10",
   "wtr_13",
   "wtr_15"
 )
 
 temp_depths <- c(0.75, 1, 4, 7, 9, 10, 13, 15)
 
 # Keep only rows where full profile exists --------------------------------
 
 brooks_temp_wide_full <- brooks_temp_wide |>
   select(datetime, all_of(temp_cols)) |>
   filter(if_all(all_of(temp_cols), ~ !is.na(.x)))
 
 # Estimated Brooks bathymetry ---------------------------------------------
 
 bthD <- c(0.75, 1, 4, 7, 9, 10, 13, 15)
 
 bthA <- c(
   866030,
   820000,
   650000,
   430000,
   300000,
   230000,
   90000,
   10000
 )
 
 # Calculate Schmidt stability --------------------------------------------
 
 schmidt_brooks <- brooks_temp_wide_full |>
   rowwise() |>
   mutate(
     schmidt_stability = schmidt.stability(
       wtr = c_across(all_of(temp_cols)),
       depths = temp_depths,
       bthD = bthD,
       bthA = bthA
     )
   ) |>
   ungroup() |>
   select(datetime, schmidt_stability) |>
   mutate(
     stability_24hr = rollmean(
       schmidt_stability,
       k = 24,
       fill = NA,
       align = "center"
     )
   )
 
 # Plot raw + smoothed stability ------------------------------------------
 
 brooks_stability_plot <- ggplot(schmidt_brooks, aes(x = datetime)) +
   geom_line(
     aes(y = schmidt_stability),
     linewidth = 0.35,
     alpha = 0.35
   ) +
   geom_line(
     aes(y = stability_24hr),
     linewidth = 1
   ) +
   scale_x_datetime(
     date_breaks = "1 month",
     date_labels = "%b"
   ) +
   labs(
     x = "Date",
     y = expression("Schmidt stability (J m"^{-2}*")"),
     title = ""
   ) +
   theme_classic(base_family = "Helvetica") +
   theme(
     axis.text = element_text(size = 10),
     axis.title = element_text(size = 12)
   )
 
 brooks_stability_plot
 
 
 
 
 
 # DO/ dissolved oxygen
 
 do_brooks <- brooks |>
   select(datetime, odomgL, odomgL_15m, odosat, odosat_15m) |>
   pivot_longer(
     cols = c(odomgL, odomgL_15m),
     names_to = "depth",
     values_to = "do_mgl"
   ) |>
   mutate(
     depth = case_when(
       depth == "odomgL" ~ "Surface",
       depth == "odomgL_15m" ~ "15 m",
       TRUE ~ depth
     )
   ) |>
   filter(!is.na(do_mgl))

 
 do_hourly <- do_brooks |>
   mutate(datetime_hour = round_date(datetime, unit = "hour")) |>
   group_by(datetime_hour, depth) |>
   summarise(
     do_mgl = mean(do_mgl, na.rm = TRUE),
     .groups = "drop"
   ) 
 
 brooks_do_plot <- ggplot(do_hourly, aes(x = datetime_hour, y = do_mgl, color = depth)) +
   
   geom_line(linewidth = 0.7) +
  scale_color_manual(
     values = c(
       "Surface" = "#1f78b4",
       "15 m" = "#d73027"
     )
   ) +
   
   scale_y_continuous(limits = c(0, 18)) +
   
   scale_x_datetime(
     date_breaks = "1 month",
     date_labels = "%b"
   ) +
   
   labs(
     x = "Date",
     y = "Dissolved oxygen (mg/L)",
     color = "Depth"
   ) +
   
   theme_classic(base_family = "Helvetica") +
   theme(
     axis.text = element_text(size = 10),
     axis.title = element_text(size = 12)
   )
 
 brooks_do_plot
 
 colnames(deq_nutrients_clean_2025)
 
 
 #### nutients 
 nutrients_brooks <- deq_nutrients_clean_2025 |>
   filter(str_to_lower(lake) == "brooks lake") |>  # adjust if exact name differs
   mutate(
     date = as.Date(date),
     depth_zone = case_when(
       str_detect(str_to_lower(depth), "surf") ~ "Surface",
       str_detect(str_to_lower(depth), "mid") ~ "Middle / thermocline",
       str_detect(str_to_lower(depth), "therm") ~ "Middle / thermocline",
       str_detect(str_to_lower(depth), "bot") ~ "Deep",
       TRUE ~ NA_character_
     ),
     depth_zone = factor(
       depth_zone,
       levels = c("Surface", "Middle / thermocline", "Deep")
     )
   ) |>
   filter(!is.na(depth_zone)) |>
   filter(date >= as.Date("2025-04-01"))
 
 # Check that ONLY Brooks is included
 count(nutrients_brooks, lake, depth_zone)
 
 nutrients_long <- nutrients_brooks |>
   select(date, depth_zone, ammonia, din, tp, tn) |>
   pivot_longer(
     cols = c(ammonia, din, tp, tn),
     names_to = "nutrient",
     values_to = "concentration"
   ) |>
   filter(!is.na(concentration)) |>
   group_by(date, depth_zone, nutrient) |>
   summarise(
     concentration = mean(concentration, na.rm = TRUE),
     .groups = "drop"
   ) |>
   mutate(
     nutrient = recode(
       nutrient,
       ammonia = "Ammonia",
       din = "DIN",
       tp = "Total Phosphorus",
       tn = "Total Nitrogen",
     )
   )
 
 nutrient_time_plot <- ggplot(
   nutrients_long,
   aes(x = date, y = concentration, color = depth_zone)
 ) +
   geom_line(
     aes(group = depth_zone),
     linewidth = 0.7,
     alpha = 0.8
   ) +
   geom_point(size = 2.5) +
   facet_wrap(
     ~ nutrient,
     scales = "free_y",
     ncol = 1
   ) +
   scale_x_date(
     date_breaks = "1 month",
     date_labels = "%b"
   ) +
   labs(
     x = "",
     y = "Concentration (µg/L)",
     color = "Depth"
   ) +
   theme_classic(base_family = "Helvetica") +
   theme(
     strip.text = element_text(size = 11, face = "bold"),
     axis.text = element_text(size = 10),
     axis.title = element_text(size = 12),
     legend.position = "bottom"
   )
 
 nutrient_time_plot
 
 
 ##Joining
 # Classify stratification state ------------------------------------------
 
 schmidt_daily <- schmidt_brooks |>
   mutate(date = as.Date(datetime)) |>
   group_by(date) |>
   summarise(
     stability_daily = mean(stability_24hr, na.rm = TRUE),
     .groups = "drop"
   ) |>
   mutate(
     strat_state = case_when(
       stability_daily < 20 ~ "Mixed / weak",
       stability_daily >= 20 & stability_daily < 50 ~ "Moderate",
       stability_daily >= 50 ~ "Strong",
       TRUE ~ NA_character_
     ),
     strat_state = factor(
       strat_state,
       levels = c("Mixed / weak", "Moderate", "Strong")
     )
   )
 # Prepare daily deep DO ---------------------------------------------------
 
 deep_do_daily <- do_hourly |>
   filter(depth == "15 m") |>
   mutate(date = as.Date(datetime_hour)) |>
   group_by(date) |>
   summarise(
     deep_do_mgl = mean(do_mgl, na.rm = TRUE),
     .groups = "drop"
   )
 
 # Join stability state to deep DO ----------------------------------------
 
 deep_do_strat <- deep_do_daily |>
   left_join(schmidt_daily, by = "date") |>
   filter(!is.na(strat_state))
 deep_do_boxplot <- ggplot(deep_do_strat, aes(x = strat_state, y = deep_do_mgl)) +
   geom_boxplot(outlier.shape = NA, alpha = 0.5) +
   geom_jitter(width = 0.15, size = 1.8, alpha = 0.7) +
   labs(
     x = "Stratification state",
     y = "Deep dissolved oxygen (mg/L)"
   ) +
   theme_classic(base_family = "Helvetica") +
   theme(
     axis.text = element_text(size = 10),
     axis.title = element_text(size = 12)
   )
 
 deep_do_boxplot
 # Prepare bottom nutrients ------------------------------------------------
 
 bottom_nutrients <- nutrients_long |>
   filter(depth_zone == "Deep") |>
   mutate(date = as.Date(date)) |>
   left_join(schmidt_daily, by = "date") |>
   filter(!is.na(strat_state))
 bottom_nutrient_boxplot <- ggplot(
   bottom_nutrients,
   aes(x = strat_state, y = concentration)
 ) +
   geom_boxplot(outlier.shape = NA, alpha = 0.5) +
   geom_jitter(width = 0.15, size = 2, alpha = 0.75) +
   facet_wrap(~ nutrient, scales = "free_y") +
   labs(
     x = "Stratification state",
     y = "Deep nutrient concentration (µg/L)"
   ) +
   theme_classic(base_family = "Helvetica") +
   theme(
     strip.text = element_text(size = 11, face = "bold"),
     axis.text = element_text(size = 10),
     axis.title = element_text(size = 12)
   )
 
 bottom_nutrient_boxplot