
########
# ======================================================================
# Main toxin figure: buoy grab concentrations + grab vs SPATT detection
# ======================================================================

library(tidyverse)
library(lubridate)
library(scales)
library(cowplot)

source("theme_thesis.R")

tox_plot <- readRDS(
  "~/Desktop/Project/Brooks_lake_2025/data_clean/toxins/tox_plot.rds"
)

fig_dir <- "~/Desktop/Project/Brooks_lake_2025/figures/toxins"
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

toxin_cols <- c(
  "rr", "yr", "lr", "la", "dm_lr", "ly",
  "nod", "lf", "wr", "atx", "hatx", "cyl"
)

lake_lookup <- c(
  "brooks" = "Brooks Lake",
  "lower jade" = "Lower Jade Lake",
  "rainbow" = "Rainbow Lake",
  "upper brooks" = "Upper Brooks Lake"
)

depth_colors <- c(
  "Surface" = "#00A6A6",
  "Depth" = "#E64B35"
)

detection_colors <- c(
  "Detected" = "#1F2937",
  "Not detected" = "#E5E7EB"
)

method_colors <- c(
  "Grab" = "#E64B35",
  "SPATT" = "#00A6A6"
)

tox_4lakes <- tox_plot %>%
  mutate(
    lake_label = recode(lake, !!!lake_lookup),
    lake_label = factor(lake_label, levels = lake_order),
    method = str_to_lower(method),
    site_type = str_to_lower(site_type),
    sample_date = as.Date(sample_date),
    site_type_clean = case_when(
      site_type == "buoy_surface" ~ "Surface",
      site_type == "buoy_depth" ~ "Depth",
      TRUE ~ str_to_title(site_type)
    ),
    any_toxin_detected = if_any(
      all_of(toxin_cols),
      ~ !is.na(.x) & .x > 0
    )
  ) %>%
  filter(
    lake_label %in% lake_order,
    method %in% c("grab", "spatt")
  )

# ======================================================================
# Panel A: Grab-sample microcystin concentrations
# ======================================================================

grab_buoy <- tox_4lakes %>%
  filter(
    method == "grab",
    site_type %in% c("buoy_surface", "buoy_depth")
  )

mc_y_limits <- c(
  0,
  max(grab_buoy$total_mc, na.rm = TRUE) * 1.15
)

pA <- ggplot(
  grab_buoy,
  aes(
    x = sample_date,
    y = total_mc,
    color = site_type_clean,
    group = site_type_clean
  )
) +
  geom_line(linewidth = 0.8, alpha = 0.85) +
  geom_point(size = 2) +
  facet_wrap(~ lake_label, nrow = 1) +
  scale_color_manual(values = depth_colors) +
  scale_y_continuous(
    trans = "log1p",
    limits = mc_y_limits,
    labels = label_number(accuracy = 0.1)
  ) +
  date_scale_thesis() +
  labs(
    x = NULL,
    y = expression(Total~microcystins~"("*mu*g~L^{-1}*")"),
    color = NULL
  ) +
  theme_thesis() +
  theme_lake_strip() +
  theme(
      axis.text.x = element_text(
        angle = 45,
        hjust = 1,
        vjust = 1,
        size = 7.5
      ),
      axis.ticks.x = element_line(),
      axis.title.x = element_blank()
    )

legend_A <- legend_row(
  pA,
  aesthetic = "color",
  nrow = 1
)

pA_clean <- pA + theme_no_legend()

# ======================================================================
# Panel B: Temporal cyanotoxin detection
# ======================================================================

buoy_method_detection_time <- tox_4lakes %>%
  filter(site_type %in% c("buoy_surface", "buoy_depth")) %>%
  mutate(
    method_clean = case_when(
      method == "grab" ~ "Grab",
      method == "spatt" ~ "SPATT"
    ),
    method_site = paste(method_clean, site_type_clean)
  ) %>%
  group_by(lake_label, sample_date, method_site) %>%
  summarise(
    detected_any = any(any_toxin_detected, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    detected = if_else(detected_any, "Detected", "Not detected"),
    detected = factor(detected, levels = c("Detected", "Not detected")),
    method_site = factor(
      method_site,
      levels = c(
        "Grab Surface",
        "SPATT Surface",
        "Grab Depth",
        "SPATT Depth"
      )
    )
  ) %>%
  filter(!is.na(method_site))

pB <- ggplot(
  buoy_method_detection_time,
  aes(
    x = sample_date,
    y = method_site,
    fill = detected
  )
) +
  geom_tile(color = "white", linewidth = 0.4) +
  facet_wrap(~ lake_label, nrow = 1) +
  scale_fill_manual(values = detection_colors) +
  date_scale_thesis() +
  labs(
    x = NULL,
    y = "Sampling method",
    fill = NULL
  ) +
  theme_thesis() +
  theme_lake_strip() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 7.5)
  )

legend_B <- legend_row(
  pB,
  aesthetic = "fill",
  nrow = 1
)

pB_clean <- pB + theme_no_legend()

# ======================================================================
# Panel C: Detection frequency
# ======================================================================

method_detection_frequency <- tox_4lakes %>%
  filter(site_type %in% c("buoy_surface", "buoy_depth")) %>%
  mutate(
    method_clean = case_when(
      method == "grab" ~ "Grab",
      method == "spatt" ~ "SPATT"
    ),
    method_clean = factor(method_clean, levels = c("Grab", "SPATT")),
    site_type_clean = factor(
      site_type_clean,
      levels = c("Surface", "Depth")
    )
  ) %>%
  group_by(lake_label, method_clean, site_type_clean) %>%
  summarise(
    n_samples = n(),
    detections = sum(any_toxin_detected, na.rm = TRUE),
    detection_frequency = detections / n_samples,
    .groups = "drop"
  )

pC <- ggplot(
  method_detection_frequency,
  aes(
    x = site_type_clean,
    y = detection_frequency,
    fill = method_clean
  )
) +
  geom_col(
    position = position_dodge(width = 0.75),
    width = 0.65
  ) +
  facet_wrap(~ lake_label, nrow = 1) +
  scale_fill_manual(values = method_colors) +
  scale_y_continuous(
    labels = percent_format(),
    limits = c(0, 1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = NULL,
    y = "Detection frequency",
    fill = NULL
  ) +
  theme_thesis() +
  theme_lake_strip() +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 1)
  )

legend_C <- legend_row(
  pC,
  aesthetic = "fill",
  nrow = 1
)

pC_clean <- pC + theme_no_legend()

# ======================================================================
# Final figure
# ======================================================================

tox_main_fig <- publication_panel_3row(
  row_A = pA_clean,
  row_B = pB_clean,
  row_C = pC_clean,
  legend_A = legend_A,
  legend_B = legend_B,
  legend_C = legend_C,
  titles = c(
    "Grab-sample microcystin concentrations",
    "Temporal cyanotoxin detection",
    "Detection frequency"
  ),
  heights = c(
    thesis_panel_heights$panel_large,
    thesis_panel_heights$legend,
    
    thesis_panel_heights$panel_medium,
    thesis_panel_heights$legend,
    
    thesis_panel_heights$panel_small,
    thesis_panel_heights$legend
  )
)
tox_main_fig

save_thesis_fig(
  tox_main_fig,
  file.path(fig_dir, "tox_main_fig.png"),
  width = thesis_figures$width,
  height = thesis_figures$height
)


## Get detection frequency number (percents on how sccesful samplign methods were )###

overall_detection <- tox_4lakes %>%
  filter(site_type %in% c("buoy_surface", "buoy_depth")) %>%
  mutate(
    method = str_to_title(method)
  ) %>%
  group_by(method) %>%
  summarise(
    n_samples = n(),
    detections = sum(any_toxin_detected),
    detection_frequency = detections / n_samples,
    .groups = "drop"
  )

overall_detection

lake_detection <- tox_4lakes %>%
  filter(site_type %in% c("buoy_surface","buoy_depth")) %>%
  mutate(
    method = str_to_title(method)
  ) %>%
  group_by(lake_label, method) %>%
  summarise(
    n_samples = n(),
    detections = sum(any_toxin_detected),
    frequency = detections / n_samples,
    .groups = "drop"
  )

lake_detection

depth_detection <- tox_4lakes %>%
  filter(site_type %in% c("buoy_surface","buoy_depth")) %>%
  mutate(
    method = str_to_title(method),
    depth = recode(
      site_type,
      buoy_surface = "Surface",
      buoy_depth = "Depth"
    )
  ) %>%
  group_by(method, depth) %>%
  summarise(
    n_samples = n(),
    detections = sum(any_toxin_detected),
    frequency = detections / n_samples,
    .groups = "drop"
  )

depth_detection
panelC_table <- tox_4lakes %>%
  filter(site_type %in% c("buoy_surface","buoy_depth")) %>%
  mutate(
    method = str_to_title(method),
    depth = recode(
      site_type,
      buoy_surface = "Surface",
      buoy_depth = "Depth"
    )
  ) %>%
  group_by(
    lake_label,
    depth,
    method
  ) %>%
  summarise(
    n = n(),
    detections = sum(any_toxin_detected),
    frequency = detections / n,
    .groups = "drop"
  )

panelC_table



### Code for Brooks Lake cyanotoxins##
# ======================================================================
# Brooks Lake toxin figure: habitat and littoral variation
# ======================================================================

library(tidyverse)
library(lubridate)
library(scales)
library(cowplot)

source("theme_thesis.R")

tox_plot <- readRDS(
  "~/Desktop/Project/Brooks_lake_2025/data_clean/toxins/tox_plot.rds"
)

fig_dir <- "~/Desktop/Project/Brooks_lake_2025/figures/toxins"
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

# Colors ----------------------------------------------------------------

habitat_colors <- c(
  "Shore mean" = "#4D4D4D",
  "Buoy surface" = "#00A6A6",
  "Buoy depth" = "#E64B35"
)

shore_region_colors <- c(
  "North" = "#0072B2",
  "South" = "#D55E00",
  "East" = "#009E73",
  "West" = "#CC79A7"
)

# Clean Brooks grab data ------------------------------------------------

brooks_grab <- tox_plot %>%
  mutate(
    method = str_to_lower(method),
    site_type = str_to_lower(site_type),
    lake = str_to_lower(lake),
    sample_status = str_to_lower(sample_status),
    sample_type = str_to_lower(sample_type),
    sample_date = as.Date(sample_date)
  ) %>%
  filter(
    lake == "brooks",
    method == "grab",
    !str_detect(sample_type, "duplicate|blank"),
    sample_status != "not_analyzed"
  )

# Brooks shoreline metadata --------------------------------------------

brooks_site_metadata <- tribble(
  ~site_id, ~shore_region,
  "BKS_BL_SH_01", "South",
  "BKS_BL_SH_02", "South",
  "BKS_BL_SH_03", "South",
  "BKS_BL_SH_04", "West",
  "BKS_BL_SH_05", "North",
  "BKS_BL_SH_06", "North",
  "BKS_BL_SH_07", "East"
)

brooks_shore_ids <- brooks_site_metadata$site_id

# Sampling event dates --------------------------------------------------
# This groups shoreline samples collected 1–2 days apart into one event.

brooks_grab <- brooks_grab %>%
  mutate(
    sampling_event = case_when(
      sample_date >= as.Date("2025-07-08") & sample_date <= as.Date("2025-07-10") ~ as.Date("2025-07-09"),
      sample_date >= as.Date("2025-07-21") & sample_date <= as.Date("2025-07-23") ~ as.Date("2025-07-22"),
      sample_date >= as.Date("2025-08-04") & sample_date <= as.Date("2025-08-06") ~ as.Date("2025-08-05"),
      sample_date >= as.Date("2025-08-18") & sample_date <= as.Date("2025-08-20") ~ as.Date("2025-08-19"),
      sample_date >= as.Date("2025-09-01") & sample_date <= as.Date("2025-09-02") ~ as.Date("2025-09-01"),
      sample_date >= as.Date("2025-09-16") & sample_date <= as.Date("2025-09-17") ~ as.Date("2025-09-16"),
      sample_date >= as.Date("2025-09-29") & sample_date <= as.Date("2025-09-30") ~ as.Date("2025-09-29"),
      sample_date >= as.Date("2025-10-14") & sample_date <= as.Date("2025-10-15") ~ as.Date("2025-10-14"),
      TRUE ~ sample_date
    )
  )

# ======================================================================
# Panel A: Habitat comparison — shoreline mean vs buoy
# ======================================================================

brooks_shore_mean <- brooks_grab %>%
  filter(site_id %in% brooks_shore_ids) %>%
  group_by(sampling_event) %>%
  summarise(
    total_mc = mean(total_mc, na.rm = TRUE),
    shore_min = min(total_mc, na.rm = TRUE),
    shore_max = max(total_mc, na.rm = TRUE),
    n_sites = n(),
    .groups = "drop"
  ) %>%
  rename(sample_date = sampling_event) %>%
  mutate(habitat = "Shore mean")

brooks_buoy <- brooks_grab %>%
  filter(site_type %in% c("buoy_surface", "buoy_depth")) %>%
  transmute(
    sample_date = sampling_event,
    total_mc,
    habitat = case_when(
      site_type == "buoy_surface" ~ "Buoy surface",
      site_type == "buoy_depth" ~ "Buoy depth"
    )
  )

brooks_habitat <- bind_rows(
  brooks_shore_mean %>% select(sample_date, total_mc, habitat),
  brooks_buoy
) %>%
  mutate(
    habitat = factor(
      habitat,
      levels = c("Shore mean", "Buoy surface", "Buoy depth")
    )
  )

brooks_y_limits <- c(
  0,
  max(
    brooks_habitat$total_mc,
    brooks_grab$total_mc,
    na.rm = TRUE
  ) * 1.15
)

pA <- ggplot(
  brooks_habitat,
  aes(
    x = sample_date,
    y = total_mc,
    color = habitat,
    group = habitat
  )
) +
  geom_line(linewidth = 0.9, alpha = 0.85) +
  geom_point(size = 2) +
  scale_color_manual(values = habitat_colors) +
  scale_y_continuous(
    trans = "log1p",
    limits = brooks_y_limits,
    labels = label_number(accuracy = 0.1)
  ) +
  date_scale_thesis() +
  labs(
    x = NULL,
    y = expression(Total~microcystins~"("*mu*g~L^{-1}*")"),
    color = NULL
  ) +
  theme_thesis() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7.5)
  )

legend_A <- legend_row(pA, aesthetic = "color", nrow = 1)
pA_clean <- pA + theme_no_legend()

# ======================================================================
# Panel B: Individual shoreline sites
# ======================================================================

brooks_shore_sites <- brooks_grab %>%
  filter(site_id %in% brooks_shore_ids) %>%
  left_join(brooks_site_metadata, by = "site_id") %>%
  mutate(
    shore_region = factor(
      shore_region,
      levels = c("North", "South", "East", "West")
    )
  )

pB <- ggplot(
  brooks_shore_sites,
  aes(
    x = sampling_event,
    y = total_mc,
    color = shore_region,
    group = site_id
  )
) +
  geom_line(linewidth = 0.8, alpha = 0.75) +
  geom_point(size = 2) +
  scale_color_manual(values = shore_region_colors) +
  scale_y_continuous(
    trans = "log1p",
    limits = brooks_y_limits,
    labels = label_number(accuracy = 0.1)
  ) +
  date_scale_thesis() +
  labs(
    x = NULL,
    y = expression(Total~microcystins~"("*mu*g~L^{-1}*")"),
    color = NULL
  ) +
  theme_thesis() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7.5)
  )

legend_B <- legend_row(pB, aesthetic = "color", nrow = 1)
pB_clean <- pB + theme_no_legend()

# ======================================================================
# Panel C: Shoreline region summaries
# ======================================================================

brooks_region_summary <- brooks_shore_sites %>%
  group_by(sampling_event, shore_region) %>%
  summarise(
    mean_mc = mean(total_mc, na.rm = TRUE),
    min_mc = min(total_mc, na.rm = TRUE),
    max_mc = max(total_mc, na.rm = TRUE),
    n_sites = n(),
    .groups = "drop"
  )

pC <- ggplot(
  brooks_region_summary,
  aes(
    x = sampling_event,
    y = mean_mc,
    color = shore_region,
    group = shore_region
  )
) +
  geom_line(linewidth = 0.9, alpha = 0.85) +
  geom_point(size = 2) +
  scale_color_manual(values = shore_region_colors) +
  scale_y_continuous(
    trans = "log1p",
    limits = brooks_y_limits,
    labels = label_number(accuracy = 0.1)
  ) +
  date_scale_thesis() +
  labs(
    x = NULL,
    y = expression(Total~microcystins~"("*mu*g~L^{-1}*")"),
    color = NULL
  ) +
  theme_thesis() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7.5)
  )

legend_C <- legend_row(pC, aesthetic = "color", nrow = 1)
pC_clean <- pC + theme_no_legend()

# ======================================================================
# Final figure
# ======================================================================

brooks_toxin_fig <- stacked_publication_figure(
  panels = list(
    panel_title(pA_clean, "A", "Brooks Lake (Littoral vs. Pelagic) toxin variation"),
    panel_title(pB_clean, "B", "Individual shoreline site concentrations"),
    panel_title(pC_clean, "C", "Mean shoreline concentrations by region")
  ),
  legends = list(
    legend_A,
    legend_B,
    NULL
  ),
  heights = c(
    thesis_panel_heights$panel_medium, thesis_panel_heights$legend,
    thesis_panel_heights$panel_medium, thesis_panel_heights$legend,
    thesis_panel_heights$panel_medium
  )
)

brooks_toxin_fig



save_thesis_fig(
  brooks_toxin_fig,
  file.path(fig_dir, "brooks_toxin_habitat_littoral_variation.png"),
  width = 12,
  height = 10.5
)

colnames(tox_plot)




### i need to create a new tox_plot sheet, with adjusting the total concentrations for SPATT, you divide the total_mc by 3 grams of resin. 

tox_plot <- readRDS(
  "~/Desktop/Project/Brooks_lake_2025/data_clean/toxins/tox_plot.rds"
)

tox_plot <- tox_plot %>%
  mutate(
    total_mc_converted = case_when(
      method == "spatt" ~ total_mc / 3,
      TRUE ~ total_mc
    )
  )

saveRDS(
  tox_plot,"~/Desktop/Project/Brooks_lake_2025/data_clean/toxins/tox_plot_converted.rds")
 
tox_plot %>%
  select(sample_number, lake, site_type, sample_date, method, total_mc, total_mc_converted) %>%
  arrange(sample_date, lake, method) %>%
  print(n = 30)



##### spatt and grab over time ##
library(tidyverse)
library(lubridate)
library(scales)
library(cowplot)

source("theme_thesis.R")

tox_plot <- readRDS(
  "~/Desktop/Project/Brooks_lake_2025/data_clean/toxins/tox_plot_converted.rds"
)

fig_dir <- "~/Desktop/Project/Brooks_lake_2025/figures/toxins"
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

lake_lookup <- c(
  "brooks" = "Brooks Lake",
  "lower jade" = "Lower Jade Lake",
  "rainbow" = "Rainbow Lake",
  "upper brooks" = "Upper Brooks Lake"
)

lake_order <- c(
  "Brooks Lake",
  "Lower Jade Lake",
  "Rainbow Lake",
  "Upper Brooks Lake"
)

method_site_colors <- c(
  "Grab Surface" = "#E64B35",
  "Grab Depth" = "#A32020",
  "SPATT Surface" = "#00A6A6",
  "SPATT Depth" = "#006D77"
)

tox_time <- tox_plot %>%
  mutate(
    lake_label = recode(lake, !!!lake_lookup),
    lake_label = factor(lake_label, levels = lake_order),
    method = str_to_lower(method),
    site_type = str_to_lower(site_type),
    sample_date = as.Date(sample_date),
    
    depth_clean = case_when(
      site_type == "buoy_surface" ~ "Surface",
      site_type == "buoy_depth" ~ "Depth",
      TRUE ~ NA_character_
    ),
    
    method_clean = case_when(
      method == "grab" ~ "Grab",
      method == "spatt" ~ "SPATT",
      TRUE ~ NA_character_
    ),
    
    method_site = paste(method_clean, depth_clean),
    method_site = factor(
      method_site,
      levels = c(
        "Grab Surface",
        "Grab Depth",
        "SPATT Surface",
        "SPATT Depth"
      )
    )
  ) %>%
  filter(
    lake_label %in% lake_order,
    method %in% c("grab", "spatt"),
    site_type %in% c("buoy_surface", "buoy_depth"),
    !is.na(total_mc_converted)
  )

# Check data before plotting
tox_time %>%
  select(
    lake_label,
    sample_date,
    method,
    site_type,
    total_mc,
    total_mc_converted,
    method_site
  ) %>%
  arrange(lake_label, sample_date, method_site) %>%
  print(n = 50)

# Shared y-axis limit across all lakes
mc_y_limits <- c(
  0,
  max(tox_time$total_mc_converted, na.rm = TRUE) * 1.15
)

make_tox_lake_plot <- function(lake_name) {
  
  plot_data <- tox_time %>%
    filter(lake_label == lake_name)
  
  p <- ggplot(
    plot_data,
    aes(
      x = sample_date,
      y = total_mc_converted,
      color = method_site,
      group = method_site
    )
  ) +
    geom_line(linewidth = 0.8, alpha = 0.85) +
    geom_point(size = 2) +
    scale_color_manual(values = method_site_colors, drop = FALSE) +
    scale_y_continuous(
      trans = "log1p",
      limits = mc_y_limits,
      labels = label_number(accuracy = 0.1)
    ) +
    date_scale_thesis() +
    labs(
      title = lake_name,
      x = NULL,
      y = expression(Total~microcystins),
      color = NULL
    ) +
    theme_thesis() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0),
      axis.text.x = element_text(
        angle = 45,
        hjust = 1,
        vjust = 1,
        size = 7.5
      )
    )
  
  return(p)
}

tox_all_lakes_fig <- ggplot(
  tox_time,
  aes(
    x = sample_date,
    y = total_mc_converted,
    color = method_site,
    group = method_site
  )
) +
  geom_line(linewidth = 0.8, alpha = 0.85) +
  geom_point(size = 2) +
  facet_wrap(~ lake_label, nrow = 1) +
  scale_color_manual(values = method_site_colors, drop = FALSE) +
  scale_y_continuous(
    trans = "log1p",
    limits = mc_y_limits,
    breaks = c(
      0, 0.5, 1, 2, 5, 10, 20, 50, 100
    ),
    minor_breaks = c(
      0.1, 0.2, 0.3, 0.4,
      1.5, 3, 4,
      15, 30, 40, 60, 70, 80, 90
    ),
    labels = label_number(accuracy = 0.1)
  ) +
  date_scale_thesis() +
  labs(
    x = NULL,
    y = "Total microcystins\n(Grab = µg/L; SPATT = µg/g resin)",
    color = NULL
  ) +
  theme_thesis() +
  theme_lake_strip() +
  theme(
    panel.grid.major.y = element_line(color = "grey80", linewidth = 0.3),
    panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.2),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1,
      size = 7.5
    ),
    axis.text.y = element_text(
      margin = margin(r = 8)
    ),
    
    axis.title.y = element_text(
      margin = margin(r = 12)
    )
  )
tox_all_lakes_fig
save_thesis_fig(
  tox_all_lakes_fig,
  file.path(fig_dir, "tox_concentration_lines_all_lakes_method_site.png"),
  width = thesis_figures$width,
  height = 5
)

# ======================================================================
# Heatmap: total microcystins by method/site type
# Sampling dates within 2 days are grouped as the same event
# ======================================================================

library(tidyverse)
library(lubridate)
library(scales)
library(cowplot)
library(viridis)
library(grid)

source("theme_thesis.R")

tox_plot <- readRDS(
  "~/Desktop/Project/Brooks_lake_2025/data_clean/toxins/tox_plot_converted.rds"
)

fig_dir <- "~/Desktop/Project/Brooks_lake_2025/figures/toxins"
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

lake_lookup <- c(
  "brooks" = "Brooks Lake",
  "lower jade" = "Lower Jade Lake",
  "rainbow" = "Rainbow Lake",
  "upper brooks" = "Upper Brooks Lake"
)

lake_order <- c(
  "Brooks Lake",
  "Lower Jade Lake",
  "Rainbow Lake",
  "Upper Brooks Lake"
)

# ----------------------------------------------------------------------
# Clean toxin data
# ----------------------------------------------------------------------

tox_heatmap_base <- tox_plot %>%
  mutate(
    lake_label = recode(lake, !!!lake_lookup),
    lake_label = factor(lake_label, levels = lake_order),
    method = str_to_lower(method),
    site_type = str_to_lower(site_type),
    sample_date = as.Date(sample_date),
    
    depth_clean = case_when(
      site_type == "buoy_surface" ~ "Surface",
      site_type == "buoy_depth" ~ "Depth",
      TRUE ~ NA_character_
    ),
    
    method_clean = case_when(
      method == "grab" ~ "Grab",
      method == "spatt" ~ "SPATT",
      TRUE ~ NA_character_
    ),
    
    method_site = paste(method_clean, depth_clean),
    method_site = factor(
      method_site,
      levels = c(
        "SPATT Depth",
        "Grab Depth",
        "SPATT Surface",
        "Grab Surface"
      )
    )
  ) %>%
  filter(
    lake_label %in% lake_order,
    method %in% c("grab", "spatt"),
    site_type %in% c("buoy_surface", "buoy_depth"),
    !is.na(method_site),
    !is.na(total_mc_converted)
  )

# ----------------------------------------------------------------------
# Group sampling dates into events if they are within 2 days
# ----------------------------------------------------------------------

event_date_lookup <- tox_heatmap_base %>%
  distinct(sample_date) %>%
  arrange(sample_date) %>%
  mutate(
    days_since_previous = as.numeric(sample_date - lag(sample_date)),
    new_event = if_else(
      is.na(days_since_previous) | days_since_previous > 2,
      1,
      0
    ),
    event_number = cumsum(new_event)
  )

event_lookup <- event_date_lookup %>%
  group_by(event_number) %>%
  summarise(
    event_start = min(sample_date),
    event_end = max(sample_date),
    event_label = if_else(
      event_start == event_end,
      format(event_start, "%b %d"),
      paste0(format(event_start, "%b %d"), "-", format(event_end, "%d"))
    ),
    .groups = "drop"
  )

tox_heatmap <- tox_heatmap_base %>%
  left_join(
    event_date_lookup %>%
      select(sample_date, event_number),
    by = "sample_date"
  ) %>%
  left_join(event_lookup, by = "event_number") %>%
  mutate(
    sample_event = factor(
      event_label,
      levels = event_lookup$event_label
    )
  )

# Check event grouping
event_lookup

# ----------------------------------------------------------------------
# Plot heatmap
# ----------------------------------------------------------------------

tox_heatmap_fig <- ggplot(
  tox_heatmap,
  aes(
    x = sample_event,
    y = method_site,
    fill = total_mc_converted
  )
) +
  geom_tile(
    color = "white",
    linewidth = 0.45,
    width = 0.95,
    height = 0.95
  ) +
  facet_wrap(~ lake_label, nrow = 1) +
  scale_fill_viridis_c(
    option = "viridis",
    trans = "log1p",
    breaks = c(
      0, 0.5, 1, 2, 5, 10, 20, 50, 100
    ),
    labels = c(
      "0", "0.5", "1", "2", "5", "10", "20", "50", "100"
    ),
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = unit(10, "cm"),
      barheight = unit(0.45, "cm")
    ),
    name = "Total microcystins\n(Grab = µg/L; SPATT = µg/g resin)"
  ) +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_thesis() +
  theme_lake_strip() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1,
      size = 8
    ),
    axis.text.y = element_text(size = 8),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    panel.grid = element_blank()
  )

tox_heatmap_fig

save_thesis_fig(
  tox_heatmap_fig,
  file.path(fig_dir, "tox_concentration_heatmap_all_lakes_grouped_events.png"),
  width = thesis_figures$width,
  height = 5.5
)


## combine line and heatmap
# ======================================================================
# Combined panel figure: line plot + heatmap
# ======================================================================

# Remove legends from individual plots
tox_lines_clean <- tox_all_lakes_fig +
  theme(
    legend.position = "none",
    axis.title.x = element_blank()
  )

tox_heatmap_clean <- tox_heatmap_fig +
  theme(
    legend.position = "none"
  )

# Extract legends
legend_lines <- cowplot::get_legend(
  tox_all_lakes_fig +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal"
    )
)

legend_heatmap <- cowplot::get_legend(
  tox_heatmap_fig +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal"
    )
)

# Add panel labels
panel_A <- cowplot::ggdraw(tox_lines_clean) +
  cowplot::draw_label(
    "A",
    x = 0.01,
    y = 0.98,
    hjust = 0,
    vjust = 1,
    fontface = "bold",
    size = 14
  )

panel_B <- cowplot::ggdraw(tox_heatmap_clean) +
  cowplot::draw_label(
    "B",
    x = 0.01,
    y = 0.98,
    hjust = 0,
    vjust = 1,
    fontface = "bold",
    size = 14
  )

# Combine into final figure
tox_combined_panel <- cowplot::plot_grid(
  panel_A,
  legend_lines,
  panel_B,
  legend_heatmap,
  ncol = 1,
  rel_heights = c(
    1.05,
    0.18,
    1.10,
    0.22
  )
)

tox_combined_panel

save_thesis_fig(
  tox_combined_panel,
  file.path(fig_dir, "tox_concentration_lines_heatmap_panel.png"),
  width = thesis_figures$width,
  height = 9
)

#### plotting shore for all akes, 
# ======================================================================
# Shoreline SPATT microcystins over time
# ======================================================================

shore_tox <- tox_plot %>%
  mutate(
    lake_label = recode(lake, !!!lake_lookup),
    lake_label = factor(lake_label, levels = lake_order),
    method = str_to_lower(method),
    site_type = str_to_lower(site_type),
    sample_date = as.Date(sample_date)
  ) %>%
  filter(
    lake_label %in% lake_order,
    site_type == "shore",
    method == "spatt",
    !is.na(total_mc_converted)
  )

shore_tox_fig <- ggplot(
  shore_tox,
  aes(
    x = sample_date,
    y = total_mc_converted,
    color = site_id,
    group = site_id
  )
) +
  geom_line(linewidth = 0.7, alpha = 0.8) +
  geom_point(size = 1.8, alpha = 0.9) +
  facet_wrap(~ lake_label, nrow = 1) +
  scale_y_continuous(
    trans = "log1p",
    breaks = c(0, 0.5, 1, 2, 5, 10, 20, 50, 100),
    minor_breaks = c(
      0.1, 0.2, 0.3, 0.4,
      1.5, 3, 4,
      6, 7, 8, 9,
      15, 30, 40, 60, 70, 80, 90
    ),
    labels = label_number(accuracy = 0.1)
  ) +
  date_scale_thesis() +
  labs(
    x = NULL,
    y = "Total microcystins\n(µg/g resin)",
    color = "Shore site"
  ) +
  theme_thesis() +
  theme_lake_strip() +
  theme(
    panel.grid.major.y = element_line(color = "grey80", linewidth = 0.3),
    panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.2),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1,
      size = 7.5
    ),
    legend.position = "bottom"
  )

shore_tox_fig

save_thesis_fig(
  shore_tox_fig,
  file.path(fig_dir, "shore_spatt_microcystins_over_time.png"),
  width = thesis_figures$width,
  height = 5
)