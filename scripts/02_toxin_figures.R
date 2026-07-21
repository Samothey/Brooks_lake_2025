# ======================================================================
# TOXIN FIGURES
#
# Figure 1
#   A. Grab-sample buoy microcystin concentrations across lakes
#   B. Brooks Lake shoreline mean versus buoy concentrations
#   C. Brooks Lake individual shoreline-site concentrations
#
# Figure 2
#   A. Grab and SPATT microcystin concentrations through time
#   B. Grab and SPATT concentration heatmap by sampling event
# ======================================================================


# ======================================================================
# Packages and thesis theme
# ======================================================================

library(tidyverse)
library(lubridate)
library(scales)
library(cowplot)
library(viridis)
library(grid)

source("Scripts/theme_thesis.R")


# ======================================================================
# File paths
# ======================================================================

tox_raw_path <- paste0(
  "~/Desktop/Project/Brooks_lake_2025/",
  "data_clean/toxins/tox_plot.rds"
)

tox_converted_path <- paste0(
  "~/Desktop/Project/Brooks_lake_2025/",
  "data_clean/toxins/tox_plot_converted.rds"
)

fig_dir <- paste0(
  "~/Desktop/Project/Brooks_lake_2025/",
  "figures/toxins"
)

dir.create(
  fig_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# ======================================================================
# Read data
# ======================================================================

tox_plot <- readRDS(tox_raw_path)

tox_plot_converted <- readRDS(tox_converted_path)


# ======================================================================
# Shared figure settings
# ======================================================================

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


method_site_colors <- c(
  "Grab Surface" = "#E64B35",
  "Grab Depth" = "#A32020",
  "SPATT Surface" = "#00A6A6",
  "SPATT Depth" = "#006D77"
)


mc_breaks <- c(
  0,
  0.5,
  1,
  2,
  5,
  10,
  20,
  50,
  100
)


mc_minor_breaks <- c(
  0.1,
  0.2,
  0.3,
  0.4,
  1.5,
  3,
  4,
  15,
  30,
  40,
  60,
  70,
  80,
  90
)


# ======================================================================
# Shared helper: assign Brooks Lake sampling events
# ======================================================================

assign_brooks_sampling_event <- function(date) {
  
  case_when(
    
    date >= as.Date("2025-07-08") &
      date <= as.Date("2025-07-10") ~
      as.Date("2025-07-09"),
    
    date >= as.Date("2025-07-21") &
      date <= as.Date("2025-07-23") ~
      as.Date("2025-07-22"),
    
    date >= as.Date("2025-08-04") &
      date <= as.Date("2025-08-06") ~
      as.Date("2025-08-05"),
    
    date >= as.Date("2025-08-18") &
      date <= as.Date("2025-08-20") ~
      as.Date("2025-08-19"),
    
    date >= as.Date("2025-09-01") &
      date <= as.Date("2025-09-02") ~
      as.Date("2025-09-01"),
    
    date >= as.Date("2025-09-16") &
      date <= as.Date("2025-09-17") ~
      as.Date("2025-09-16"),
    
    date >= as.Date("2025-09-29") &
      date <= as.Date("2025-09-30") ~
      as.Date("2025-09-29"),
    
    date >= as.Date("2025-10-14") &
      date <= as.Date("2025-10-15") ~
      as.Date("2025-10-14"),
    
    TRUE ~ date
  )
}


# ======================================================================
# Shared Brooks Lake shoreline metadata
# ======================================================================

brooks_site_metadata <- tribble(
  ~site_id,       ~shore_region,
  "BKS_BL_SH_01", "South",
  "BKS_BL_SH_02", "South",
  "BKS_BL_SH_03", "South",
  "BKS_BL_SH_04", "West",
  "BKS_BL_SH_05", "North",
  "BKS_BL_SH_06", "North",
  "BKS_BL_SH_07", "East"
)

brooks_shore_ids <- brooks_site_metadata$site_id


# ######################################################################
# FIGURE 1
# Grab concentrations, habitat comparison, and shoreline variation
# ######################################################################


# ======================================================================
# Figure 1 data cleaning
# ======================================================================

tox_grab_clean <- tox_plot %>%
  mutate(
    lake = str_to_lower(str_trim(lake)),
    method = str_to_lower(str_trim(method)),
    site_type = str_to_lower(str_trim(site_type)),
    sample_status = str_to_lower(str_trim(sample_status)),
    sample_type = str_to_lower(str_trim(sample_type)),
    sample_date = as.Date(sample_date),
    
    lake_label = recode(
      lake,
      !!!lake_lookup,
      .default = NA_character_
    ),
    
    lake_label = factor(
      lake_label,
      levels = lake_order
    )
  ) %>%
  filter(
    method == "grab",
    !str_detect(sample_type, "duplicate|blank"),
    sample_status != "not_analyzed",
    !is.na(total_mc)
  )


# ======================================================================
# Figure 1, Panel A data
# ======================================================================

grab_buoy <- tox_grab_clean %>%
  filter(
    lake_label %in% lake_order,
    site_type %in% c(
      "buoy_surface",
      "buoy_depth"
    )
  ) %>%
  mutate(
    depth = case_when(
      site_type == "buoy_surface" ~ "Surface",
      site_type == "buoy_depth" ~ "Depth",
      TRUE ~ NA_character_
    ),
    
    depth = factor(
      depth,
      levels = c(
        "Surface",
        "Depth"
      )
    )
  ) %>%
  filter(
    !is.na(depth)
  )


# ======================================================================
# Figure 1 Brooks Lake data
# ======================================================================

brooks_grab <- tox_grab_clean %>%
  filter(
    lake == "brooks"
  ) %>%
  mutate(
    sampling_event = assign_brooks_sampling_event(
      sample_date
    )
  )


# ======================================================================
# Figure 1 shared y-axis limits
# ======================================================================

grab_shared_y_max <- max(
  grab_buoy$total_mc,
  brooks_grab$total_mc,
  na.rm = TRUE
) * 1.15


grab_shared_y_limits <- c(
  0,
  grab_shared_y_max
)


# ======================================================================
# Figure 1, Panel A
# Grab-sample buoy concentrations across lakes
# ======================================================================

grab_fig_pA <- ggplot(
  grab_buoy,
  aes(
    x = sample_date,
    y = total_mc,
    color = depth,
    group = depth
  )
) +
  geom_line(
    linewidth = 0.8,
    alpha = 0.85,
    na.rm = TRUE
  ) +
  geom_point(
    size = 2,
    na.rm = TRUE
  ) +
  facet_wrap(
    ~lake_label,
    nrow = 1
  ) +
  scale_color_manual(
    values = depth_colors,
    drop = FALSE
  ) +
  scale_y_continuous(
    trans = "log1p",
    limits = grab_shared_y_limits,
    labels = label_number(
      accuracy = 0.1
    )
  ) +
  date_scale_thesis() +
  labs(
    x = NULL,
    y = expression(
      Total~microcystins~
        "("*mu*g~L^{-1}*")"
    ),
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


grab_fig_legend_A <- legend_row(
  grab_fig_pA,
  aesthetic = "color",
  nrow = 1,
  top_margin = 8,
  bottom_margin = 4
)


grab_fig_pA_clean <- grab_fig_pA +
  theme_no_legend()


# ======================================================================
# Figure 1, Panel B data
# Brooks shoreline mean versus buoy
# ======================================================================

brooks_shore_mean <- brooks_grab %>%
  filter(
    site_id %in% brooks_shore_ids
  ) %>%
  group_by(
    sampling_event
  ) %>%
  summarise(
    total_mc = mean(
      total_mc,
      na.rm = TRUE
    ),
    
    n_sites = sum(
      !is.na(total_mc)
    ),
    
    .groups = "drop"
  ) %>%
  filter(
    n_sites > 0,
    is.finite(total_mc)
  ) %>%
  transmute(
    sample_date = sampling_event,
    total_mc,
    habitat = "Shore mean"
  )


brooks_buoy <- brooks_grab %>%
  filter(
    site_type %in% c(
      "buoy_surface",
      "buoy_depth"
    )
  ) %>%
  transmute(
    sample_date = sampling_event,
    total_mc,
    
    habitat = case_when(
      site_type == "buoy_surface" ~ "Buoy surface",
      site_type == "buoy_depth" ~ "Buoy depth",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(
    !is.na(habitat)
  )


brooks_habitat <- bind_rows(
  brooks_shore_mean,
  brooks_buoy
) %>%
  mutate(
    habitat = factor(
      habitat,
      levels = c(
        "Shore mean",
        "Buoy surface",
        "Buoy depth"
      )
    )
  )


# ======================================================================
# Figure 1, Panel B
# Brooks shoreline mean versus buoy
# ======================================================================

grab_fig_pB <- ggplot(
  brooks_habitat,
  aes(
    x = sample_date,
    y = total_mc,
    color = habitat,
    group = habitat
  )
) +
  geom_line(
    linewidth = 0.9,
    alpha = 0.85,
    na.rm = TRUE
  ) +
  geom_point(
    size = 2,
    na.rm = TRUE
  ) +
  scale_color_manual(
    values = habitat_colors,
    drop = FALSE
  ) +
  scale_y_continuous(
    trans = "log1p",
    limits = grab_shared_y_limits,
    labels = label_number(
      accuracy = 0.1
    )
  ) +
  date_scale_thesis() +
  labs(
    x = NULL,
    y = expression(
      Total~microcystins~
        "("*mu*g~L^{-1}*")"
    ),
    color = NULL
  ) +
  theme_thesis() +
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


grab_fig_legend_B <- legend_row(
  grab_fig_pB,
  aesthetic = "color",
  nrow = 1,
  top_margin = 8,
  bottom_margin = 4
)


grab_fig_pB_clean <- grab_fig_pB +
  theme_no_legend()


# ======================================================================
# Figure 1, Panel C data
# Individual Brooks shoreline sites
# ======================================================================

brooks_shore_sites <- brooks_grab %>%
  filter(
    site_id %in% brooks_shore_ids
  ) %>%
  left_join(
    brooks_site_metadata,
    by = "site_id"
  ) %>%
  mutate(
    shore_region = factor(
      shore_region,
      levels = c(
        "North",
        "South",
        "East",
        "West"
      )
    )
  ) %>%
  filter(
    !is.na(shore_region)
  )


# ======================================================================
# Figure 1, Panel C
# Individual shoreline sites
# ======================================================================

grab_fig_pC <- ggplot(
  brooks_shore_sites,
  aes(
    x = sampling_event,
    y = total_mc,
    color = shore_region,
    group = site_id
  )
) +
  geom_line(
    linewidth = 0.8,
    alpha = 0.75,
    na.rm = TRUE
  ) +
  geom_point(
    size = 2,
    na.rm = TRUE
  ) +
  scale_color_manual(
    values = shore_region_colors,
    drop = FALSE
  ) +
  scale_y_continuous(
    trans = "log1p",
    limits = grab_shared_y_limits,
    labels = label_number(
      accuracy = 0.1
    )
  ) +
  date_scale_thesis() +
  labs(
    x = NULL,
    y = expression(
      Total~microcystins~
        "("*mu*g~L^{-1}*")"
    ),
    color = NULL
  ) +
  theme_thesis() +
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


grab_fig_legend_C <- legend_row(
  grab_fig_pC,
  aesthetic = "color",
  nrow = 1,
  top_margin = 10,
  bottom_margin = 4
)


grab_fig_pC_clean <- grab_fig_pC +
  theme_no_legend()


# ======================================================================
# Assemble Figure 1
# ======================================================================

tox_grab_habitat_shore_fig <- publication_panel_3row(
  row_A = grab_fig_pA_clean,
  row_B = grab_fig_pB_clean,
  row_C = grab_fig_pC_clean,
  
  legend_A = grab_fig_legend_A,
  legend_B = grab_fig_legend_B,
  legend_C = grab_fig_legend_C,
  
  titles = c(
    "Grab-sample microcystin concentrations",
    "Brooks Lake shoreline mean and buoy concentrations",
    "Brooks Lake individual shoreline site concentrations"
  ),
  
  heights = c(
    1.20,
    0.32,
    0.10,
    
    1.10,
    0.32,
    0.10,
    
    1.10,
    0.36
  )
)


tox_grab_habitat_shore_fig


# ======================================================================
# Save Figure 1
# ======================================================================

save_thesis_fig(
  tox_grab_habitat_shore_fig,
  
  file.path(
    fig_dir,
    "tox_grab_habitat_shore_combined.png"
  ),
  
  width = 12,
  height = 12
)


# ######################################################################
# FIGURE 2
# Grab and SPATT concentrations and heatmap
# ######################################################################


# ======================================================================
# Figure 2 data cleaning
# ======================================================================

tox_time <- tox_plot_converted %>%
  mutate(
    lake = str_to_lower(str_trim(lake)),
    method = str_to_lower(str_trim(method)),
    site_type = str_to_lower(str_trim(site_type)),
    sample_date = as.Date(sample_date),
    
    lake_label = recode(
      lake,
      !!!lake_lookup,
      .default = NA_character_
    ),
    
    lake_label = factor(
      lake_label,
      levels = lake_order
    ),
    
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
    
    method_site = paste(
      method_clean,
      depth_clean
    ),
    
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
    method %in% c(
      "grab",
      "spatt"
    ),
    site_type %in% c(
      "buoy_surface",
      "buoy_depth"
    ),
    !is.na(method_site),
    !is.na(total_mc_converted)
  )


# ======================================================================
# Figure 2 concentration limits
# ======================================================================

spatt_grab_y_limits <- c(
  0,
  max(
    tox_time$total_mc_converted,
    na.rm = TRUE
  ) * 1.15
)


# ======================================================================
# Figure 2, Panel A
# Grab and SPATT concentrations through time
# ======================================================================

spatt_fig_pA <- ggplot(
  tox_time,
  aes(
    x = sample_date,
    y = total_mc_converted,
    color = method_site,
    group = method_site
  )
) +
  geom_line(
    linewidth = 0.8,
    alpha = 0.85,
    na.rm = TRUE
  ) +
  geom_point(
    size = 2,
    na.rm = TRUE
  ) +
  facet_wrap(
    ~lake_label,
    nrow = 1
  ) +
  scale_color_manual(
    values = method_site_colors,
    drop = FALSE
  ) +
  scale_y_continuous(
    trans = "log1p",
    limits = spatt_grab_y_limits,
    breaks = mc_breaks,
    minor_breaks = mc_minor_breaks,
    labels = label_number(
      accuracy = 0.1
    )
  ) +
  date_scale_thesis() +
  labs(
    x = NULL,
    y = paste0(
      "Total microcystins\n",
      "(Grab = \u00B5g L\u207B\u00B9; ",
      "SPATT = \u00B5g g\u207B\u00B9 resin)"
    ),
    color = NULL
  ) +
  theme_thesis() +
  theme_lake_strip() +
  theme(
    panel.grid.major.y = element_line(
      color = "grey80",
      linewidth = 0.3
    ),
    
    panel.grid.minor.y = element_line(
      color = "grey90",
      linewidth = 0.2
    ),
    
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


spatt_fig_legend_A <- legend_row(
  spatt_fig_pA,
  aesthetic = "color",
  nrow = 1,
  top_margin = 8,
  bottom_margin = 4
)


spatt_fig_pA_clean <- spatt_fig_pA +
  theme_no_legend()


# ======================================================================
# Figure 2, Panel B data
# Group dates within two days into sampling events
# ======================================================================

event_date_lookup <- tox_time %>%
  distinct(
    sample_date
  ) %>%
  arrange(
    sample_date
  ) %>%
  mutate(
    days_since_previous = as.numeric(
      sample_date - lag(sample_date)
    ),
    
    new_event = if_else(
      is.na(days_since_previous) |
        days_since_previous > 2,
      1L,
      0L
    ),
    
    event_number = cumsum(
      new_event
    )
  )


event_lookup <- event_date_lookup %>%
  group_by(
    event_number
  ) %>%
  summarise(
    event_start = min(
      sample_date
    ),
    
    event_end = max(
      sample_date
    ),
    
    event_label = if_else(
      event_start == event_end,
      
      format(
        event_start,
        "%b %d"
      ),
      
      paste0(
        format(
          event_start,
          "%b %d"
        ),
        "\u2013",
        format(
          event_end,
          "%d"
        )
      )
    ),
    
    .groups = "drop"
  )


tox_heatmap <- tox_time %>%
  left_join(
    event_date_lookup %>%
      select(
        sample_date,
        event_number
      ),
    by = "sample_date"
  ) %>%
  left_join(
    event_lookup,
    by = "event_number"
  ) %>%
  mutate(
    method_site = factor(
      method_site,
      levels = c(
        "SPATT Depth",
        "Grab Depth",
        "SPATT Surface",
        "Grab Surface"
      )
    ),
    
    sample_event = factor(
      event_label,
      levels = event_lookup$event_label
    )
  )


# View event grouping if needed

event_lookup


# ======================================================================
# Figure 2, Panel B
# Concentration heatmap
# ======================================================================

spatt_fig_pB <- ggplot(
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
  facet_wrap(
    ~lake_label,
    nrow = 1
  ) +
  scale_fill_viridis_c(
    option = "viridis",
    trans = "log1p",
    breaks = mc_breaks,
    
    labels = label_number(
      accuracy = 0.1
    ),
    
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      
      barwidth = grid::unit(
        10,
        "cm"
      ),
      
      barheight = grid::unit(
        0.45,
        "cm"
      )
    ),
    
    name = paste0(
      "Total microcystins\n",
      "(Grab = \u00B5g L\u207B\u00B9; ",
      "SPATT = \u00B5g g\u207B\u00B9 resin)"
    )
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
    
    axis.text.y = element_text(
      size = 8
    ),
    
    legend.position = "bottom",
    
    legend.title = element_text(
      size = 10
    ),
    
    legend.text = element_text(
      size = 8
    ),
    
    legend.margin = margin(
      t = 8,
      r = 0,
      b = 4,
      l = 0
    ),
    
    panel.grid = element_blank()
  )


spatt_fig_legend_B <- cowplot::get_legend(
  spatt_fig_pB +
    theme(
      legend.position = "bottom"
    )
)


spatt_fig_pB_clean <- spatt_fig_pB +
  theme_no_legend()


# ======================================================================
# Assemble Figure 2
# ======================================================================

tox_grab_spatt_panel <- stacked_publication_figure(
  panels = list(
    
    panel_title(
      spatt_fig_pA_clean,
      "A",
      "Grab and SPATT microcystin concentrations"
    ),
    
    panel_title(
      spatt_fig_pB_clean,
      "B",
      "Microcystin concentrations by sampling event"
    )
  ),
  
  legends = list(
    spatt_fig_legend_A,
    spatt_fig_legend_B
  ),
  
  heights = c(
    1.20,
    0.32,
    0.12,
    1.10,
    0.40
  ),
  
  panel_spacing = 0.12,
  add_spacers = TRUE,
  align = "v",
  axis = "lr"
)


tox_grab_spatt_panel


# ======================================================================
# Save Figure 2
# ======================================================================

save_thesis_fig(
  tox_grab_spatt_panel,
  
  file.path(
    fig_dir,
    "tox_grab_spatt_lines_heatmap_panel.png"
  ),
  
  width = thesis_figures$width,
  height = 9
)