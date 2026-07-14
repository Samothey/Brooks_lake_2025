

###
# ======================================================================
# Phytoplankton / cyanobacteria community panel figure
# ======================================================================

library(tidyverse)
library(scales)
library(cowplot)

source("theme_thesis.R")

phyto_clean <- readRDS(
  "~/Desktop/Project/Brooks_lake_2025/data_clean/phytoplankton/phyto_clean.rds"
)

fig_dir <- "~/Desktop/Project/Brooks_lake_2025/figures/phytoplankton"
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

# ======================================================================
# Data preparation
# ======================================================================

phyto_4lakes <- phyto_clean %>%
  filter(lake %in% lake_order) %>%
  mutate(
    lake = factor(lake, levels = lake_order),
    date = as.Date(date),
    division = str_to_lower(division)
  )

phyto_division <- phyto_4lakes %>%
  mutate(
    division = case_when(
      division == "bacillariophyta" ~ "Diatoms",
      division == "chlorophyta" ~ "Green algae",
      division == "chrysophyta" ~ "Chrysophytes",
      division == "cryptophyta" ~ "Cryptophytes",
      division == "cyanophyta" ~ "Cyanobacteria",
      division %in% c("pyrrhophyta", "rotifera") ~ "Other",
      TRUE ~ str_to_title(division)
    )
  ) %>%
  group_by(lake, date, division) %>%
  summarise(
    cells = sum(total_cells, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(lake, date) %>%
  mutate(
    rel_abundance = cells / sum(cells, na.rm = TRUE)
  ) %>%
  ungroup()

cyano_taxa <- phyto_4lakes %>%
  filter(division == "cyanophyta") %>%
  group_by(lake, date, taxon) %>%
  summarise(
    cells = sum(total_cells, na.rm = TRUE),
    .groups = "drop"
  )

cyano_taxa_rel <- cyano_taxa %>%
  group_by(lake, date) %>%
  mutate(
    rel_abundance = cells / sum(cells, na.rm = TRUE)
  ) %>%
  ungroup()

# ======================================================================
# Dominant cyanobacteria taxa table
# ======================================================================

dominant_cyano_taxa <- cyano_taxa %>%
  group_by(lake, taxon) %>%
  summarise(
    total_cyano_cells = sum(cells, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(lake) %>%
  mutate(
    percent_of_cyano = total_cyano_cells /
      sum(total_cyano_cells, na.rm = TRUE) * 100
  ) %>%
  arrange(lake, desc(total_cyano_cells))

write_csv(
  dominant_cyano_taxa,
  file.path(fig_dir, "dominant_cyano_taxa_by_lake.csv")
)

# ======================================================================
# Shared y-axis limits for Panel C
# ======================================================================

cyano_min <- min(cyano_taxa$cells[cyano_taxa$cells > 0], na.rm = TRUE)
cyano_max <- max(cyano_taxa$cells, na.rm = TRUE)

cyano_y_limits <- c(
  10^floor(log10(cyano_min)),
  10^ceiling(log10(cyano_max))
)

cyano_y_breaks <- 10^seq(
  floor(log10(cyano_y_limits[1])),
  ceiling(log10(cyano_y_limits[2])),
  by = 1
)

# ======================================================================
# Panel A: Phytoplankton relative abundance
# ======================================================================

pA <- ggplot(
  phyto_division,
  aes(x = date, y = rel_abundance, fill = division)
) +
  geom_col(width = 10, alpha = 0.9) +
  facet_wrap(~ lake, nrow = 1) +
  scale_y_continuous(
    labels = percent_format(),
    limits = c(0, 1),
    expand = c(0, 0)
  ) +
  date_scale_thesis() +
  labs(
    x = NULL,
    y = "Phytoplankton\nrelative abundance",
    fill = NULL
  ) +
  theme_thesis() +
  theme_lake_strip() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

legend_A <- legend_row(
  pA,
  aesthetic = "fill",
  nrow = 2
)

pA_clean <- pA + theme_no_legend()

# ======================================================================
# Panel B: Cyanobacteria relative abundance
# ======================================================================

pB <- ggplot(
  cyano_taxa_rel,
  aes(x = date, y = rel_abundance, fill = taxon)
) +
  geom_col(width = 10, alpha = 0.9) +
  facet_wrap(~ lake, nrow = 1) +
  scale_y_continuous(
    labels = percent_format(),
    limits = c(0, 1),
    expand = c(0, 0)
  ) +
  date_scale_thesis() +
  labs(
    x = NULL,
    y = "Cyanobacteria\nrelative abundance",
    fill = NULL
  ) +
  theme_thesis() +
  theme_lake_strip() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

legend_B <- legend_row(
  pB,
  aesthetic = "fill",
  nrow = 1
)

pB_clean <- pB + theme_no_legend()

# ======================================================================
# Panel C: Cyanobacteria cell abundance
# ======================================================================

pC <- ggplot(
  cyano_taxa,
  aes(x = date, y = cells, color = taxon)
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8) +
  facet_wrap(~ lake, nrow = 1) +
  scale_y_log10(
    limits = cyano_y_limits,
    breaks = cyano_y_breaks,
    labels = label_scientific(),
    expand = expansion(mult = c(0.05, 0.08))
  ) +
  date_scale_thesis() +
  labs(
    x = NULL,
    y = expression(Cyanobacteria~cells~L^{-1}),
    color = NULL
  ) +
  theme_thesis() +
  theme_lake_strip() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      size = 7.5
    )
  )

legend_C <- legend_row(
  pC,
  aesthetic = "color",
  nrow = 1
)

pC_clean <- pC + theme_no_legend()

# ======================================================================
# Final figure
# ======================================================================

phyto_cyano_panel_publication <- publication_panel_3row(
  row_A = pA_clean,
  row_B = pB_clean,
  row_C = pC_clean,
  legend_A = legend_A,
  legend_B = NULL,
  legend_C = legend_C,
  titles = c(
    "Phytoplankton relative abundance",
    "Cyanobacteria relative abundance",
    "Cyanobacteria cell abundance"
  ),
  heights = c(
    1.20, 0.42,   # Panel A + 2-row legend
    1.05,         # Panel B
    1.35, 0.285    # Panel C + legend
  )
)
phyto_cyano_panel_publication

save_thesis_fig(
  phyto_cyano_panel_publication,
  file.path(fig_dir, "phyto_cyano_panel_publication_layout.png"),
  width = 15,
  height = 12.5
)