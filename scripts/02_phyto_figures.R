

###
# ======================================================================
# Phytoplankton / cyanobacteria community panel figure
# ======================================================================

library(tidyverse)
library(scales)
library(cowplot)

source("Scripts/theme_thesis.R")

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
####### add chlorply-a

# ======================================================================
# Phytoplankton, cyanobacteria, and chlorophyll-a panel figure
#
# Panel A: Phytoplankton relative abundance
# Panel B: Cyanobacteria relative abundance
# Panel C: Cyanobacteria cell abundance
# Panel D: Surface chlorophyll-a concentrations
# ======================================================================

library(tidyverse)
library(scales)
library(cowplot)

source("Scripts/theme_thesis.R")


# ======================================================================
# Read data
# ======================================================================

phyto_clean <- readRDS(
  "~/Desktop/Project/Brooks_lake_2025/data_clean/phytoplankton/phyto_clean.rds"
)

fig_dir <- "~/Desktop/Project/Brooks_lake_2025/figures/phytoplankton"

dir.create(
  fig_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# ======================================================================
# Lake names and order
# ======================================================================

lake_order <- c(
  "Brooks Lake",
  "Lower Jade Lake",
  "Rainbow Lake",
  "Upper Brooks Lake"
)

lake_lookup <- c(
  "brooks" = "Brooks Lake",
  "brooks lake" = "Brooks Lake",
  "lower jade" = "Lower Jade Lake",
  "lower jade lake" = "Lower Jade Lake",
  "rainbow" = "Rainbow Lake",
  "rainbow lake" = "Rainbow Lake",
  "upper brooks" = "Upper Brooks Lake",
  "upper brooks lake" = "Upper Brooks Lake"
)


# ======================================================================
# Phytoplankton data preparation
# ======================================================================

phyto_4lakes <- phyto_clean %>%
  mutate(
    lake = str_to_lower(lake),
    
    lake = recode(
      lake,
      !!!lake_lookup,
      .default = str_to_title(lake)
    ),
    
    lake = factor(
      lake,
      levels = lake_order
    ),
    
    date = as.Date(date),
    division = str_to_lower(division)
  ) %>%
  filter(
    lake %in% lake_order
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
  group_by(
    lake,
    date,
    division
  ) %>%
  summarise(
    cells = sum(total_cells, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(
    lake,
    date
  ) %>%
  mutate(
    rel_abundance =
      cells / sum(cells, na.rm = TRUE)
  ) %>%
  ungroup()


cyano_taxa <- phyto_4lakes %>%
  filter(
    division == "cyanophyta"
  ) %>%
  group_by(
    lake,
    date,
    taxon
  ) %>%
  summarise(
    cells = sum(total_cells, na.rm = TRUE),
    .groups = "drop"
  )


cyano_taxa_rel <- cyano_taxa %>%
  group_by(
    lake,
    date
  ) %>%
  mutate(
    rel_abundance =
      cells / sum(cells, na.rm = TRUE)
  ) %>%
  ungroup()


# ======================================================================
# Dominant cyanobacteria taxa table
# ======================================================================

dominant_cyano_taxa <- cyano_taxa %>%
  group_by(
    lake,
    taxon
  ) %>%
  summarise(
    total_cyano_cells = sum(
      cells,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  group_by(lake) %>%
  mutate(
    percent_of_cyano =
      total_cyano_cells /
      sum(total_cyano_cells, na.rm = TRUE) *
      100
  ) %>%
  ungroup() %>%
  arrange(
    lake,
    desc(total_cyano_cells)
  )


write_csv(
  dominant_cyano_taxa,
  file.path(
    fig_dir,
    "dominant_cyano_taxa_by_lake.csv"
  )
)


# ======================================================================
# Chlorophyll-a data preparation
# Use only samples where type == "surface"
# ======================================================================

chla_summary <- deq_nutrients_clean_2025 %>%
  mutate(
    lake = str_to_lower(lake),
    
    lake = recode(
      lake,
      !!!lake_lookup,
      .default = str_to_title(lake)
    ),
    
    lake = factor(
      lake,
      levels = lake_order
    ),
    
    date = as.Date(date),
    type = str_to_lower(str_trim(type)),
    chla = as.numeric(chla)
  ) %>%
  filter(
    lake %in% lake_order,
    type == "surface",
    !is.na(date),
    !is.na(chla)
  ) %>%
  arrange(
    lake,
    date
  )


# Check that each lake-date combination has only one surface value

chla_summary %>%
  count(
    lake,
    date
  ) %>%
  filter(n > 1)


# ======================================================================
# Shared y-axis limits for Panel C
# ======================================================================

cyano_min <- min(
  cyano_taxa$cells[
    cyano_taxa$cells > 0
  ],
  na.rm = TRUE
)

cyano_max <- max(
  cyano_taxa$cells,
  na.rm = TRUE
)

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
# Shared y-axis limits for Panel D
# ======================================================================

chla_y_limits <- c(
  0,
  max(
    chla_summary$chla,
    na.rm = TRUE
  ) * 1.10
)


# ======================================================================
# Panel A: Phytoplankton relative abundance
# ======================================================================

pA <- ggplot(
  phyto_division,
  aes(
    x = date,
    y = rel_abundance,
    fill = division
  )
) +
  geom_col(
    width = 10,
    alpha = 0.9
  ) +
  facet_wrap(
    ~lake,
    nrow = 1
  ) +
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

pA_clean <- pA +
  theme_no_legend() +
  theme(
    plot.margin = margin(
      t = 5,
      r = 5,
      b = 20,   # increase this
      l = 5
    )
  )

# ======================================================================
# Panel B: Cyanobacteria relative abundance
# ======================================================================

pB <- ggplot(
  cyano_taxa_rel,
  aes(
    x = date,
    y = rel_abundance,
    fill = taxon
  )
) +
  geom_col(
    width = 10,
    alpha = 0.9
  ) +
  facet_wrap(
    ~lake,
    nrow = 1
  ) +
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

pB_clean <- pB +
  theme_no_legend()


# ======================================================================
# Panel C: Cyanobacteria cell abundance
# ======================================================================

pC <- ggplot(
  cyano_taxa,
  aes(
    x = date,
    y = cells,
    color = taxon,
    group = taxon
  )
) +
  geom_line(
    linewidth = 0.9
  ) +
  geom_point(
    size = 1.8
  ) +
  facet_wrap(
    ~lake,
    nrow = 1
  ) +
  scale_y_log10(
    limits = cyano_y_limits,
    breaks = cyano_y_breaks,
    labels = label_scientific(),
    expand = expansion(
      mult = c(0.05, 0.08)
    )
  ) +
  date_scale_thesis() +
  labs(
    x = NULL,
    y = expression(
      Cyanobacteria~cells~L^{-1}
    ),
    color = NULL
  ) +
  theme_thesis() +
  theme_lake_strip() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

legend_C <- legend_row(
  pC,
  aesthetic = "color",
  nrow = 1
)

pC_clean <- pC +
  theme_no_legend()


# ======================================================================
# Panel D: Surface chlorophyll-a concentrations
# ======================================================================

pD <- ggplot(
  chla_summary,
  aes(
    x = date,
    y = chla,
    group = 1
  )
) +
  geom_line(
    linewidth = 0.9,
    alpha = 0.85
  ) +
  geom_point(
    size = 2
  ) +
  facet_wrap(
    ~lake,
    nrow = 1
  ) +
  scale_y_continuous(
    limits = chla_y_limits,
    labels = label_number(
      accuracy = 0.1
    ),
    expand = expansion(
      mult = c(0, 0.08)
    )
  ) +
  date_scale_thesis() +
  labs(
    x = NULL,
    y = expression(
      Chlorophyll*"-"*a~
        "("*mu*g~L^{-1}*")"
    )
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

pD_clean <- pD


# ======================================================================
# Add panel titles
# ======================================================================

panel_A <- panel_title(
  pA_clean,
  "A",
  "Phytoplankton relative abundance"
)

panel_B <- panel_title(
  pB_clean,
  "B",
  "Cyanobacteria relative abundance"
)

panel_C <- panel_title(
  pC_clean,
  "C",
  "Cyanobacteria cell abundance"
)

panel_D <- panel_title(
  pD_clean,
  "D",
  "Surface chlorophyll-a concentrations"
)



# ======================================================================
# Final four-panel figure
# ======================================================================

phyto_cyano_chla_panel_publication <- publication_panel_4row(
  row_A = pA_clean,
  row_B = pB_clean,
  row_C = pC_clean,
  row_D = pD_clean,
  
  legend_A = legend_A,
  legend_B = NULL,
  legend_C = legend_C,
  legend_D = NULL,
  
  titles = c(
    "Phytoplankton relative abundance",
    "Cyanobacteria relative abundance",
    "Cyanobacteria cell abundance",
    "Surface chlorophyll-a concentrations"
  ),
  
  panel_spacing = 0.12
)

phyto_cyano_chla_panel_publication

# ======================================================================
# Save figure
# ======================================================================

save_thesis_fig(
  phyto_cyano_chla_panel_publication,
  file.path(
    fig_dir,
    "phyto_cyano_chla_panel_publication_layout.png"
  ),
  width = 15,
  height = 15.5
)


####### 

# ======================================================================
# Phytoplankton, cyanobacteria, and chlorophyll-a panel figure
#
# Panel A: Phytoplankton relative abundance
# Panel B: Cyanobacteria relative abundance
# Panel C: Cyanobacteria cell abundance
# Panel D: Surface chlorophyll-a concentrations
# ======================================================================

library(tidyverse)
library(scales)
library(cowplot)

source("Scripts/theme_thesis.R")


# ======================================================================
# Read data
# ======================================================================

phyto_clean <- readRDS(
  "~/Desktop/Project/Brooks_lake_2025/data_clean/phytoplankton/phyto_clean.rds"
)

deq_nutrients_clean_2025 <- readRDS(
  "~/Desktop/Project/Brooks_lake_2025/data_clean/deq/deq_nutrients_clean_2025.rds"
)

fig_dir <- "~/Desktop/Project/Brooks_lake_2025/figures/phytoplankton"

dir.create(
  fig_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# ======================================================================
# Lake-name lookup
# ======================================================================

lake_lookup <- c(
  "brooks" = "Brooks Lake",
  "brooks lake" = "Brooks Lake",
  
  "lower jade" = "Lower Jade Lake",
  "lower jade lake" = "Lower Jade Lake",
  
  "rainbow" = "Rainbow Lake",
  "rainbow lake" = "Rainbow Lake",
  
  "upper brooks" = "Upper Brooks Lake",
  "upper brooks lake" = "Upper Brooks Lake"
)


# ======================================================================
# Phytoplankton data preparation
# ======================================================================

phyto_4lakes <- phyto_clean %>%
  mutate(
    lake = str_to_lower(str_trim(lake)),
    
    lake = recode(
      lake,
      !!!lake_lookup,
      .default = str_to_title(lake)
    ),
    
    lake = factor(
      lake,
      levels = lake_order
    ),
    
    date = as.Date(date),
    division = str_to_lower(str_trim(division))
  ) %>%
  filter(
    lake %in% lake_order,
    !is.na(date)
  )


# ======================================================================
# Phytoplankton division relative abundance
# ======================================================================

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
  group_by(
    lake,
    date,
    division
  ) %>%
  summarise(
    cells = sum(
      total_cells,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  group_by(
    lake,
    date
  ) %>%
  mutate(
    sample_total_cells = sum(
      cells,
      na.rm = TRUE
    ),
    
    rel_abundance = if_else(
      sample_total_cells > 0,
      cells / sample_total_cells,
      NA_real_
    )
  ) %>%
  ungroup()


# ======================================================================
# Cyanobacteria taxon abundance
# ======================================================================

cyano_taxa <- phyto_4lakes %>%
  filter(
    division == "cyanophyta"
  ) %>%
  group_by(
    lake,
    date,
    taxon
  ) %>%
  summarise(
    cells = sum(
      total_cells,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


cyano_taxa_rel <- cyano_taxa %>%
  group_by(
    lake,
    date
  ) %>%
  mutate(
    sample_total_cyano = sum(
      cells,
      na.rm = TRUE
    ),
    
    rel_abundance = if_else(
      sample_total_cyano > 0,
      cells / sample_total_cyano,
      NA_real_
    )
  ) %>%
  ungroup()


# ======================================================================
# Dominant cyanobacteria taxa table
# ======================================================================

dominant_cyano_taxa <- cyano_taxa %>%
  group_by(
    lake,
    taxon
  ) %>%
  summarise(
    total_cyano_cells = sum(
      cells,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  group_by(lake) %>%
  mutate(
    percent_of_cyano =
      total_cyano_cells /
      sum(total_cyano_cells, na.rm = TRUE) *
      100
  ) %>%
  ungroup() %>%
  arrange(
    lake,
    desc(total_cyano_cells)
  )


write_csv(
  dominant_cyano_taxa,
  file.path(
    fig_dir,
    "dominant_cyano_taxa_by_lake.csv"
  )
)


# ======================================================================
# Chlorophyll-a preparation
# Use only rows where type == "surface"
# ======================================================================

chla_summary <- deq_nutrients_clean_2025 %>%
  mutate(
    lake = str_to_lower(str_trim(lake)),
    
    lake = recode(
      lake,
      !!!lake_lookup,
      .default = str_to_title(lake)
    ),
    
    lake = factor(
      lake,
      levels = lake_order
    ),
    
    date = as.Date(date),
    type = str_to_lower(str_trim(type)),
    chla = as.numeric(chla)
  ) %>%
  filter(
    lake %in% lake_order,
    type == "surface",
    !is.na(date),
    !is.na(chla)
  ) %>%
  arrange(
    lake,
    date
  )


# Verify that surface filtering produced one value per lake and date

chla_duplicates <- chla_summary %>%
  count(
    lake,
    date
  ) %>%
  filter(n > 1)

chla_duplicates


# ======================================================================
# Panel C y-axis limits
# ======================================================================

cyano_positive <- cyano_taxa %>%
  filter(
    is.finite(cells),
    cells > 0
  )


cyano_min <- min(
  cyano_positive$cells,
  na.rm = TRUE
)

cyano_max <- max(
  cyano_positive$cells,
  na.rm = TRUE
)


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
# Panel D y-axis limits
# ======================================================================

chla_y_limits <- c(
  0,
  max(
    chla_summary$chla,
    na.rm = TRUE
  ) * 1.10
)


# ======================================================================
# Panel A: Phytoplankton relative abundance
# ======================================================================

pA <- ggplot(
  phyto_division,
  aes(
    x = date,
    y = rel_abundance,
    fill = division
  )
) +
  geom_col(
    width = 10,
    alpha = 0.9
  ) +
  facet_wrap(
    ~lake,
    nrow = 1
  ) +
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
  nrow = 2,
  top_margin = 8
)


pA_clean <- pA +
  theme_no_legend()


# ======================================================================
# Panel B: Cyanobacteria relative abundance
# ======================================================================

pB <- ggplot(
  cyano_taxa_rel,
  aes(
    x = date,
    y = rel_abundance,
    fill = taxon
  )
) +
  geom_col(
    width = 10,
    alpha = 0.9
  ) +
  facet_wrap(
    ~lake,
    nrow = 1
  ) +
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


pB_clean <- pB +
  theme_no_legend()


# ======================================================================
# Panel C: Cyanobacteria cell abundance
# ======================================================================

pC <- ggplot(
  cyano_positive,
  aes(
    x = date,
    y = cells,
    color = taxon,
    group = taxon
  )
) +
  geom_line(
    linewidth = 0.9,
    na.rm = TRUE
  ) +
  geom_point(
    size = 1.8,
    na.rm = TRUE
  ) +
  facet_wrap(
    ~lake,
    nrow = 1
  ) +
  scale_y_log10(
    limits = cyano_y_limits,
    breaks = cyano_y_breaks,
    labels = label_scientific(),
    expand = expansion(
      mult = c(0.05, 0.08)
    )
  ) +
  date_scale_thesis() +
  labs(
    x = NULL,
    y = expression(
      Cyanobacteria~cells~L^{-1}
    ),
    color = NULL
  ) +
  theme_thesis() +
  theme_lake_strip() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )


legend_C <- legend_row(
  pC,
  aesthetic = "color",
  nrow = 1,
  
  # Slightly more room above this legend so it does not
  # crowd the plot above it.
  top_margin = 10,
  bottom_margin = 4
)


pC_clean <- pC +
  theme_no_legend()


# ======================================================================
# Panel D: Surface chlorophyll-a concentrations
# ======================================================================

pD <- ggplot(
  chla_summary,
  aes(
    x = date,
    y = chla,
    group = 1
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
  facet_wrap(
    ~lake,
    nrow = 1
  ) +
  scale_y_continuous(
    limits = chla_y_limits,
    labels = label_number(
      accuracy = 0.1
    ),
    expand = expansion(
      mult = c(0, 0.08)
    )
  ) +
  date_scale_thesis() +
  labs(
    x = NULL,
    y = expression(
      Chlorophyll*"-"*a~
        "("*mu*g~L^{-1}*")"
    )
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


pD_clean <- pD +
  theme_no_legend()


# ======================================================================
# Final four-panel figure
#
# Layout rows:
# 1. Panel A
# 2. Legend A
# 3. Spacer
# 4. Panel B
# 5. Spacer
# 6. Panel C
# 7. Legend C
# 8. Spacer
# 9. Panel D
# ======================================================================

phyto_cyano_chla_panel_publication <- publication_panel_4row(
  row_A = pA_clean,
  row_B = pB_clean,
  row_C = pC_clean,
  row_D = pD_clean,
  
  legend_A = legend_A,
  legend_B = NULL,
  legend_C = legend_C,
  legend_D = NULL,
  
  titles = c(
    "Phytoplankton relative abundance",
    "Cyanobacteria relative abundance",
    "Cyanobacteria cell abundance",
    "Surface chlorophyll-a concentrations"
  ),
  
  heights = c(
    1.20,  # Panel A
    0.42,  # Panel A legend
    0.10,  # Spacer
    
    1.05,  # Panel B
    0.10,  # Spacer
    
    1.35,  # Panel C
    0.36,  # Panel C legend
    0.10,  # Spacer
    
    1.15   # Panel D
  )
)


phyto_cyano_chla_panel_publication


# ======================================================================
# Save figure
# ======================================================================

save_thesis_fig(
  phyto_cyano_chla_panel_publication,
  
  file.path(
    fig_dir,
    "phyto_cyano_chla_panel_publication_layout.png"
  ),
  
  width = thesis_figures$width,
  height = thesis_figures$height_four_panel
)