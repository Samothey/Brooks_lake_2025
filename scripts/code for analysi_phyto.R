# ======================================================================
# Cyanobacterial taxa and microcystin correlations
#
# Analyses:
#   1. Pooled across all lakes
#   2. Within individual lakes
#   3. Grab and SPATT analyzed separately
#   4. Absolute and relative taxon abundance
#
# Missing toxin sampling:
#   Phytoplankton-only events, such as June 24, remain in the complete
#   joined dataset but are excluded from correlations because no paired
#   toxin measurement exists.
# ======================================================================


# ----------------------------------------------------------------------
# 0. Packages and data
# ----------------------------------------------------------------------

library(tidyverse)
library(lubridate)


phyto_clean <- readRDS(
  "~/Desktop/Project/Brooks_lake_2025/data_clean/phytoplankton/phyto_clean.rds"
)

tox_plot_converted <- readRDS(
  "~/Desktop/Project/Brooks_lake_2025/data_clean/toxins/tox_plot_converted.rds"
)


focal_lakes <- c(
  "brooks",
  "lower jade",
  "rainbow",
  "upper brooks"
)

# ----------------------------------------------------------------------
# 1. Helper function: standardize lake names
# ----------------------------------------------------------------------

clean_lake_name <- function(x) {
  
  x %>%
    str_to_lower() %>%
    str_replace_all(" lake", "") %>%
    str_squish()
  
}


# ----------------------------------------------------------------------
# 2. Identify all real phytoplankton sampling events
# ----------------------------------------------------------------------
#
# This includes phytoplankton-only events such as June 24.
# ----------------------------------------------------------------------

phyto_events <- phyto_clean %>%
  filter(
    sample_type == "regular"
  ) %>%
  mutate(
    lake = clean_lake_name(lake),
    date = as.Date(date)
  ) %>%
  filter(
    lake %in% focal_lakes
  ) %>%
  distinct(
    lake,
    date
  ) %>%
  arrange(
    lake,
    date
  )


print(phyto_events, n = Inf)


# Count all phytoplankton events by lake
phyto_event_counts <- phyto_events %>%
  count(
    lake,
    name = "total_phyto_events"
  )


print(phyto_event_counts, n = Inf)


# ----------------------------------------------------------------------
# 3. Summarize cyanobacteria by lake, date, and taxon
# ----------------------------------------------------------------------

cyano_taxa_long <- phyto_clean %>%
  filter(
    sample_type == "regular",
    str_to_lower(division) == "cyanophyta"
  ) %>%
  mutate(
    lake = clean_lake_name(lake),
    date = as.Date(date),
    taxon = str_squish(taxon)
  ) %>%
  filter(
    lake %in% focal_lakes
  ) %>%
  group_by(
    lake,
    date,
    taxon
  ) %>%
  summarise(
    taxon_cells_l = sum(total_cells, na.rm = TRUE),
    .groups = "drop"
  )


# All cyanobacterial taxa observed anywhere in the dataset
cyano_taxa <- cyano_taxa_long %>%
  distinct(taxon) %>%
  arrange(taxon)


print(cyano_taxa, n = Inf)


# ----------------------------------------------------------------------
# 4. Add zeros for taxa absent from actual sampled events
# ----------------------------------------------------------------------
#
# A zero means:
#   - phytoplankton was sampled,
#   - the taxon was not observed.
#
# It does not create zeros for dates that were not sampled.
# ----------------------------------------------------------------------

cyano_complete <- phyto_events %>%
  crossing(cyano_taxa) %>%
  left_join(
    cyano_taxa_long,
    by = c(
      "lake",
      "date",
      "taxon"
    ),
    relationship = "one-to-one"
  ) %>%
  mutate(
    taxon_cells_l = replace_na(
      taxon_cells_l,
      0
    )
  ) %>%
  group_by(
    lake,
    date
  ) %>%
  mutate(
    
    total_cyano_cells_l = sum(
      taxon_cells_l,
      na.rm = TRUE
    ),
    
    taxon_relative_abundance = if_else(
      total_cyano_cells_l > 0,
      taxon_cells_l / total_cyano_cells_l,
      NA_real_
    ),
    
    taxon_percent = 100 * taxon_relative_abundance,
    
    cyano_present = total_cyano_cells_l > 0
    
  ) %>%
  ungroup() %>%
  arrange(
    lake,
    date,
    taxon
  )


# Inspect total cyanobacterial abundance by phytoplankton event
cyano_event_summary <- cyano_complete %>%
  distinct(
    lake,
    date,
    total_cyano_cells_l,
    cyano_present
  ) %>%
  arrange(
    lake,
    date
  )


print(cyano_event_summary, n = Inf)


# Confirm percentages sum to approximately 100%
# for events where cyanobacteria were observed
cyano_percent_check <- cyano_complete %>%
  group_by(
    lake,
    date
  ) %>%
  summarise(
    total_cyano_cells_l = first(total_cyano_cells_l),
    total_percent = sum(
      taxon_percent,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  arrange(
    lake,
    date
  )


print(cyano_percent_check, n = Inf)


# ----------------------------------------------------------------------
# 5. Inspect duplicate toxin records
# ----------------------------------------------------------------------

toxin_duplicate_check <- tox_plot_converted %>%
  filter(
    sample_type == "regular",
    site_type == "buoy_surface",
    str_to_lower(method) %in% c(
      "grab",
      "spatt"
    )
  ) %>%
  mutate(
    lake = clean_lake_name(lake),
    sample_date = as.Date(sample_date),
    method = str_to_lower(method)
  ) %>%
  count(
    lake,
    sample_date,
    method,
    name = "n_records"
  ) %>%
  filter(
    n_records > 1
  )


print(toxin_duplicate_check, n = Inf)


# ----------------------------------------------------------------------
# 6. Prepare surface-buoy toxin data
# ----------------------------------------------------------------------
#
# Missing sampling dates are not turned into zero.
#
# A toxin value of zero should only represent a sample that was
# collected and analyzed with no detected microcystin.
# ----------------------------------------------------------------------

toxin_surface <- tox_plot_converted %>%
  filter(
    sample_type == "regular",
    site_type == "buoy_surface",
    str_to_lower(method) %in% c("grab", "spatt")
  ) %>%
  mutate(
    lake = clean_lake_name(lake),
    toxin_date = as.Date(sample_date),
    method = str_to_lower(method)
  ) %>%
  filter(
    lake %in% focal_lakes
  ) %>%
  select(
    lake,
    toxin_date,
    method,
    total_mc_converted,
    sample_status
  ) %>%
  group_by(
    lake,
    toxin_date,
    method
  ) %>%
  summarise(
    total_mc_converted = if_else(
      all(is.na(total_mc_converted)),
      NA_real_,
      mean(total_mc_converted, na.rm = TRUE)
    ),
    sample_status = paste(
      sort(unique(sample_status)),
      collapse = "; "
    ),
    n_toxin_records = n(),
    .groups = "drop"
    
  ) %>%
  arrange(
    lake,
    toxin_date,
    method
  )


print(toxin_surface, n = Inf)


# ----------------------------------------------------------------------
# 7. Match phytoplankton events to toxin dates within ±2 days
# ----------------------------------------------------------------------
#
# Matching occurs separately for each lake and toxin method.
#
# If no toxin sample occurred within ±2 days, the phytoplankton event
# will not appear in phyto_toxin_matches. It will be restored as an
# unmatched event when the left join is performed later.
# ----------------------------------------------------------------------

maximum_date_difference <- 2


phyto_toxin_matches <- phyto_events %>%
  rename(
    phyto_date = date
  ) %>%
  inner_join(
    toxin_surface,
    by = "lake",
    relationship = "many-to-many"
  ) %>%
  mutate(
    
    signed_date_difference_days = as.numeric(
      toxin_date - phyto_date
    ),
    
    date_difference_days = abs(
      signed_date_difference_days
    )
    
  ) %>%
  filter(
    date_difference_days <= maximum_date_difference
  ) %>%
  group_by(
    lake,
    phyto_date,
    method
  ) %>%
  arrange(
    date_difference_days,
    toxin_date,
    .by_group = TRUE
  ) %>%
  slice_head(
    n = 1
  ) %>%
  ungroup() %>%
  arrange(
    lake,
    phyto_date,
    method
  )


# Inspect matched dates
print(
  phyto_toxin_matches %>%
    select(
      lake,
      phyto_date,
      toxin_date,
      signed_date_difference_days,
      date_difference_days,
      method,
      total_mc_converted,
      sample_status
    ),
  n = Inf
)


# Confirm no duplicate matches
match_duplicate_check <- phyto_toxin_matches %>%
  count(
    lake,
    phyto_date,
    method,
    name = "n"
  ) %>%
  filter(
    n > 1
  )


print(match_duplicate_check, n = Inf)


# Count matched sampling events
matched_event_counts <- phyto_toxin_matches %>%
  count(
    lake,
    method,
    name = "matched_events"
  )


print(matched_event_counts, n = Inf)


# ----------------------------------------------------------------------
# 8. Join cyanobacteria to matched toxin data
# ----------------------------------------------------------------------
#
# The left join retains every phytoplankton event.
#
# For June 24 and other phytoplankton-only dates:
#   toxin_date              = NA
#   method                  = NA
#   total_mc_converted      = NA
#
# These events remain available for community summaries but are later
# excluded from paired toxin correlations.
# ----------------------------------------------------------------------

cyano_toxin_long <- cyano_complete %>%
  rename(
    phyto_date = date
  ) %>%
  left_join(
    phyto_toxin_matches,
    by = c(
      "lake",
      "phyto_date"
    ),
    relationship = "many-to-many"
  ) %>%
  arrange(
    lake,
    phyto_date,
    method,
    taxon
  )


# Inspect the joined dataset
print(
  cyano_toxin_long %>%
    select(
      lake,
      phyto_date,
      toxin_date,
      date_difference_days,
      method,
      taxon,
      taxon_cells_l,
      total_cyano_cells_l,
      taxon_percent,
      total_mc_converted
    ) %>%
    arrange(
      lake,
      phyto_date,
      method,
      desc(taxon_cells_l)
    ),
  n = 100
)


# Confirm only one row per final analysis unit
cyano_toxin_duplicate_check <- cyano_toxin_long %>%
  filter(
    !is.na(method)
  ) %>%
  count(
    lake,
    phyto_date,
    method,
    taxon,
    name = "n"
  ) %>%
  filter(
    n > 1
  )


print(cyano_toxin_duplicate_check, n = Inf)


# ----------------------------------------------------------------------
# 9. Identify phytoplankton events without a toxin match
# ----------------------------------------------------------------------
#
# Use toxin_date rather than total_mc_converted.
#
# An NA toxin concentration could represent an analyzed sample with
# missing results. An NA toxin_date clearly means no date was matched.
# ----------------------------------------------------------------------

unmatched_phyto_events <- cyano_toxin_long %>%
  filter(
    is.na(toxin_date)
  ) %>%
  distinct(
    lake,
    phyto_date
  ) %>%
  arrange(
    lake,
    phyto_date
  )


print(unmatched_phyto_events, n = Inf)





# ----------------------------------------------------------------------
# 10. Create paired analysis dataset
# ----------------------------------------------------------------------
#
# This dataset contains only observations with:
#   - a matched toxin date,
#   - grab or SPATT method,
#   - a nonmissing toxin concentration.
#
# June 24 is therefore excluded from correlations but remains in
# cyano_toxin_long and cyano_complete.
# ----------------------------------------------------------------------

cyano_toxin_analysis <- cyano_toxin_long %>%
  filter(
    !is.na(toxin_date),
    method %in% c(
      "grab",
      "spatt"
    ),
    !is.na(total_mc_converted)
  ) %>%
  select(
    lake,
    phyto_date,
    toxin_date,
    signed_date_difference_days,
    date_difference_days,
    method,
    taxon,
    taxon_cells_l,
    total_cyano_cells_l,
    taxon_relative_abundance,
    taxon_percent,
    cyano_present,
    total_mc_converted,
    sample_status
  ) %>%
  distinct() %>%
  arrange(
    lake,
    method,
    phyto_date,
    taxon
  )


# Count the paired observations available for correlations
paired_event_counts <- cyano_toxin_analysis %>%
  distinct(
    lake,
    phyto_date,
    method
  ) %>%
  count(
    lake,
    method,
    name = "paired_events"
  )


print(paired_event_counts, n = Inf)


# ----------------------------------------------------------------------
# 11. Full-season taxon occurrence
# ----------------------------------------------------------------------
#
# Includes all phytoplankton events, including June 24.
#
# Use this table when describing community occurrence over the entire
# phytoplankton sampling season.
# ----------------------------------------------------------------------

taxon_occurrence_full_season <- cyano_complete %>%
  group_by(
    lake,
    taxon
  ) %>%
  summarise(
    
    events_present = n_distinct(
      date[taxon_cells_l > 0]
    ),
    
    total_phyto_events = n_distinct(date),
    
    occurrence_percent = 100 *
      events_present /
      total_phyto_events,
    
    .groups = "drop"
    
  ) %>%
  arrange(
    lake,
    desc(events_present),
    taxon
  )


print(taxon_occurrence_full_season, n = Inf)


# ----------------------------------------------------------------------
# 12. Matched-event taxon occurrence
# ----------------------------------------------------------------------
#
# Includes only phytoplankton events paired with toxin observations.
#
# This table is separated by toxin method because grab and SPATT may
# have different numbers of successfully matched events.
#
# Use this table to determine whether a within-lake correlation has
# enough taxon-presence events to interpret.
# ----------------------------------------------------------------------

taxon_occurrence_matched <- cyano_toxin_analysis %>%
  group_by(
    lake,
    method,
    taxon
  ) %>%
  summarise(
    
    matched_events_present = n_distinct(
      phyto_date[taxon_cells_l > 0]
    ),
    
    total_matched_events = n_distinct(
      phyto_date
    ),
    
    matched_occurrence_percent = 100 *
      matched_events_present /
      total_matched_events,
    
    .groups = "drop"
    
  ) %>%
  arrange(
    lake,
    method,
    desc(matched_events_present),
    taxon
  )


print(taxon_occurrence_matched, n = Inf)


# ----------------------------------------------------------------------
# 13. Full-season pooled taxon occurrence
# ----------------------------------------------------------------------
#
# Includes all lakes and all phytoplankton sampling dates.
# ----------------------------------------------------------------------

taxon_occurrence_pooled_full_season <- cyano_complete %>%
  mutate(
    lake_date_id = paste(
      lake,
      date,
      sep = "_"
    )
  ) %>%
  group_by(taxon) %>%
  summarise(
    
    events_present = n_distinct(
      lake_date_id[taxon_cells_l > 0]
    ),
    
    total_phyto_events = n_distinct(
      lake_date_id
    ),
    
    lakes_present = n_distinct(
      lake[taxon_cells_l > 0]
    ),
    
    occurrence_percent = 100 *
      events_present /
      total_phyto_events,
    
    .groups = "drop"
    
  ) %>%
  arrange(
    desc(events_present),
    taxon
  )


print(taxon_occurrence_pooled_full_season, n = Inf)


# ----------------------------------------------------------------------
# 14. Pooled occurrence among matched events
# ----------------------------------------------------------------------

taxon_occurrence_pooled_matched <- cyano_toxin_analysis %>%
  mutate(
    lake_date_id = paste(
      lake,
      phyto_date,
      sep = "_"
    )
  ) %>%
  group_by(
    method,
    taxon
  ) %>%
  summarise(
    
    matched_events_present = n_distinct(
      lake_date_id[taxon_cells_l > 0]
    ),
    
    total_matched_events = n_distinct(
      lake_date_id
    ),
    
    lakes_present = n_distinct(
      lake[taxon_cells_l > 0]
    ),
    
    matched_occurrence_percent = 100 *
      matched_events_present /
      total_matched_events,
    
    .groups = "drop"
    
  ) %>%
  arrange(
    method,
    desc(matched_events_present),
    taxon
  )


print(taxon_occurrence_pooled_matched, n = Inf)


# ----------------------------------------------------------------------
# 15. Safe Spearman correlation function
# ----------------------------------------------------------------------

safe_spearman <- function(
    x,
    y,
    minimum_n = 4
) {
  
  complete_rows <- complete.cases(
    x,
    y
  )
  
  x_complete <- x[complete_rows]
  y_complete <- y[complete_rows]
  
  n_complete <- length(x_complete)
  
  predictor_nonzero_n <- sum(
    x_complete > 0,
    na.rm = TRUE
  )
  
  predictor_unique_n <- n_distinct(
    x_complete
  )
  
  response_unique_n <- n_distinct(
    y_complete
  )
  
  
  if (n_complete < minimum_n) {
    
    return(
      tibble(
        n = n_complete,
        predictor_nonzero_n = predictor_nonzero_n,
        predictor_unique_n = predictor_unique_n,
        response_unique_n = response_unique_n,
        rho = NA_real_,
        p_value = NA_real_,
        result_status = "Too few paired observations"
      )
    )
    
  }
  
  
  if (
    predictor_unique_n < 2 ||
    response_unique_n < 2
  ) {
    
    return(
      tibble(
        n = n_complete,
        predictor_nonzero_n = predictor_nonzero_n,
        predictor_unique_n = predictor_unique_n,
        response_unique_n = response_unique_n,
        rho = NA_real_,
        p_value = NA_real_,
        result_status = "No variation"
      )
    )
    
  }
  
  
  correlation_test <- suppressWarnings(
    cor.test(
      x_complete,
      y_complete,
      method = "spearman",
      exact = FALSE
    )
  )
  
  
  tibble(
    n = n_complete,
    predictor_nonzero_n = predictor_nonzero_n,
    predictor_unique_n = predictor_unique_n,
    response_unique_n = response_unique_n,
    rho = unname(
      correlation_test$estimate
    ),
    p_value = correlation_test$p.value,
    result_status = "Calculated"
  )
  
}


# ----------------------------------------------------------------------
# 16. Analysis thresholds
# ----------------------------------------------------------------------

minimum_paired_events <- 4

minimum_presence_events <- 3


# ======================================================================
# POOLED CORRELATIONS ACROSS ALL LAKES
# ======================================================================


# ----------------------------------------------------------------------
# 17. Pooled absolute-abundance correlations
# ----------------------------------------------------------------------

pooled_absolute_results <- cyano_toxin_analysis %>%
  group_by(
    method,
    taxon
  ) %>%
  group_modify(
    ~ safe_spearman(
      x = .x$taxon_cells_l,
      y = .x$total_mc_converted,
      minimum_n = minimum_paired_events
    )
  ) %>%
  ungroup() %>%
  mutate(
    abundance_measure = "Absolute abundance"
  )


# ----------------------------------------------------------------------
# 18. Pooled relative-abundance correlations
# ----------------------------------------------------------------------

pooled_relative_results <- cyano_toxin_analysis %>%
  group_by(
    method,
    taxon
  ) %>%
  group_modify(
    ~ safe_spearman(
      x = .x$taxon_percent,
      y = .x$total_mc_converted,
      minimum_n = minimum_paired_events
    )
  ) %>%
  ungroup() %>%
  mutate(
    abundance_measure = "Relative abundance"
  )


# ----------------------------------------------------------------------
# 19. Combine pooled results
# ----------------------------------------------------------------------

pooled_correlation_results <- bind_rows(
  pooled_absolute_results,
  pooled_relative_results
) %>%
  group_by(
    method,
    abundance_measure
  ) %>%
  mutate(
    p_adjusted_bh = p.adjust(
      p_value,
      method = "BH"
    )
  ) %>%
  ungroup() %>%
  mutate(
    
    significance = case_when(
      is.na(p_adjusted_bh) ~ NA_character_,
      p_adjusted_bh < 0.001 ~ "***",
      p_adjusted_bh < 0.01 ~ "**",
      p_adjusted_bh < 0.05 ~ "*",
      TRUE ~ "ns"
    ),
    
    interpretation_status = case_when(
      result_status != "Calculated" ~ result_status,
      
      predictor_nonzero_n < minimum_presence_events ~
        "Taxon too infrequent for interpretation",
      
      TRUE ~
        "Retain for interpretation"
    )
    
  ) %>%
  select(
    method,
    taxon,
    abundance_measure,
    n,
    predictor_nonzero_n,
    predictor_unique_n,
    response_unique_n,
    rho,
    p_value,
    p_adjusted_bh,
    significance,
    result_status,
    interpretation_status
  ) %>%
  arrange(
    method,
    abundance_measure,
    desc(abs(rho))
  )


print(pooled_correlation_results, n = Inf)


pooled_results_retained <- pooled_correlation_results %>%
  filter(
    interpretation_status ==
      "Retain for interpretation"
  )


print(pooled_results_retained, n = Inf)


# ======================================================================
# WITHIN-LAKE CORRELATIONS
# ======================================================================


# ----------------------------------------------------------------------
# 20. Within-lake absolute-abundance correlations
# ----------------------------------------------------------------------

within_lake_absolute_results <- cyano_toxin_analysis %>%
  group_by(
    lake,
    method,
    taxon
  ) %>%
  group_modify(
    ~ safe_spearman(
      x = .x$taxon_cells_l,
      y = .x$total_mc_converted,
      minimum_n = minimum_paired_events
    )
  ) %>%
  ungroup() %>%
  mutate(
    abundance_measure = "Absolute abundance"
  )


# ----------------------------------------------------------------------
# 21. Within-lake relative-abundance correlations
# ----------------------------------------------------------------------

within_lake_relative_results <- cyano_toxin_analysis %>%
  group_by(
    lake,
    method,
    taxon
  ) %>%
  group_modify(
    ~ safe_spearman(
      x = .x$taxon_percent,
      y = .x$total_mc_converted,
      minimum_n = minimum_paired_events
    )
  ) %>%
  ungroup() %>%
  mutate(
    abundance_measure = "Relative abundance"
  )


# ----------------------------------------------------------------------
# 22. Combine within-lake results
# ----------------------------------------------------------------------

within_lake_correlation_results <- bind_rows(
  within_lake_absolute_results,
  within_lake_relative_results
) %>%
  mutate(
    
    interpretation_status = case_when(
      
      result_status != "Calculated" ~
        result_status,
      
      predictor_nonzero_n < minimum_presence_events ~
        "Taxon too infrequent for interpretation",
      
      TRUE ~
        "Retain for interpretation"
      
    )
    
  ) %>%
  group_by(
    lake,
    method,
    abundance_measure
  ) %>%
  mutate(
    p_adjusted_bh = p.adjust(
      p_value,
      method = "BH"
    )
  ) %>%
  ungroup() %>%
  mutate(
    
    significance = case_when(
      is.na(p_adjusted_bh) ~ NA_character_,
      p_adjusted_bh < 0.001 ~ "***",
      p_adjusted_bh < 0.01 ~ "**",
      p_adjusted_bh < 0.05 ~ "*",
      TRUE ~ "ns"
    )
    
  ) %>%
  select(
    lake,
    method,
    taxon,
    abundance_measure,
    n,
    predictor_nonzero_n,
    predictor_unique_n,
    response_unique_n,
    rho,
    p_value,
    p_adjusted_bh,
    significance,
    result_status,
    interpretation_status
  ) %>%
  arrange(
    lake,
    method,
    abundance_measure,
    desc(abs(rho))
  )


print(within_lake_correlation_results, n = Inf)


# Retain interpretable within-lake results
within_lake_results_retained <- within_lake_correlation_results %>%
  filter(
    interpretation_status ==
      "Retain for interpretation"
  )


print(within_lake_results_retained, n = Inf)


# Results excluded due to sparse taxon occurrence
within_lake_results_infrequent <- within_lake_correlation_results %>%
  filter(
    interpretation_status ==
      "Taxon too infrequent for interpretation"
  )


print(within_lake_results_infrequent, n = Inf)


# ======================================================================
# COMPARISON TABLES
# ======================================================================


# ----------------------------------------------------------------------
# 23. Compare pooled absolute and relative correlations
# ----------------------------------------------------------------------

pooled_comparison <- pooled_correlation_results %>%
  select(
    method,
    taxon,
    abundance_measure,
    n,
    predictor_nonzero_n,
    rho,
    p_value,
    p_adjusted_bh,
    interpretation_status
  ) %>%
  pivot_wider(
    names_from = abundance_measure,
    values_from = c(
      n,
      predictor_nonzero_n,
      rho,
      p_value,
      p_adjusted_bh,
      interpretation_status
    ),
    names_glue = "{.value}_{abundance_measure}"
  ) %>%
  arrange(
    method,
    taxon
  )


print(pooled_comparison, n = Inf)


# ----------------------------------------------------------------------
# 24. Compare within-lake absolute and relative correlations
# ----------------------------------------------------------------------

within_lake_comparison <- within_lake_results_retained %>%
  select(
    lake,
    method,
    taxon,
    abundance_measure,
    n,
    predictor_nonzero_n,
    rho,
    p_value,
    p_adjusted_bh
  ) %>%
  pivot_wider(
    names_from = abundance_measure,
    values_from = c(
      n,
      predictor_nonzero_n,
      rho,
      p_value,
      p_adjusted_bh
    ),
    names_glue = "{.value}_{abundance_measure}"
  ) %>%
  arrange(
    lake,
    method,
    taxon
  )


print(within_lake_comparison, n = Inf)


# ======================================================================
# PLOTS
# ======================================================================


# ----------------------------------------------------------------------
# 25. Taxa retained for pooled plots
# ----------------------------------------------------------------------
#
# Use occurrence among toxin-matched observations rather than all
# phytoplankton dates.
# ----------------------------------------------------------------------

taxa_for_pooled_plots <- taxon_occurrence_pooled_matched %>%
  filter(
    matched_events_present >= minimum_presence_events
  ) %>%
  distinct(taxon) %>%
  pull(taxon)


print(taxa_for_pooled_plots)


# ----------------------------------------------------------------------
# 26. Pooled absolute-abundance plot
# ----------------------------------------------------------------------

plot_pooled_absolute <- cyano_toxin_analysis %>%
  filter(
    taxon %in% taxa_for_pooled_plots
  ) %>%
  ggplot(
    aes(
      x = log10(taxon_cells_l + 1),
      y = log10(total_mc_converted + 1),
      color = lake
    )
  ) +
  geom_point(
    size = 2.4,
    alpha = 0.8
  ) +
  geom_smooth(
    aes(group = 1),
    method = "lm",
    se = FALSE,
    color = "black",
    linewidth = 0.7
  ) +
  facet_grid(
    method ~ taxon,
    scales = "free_x"
  ) +
  labs(
    x = expression(
      log[10] * "(taxon cells L"^{-1} * " + 1)"
    ),
    y = expression(
      log[10] * "(total microcystin + 1)"
    ),
    color = "Lake"
  ) +
  theme_bw(
    base_size = 11,
    base_family = "Helvetica"
  ) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(
      face = "bold"
    ),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )


plot_pooled_absolute


# ----------------------------------------------------------------------
# 27. Pooled relative-abundance plot
# ----------------------------------------------------------------------

plot_pooled_relative <- cyano_toxin_analysis %>%
  filter(
    taxon %in% taxa_for_pooled_plots,
    !is.na(taxon_percent)
  ) %>%
  ggplot(
    aes(
      x = taxon_percent,
      y = log10(total_mc_converted + 1),
      color = lake
    )
  ) +
  geom_point(
    size = 2.4,
    alpha = 0.8
  ) +
  geom_smooth(
    aes(group = 1),
    method = "lm",
    se = FALSE,
    color = "black",
    linewidth = 0.7
  ) +
  facet_grid(
    method ~ taxon,
    scales = "free_x"
  ) +
  labs(
    x = "Relative abundance within cyanobacteria (%)",
    y = expression(
      log[10] * "(total microcystin + 1)"
    ),
    color = "Lake"
  ) +
  theme_bw(
    base_size = 11,
    base_family = "Helvetica"
  ) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(
      face = "bold"
    ),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )


plot_pooled_relative


# ----------------------------------------------------------------------
# 28. Within-lake absolute-abundance plot
# ----------------------------------------------------------------------

within_lake_absolute_taxa_to_plot <- within_lake_results_retained %>%
  filter(
    abundance_measure == "Absolute abundance"
  ) %>%
  distinct(
    lake,
    method,
    taxon
  )


plot_data_within_absolute <- cyano_toxin_analysis %>%
  inner_join(
    within_lake_absolute_taxa_to_plot,
    by = c(
      "lake",
      "method",
      "taxon"
    ),
    relationship = "many-to-one"
  )


plot_within_absolute <- plot_data_within_absolute %>%
  ggplot(
    aes(
      x = log10(taxon_cells_l + 1),
      y = log10(total_mc_converted + 1)
    )
  ) +
  geom_point(
    size = 2.3,
    alpha = 0.8
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    linewidth = 0.7
  ) +
  facet_grid(
    lake + method ~ taxon,
    scales = "free"
  ) +
  labs(
    x = expression(
      log[10] * "(taxon cells L"^{-1} * " + 1)"
    ),
    y = expression(
      log[10] * "(total microcystin + 1)"
    )
  ) +
  theme_bw(
    base_size = 10,
    base_family = "Helvetica"
  ) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(
      face = "bold"
    ),
    panel.grid.minor = element_blank()
  )


plot_within_absolute


# ----------------------------------------------------------------------
# 29. Within-lake relative-abundance plot
# ----------------------------------------------------------------------

within_lake_relative_taxa_to_plot <- within_lake_results_retained %>%
  filter(
    abundance_measure == "Relative abundance"
  ) %>%
  distinct(
    lake,
    method,
    taxon
  )


plot_data_within_relative <- cyano_toxin_analysis %>%
  filter(
    !is.na(taxon_percent)
  ) %>%
  inner_join(
    within_lake_relative_taxa_to_plot,
    by = c(
      "lake",
      "method",
      "taxon"
    ),
    relationship = "many-to-one"
  )


plot_within_relative <- plot_data_within_relative %>%
  ggplot(
    aes(
      x = taxon_percent,
      y = log10(total_mc_converted + 1)
    )
  ) +
  geom_point(
    size = 2.3,
    alpha = 0.8
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    linewidth = 0.7
  ) +
  facet_grid(
    lake + method ~ taxon,
    scales = "free"
  ) +
  labs(
    x = "Relative abundance within cyanobacteria (%)",
    y = expression(
      log[10] * "(total microcystin + 1)"
    )
  ) +
  theme_bw(
    base_size = 10,
    base_family = "Helvetica"
  ) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(
      face = "bold"
    ),
    panel.grid.minor = element_blank()
  )


plot_within_relative


print(pooled_correlation_results, n = Inf, width = Inf)
print(within_lake_results_retained, n = Inf,width = Inf)
print(taxon_occurrence_full_season, n = Inf)
print(taxon_occurrence_matched, n = Inf)
print(paired_event_counts, n = Inf)
print(pooled_corre_comparison, n = Inf)
# ==================================================================
====
# SAVE RESULTS
# ======================================================================


# ----------------------------------------------------------------------
# 30. Output directories
# ----------------------------------------------------------------------

results_dir <- paste0(
  "~/Desktop/Project/Brooks_lake_2025/results/",
  "cyanobacteria_toxin_correlations"
)

figure_dir <- paste0(
  "~/Desktop/Project/Brooks_lake_2025/figures/",
  "cyanobacteria_toxin_correlations"
)


dir.create(
  results_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  figure_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# ----------------------------------------------------------------------
# 31. Save occurrence and matching tables
# ----------------------------------------------------------------------

write_csv(
  unmatched_phyto_events,
  file.path(
    results_dir,
    "phytoplankton_events_without_toxin_matches.csv"
  )
)

write_csv(
  taxon_occurrence_full_season,
  file.path(
    results_dir,
    "taxon_occurrence_full_phytoplankton_season.csv"
  )
)

write_csv(
  taxon_occurrence_matched,
  file.path(
    results_dir,
    "taxon_occurrence_toxin_matched_events.csv"
  )
)

write_csv(
  taxon_occurrence_pooled_full_season,
  file.path(
    results_dir,
    "pooled_taxon_occurrence_full_phytoplankton_season.csv"
  )
)

write_csv(
  taxon_occurrence_pooled_matched,
  file.path(
    results_dir,
    "pooled_taxon_occurrence_toxin_matched_events.csv"
  )
)


# ----------------------------------------------------------------------
# 32. Save correlation tables
# ----------------------------------------------------------------------

write_csv(
  pooled_correlation_results,
  file.path(
    results_dir,
    "pooled_taxon_microcystin_correlations_all.csv"
  )
)

write_csv(
  pooled_results_retained,
  file.path(
    results_dir,
    "pooled_taxon_microcystin_correlations_retained.csv"
  )
)

write_csv(
  within_lake_correlation_results,
  file.path(
    results_dir,
    "within_lake_taxon_microcystin_correlations_all.csv"
  )
)

write_csv(
  within_lake_results_retained,
  file.path(
    results_dir,
    "within_lake_taxon_microcystin_correlations_retained.csv"
  )
)

write_csv(
  pooled_comparison,
  file.path(
    results_dir,
    "pooled_absolute_vs_relative_comparison.csv"
  )
)

write_csv(
  within_lake_comparison,
  file.path(
    results_dir,
    "within_lake_absolute_vs_relative_comparison.csv"
  )
)


# ----------------------------------------------------------------------
# 33. Save figures
# ----------------------------------------------------------------------

ggsave(
  filename = file.path(
    figure_dir,
    "pooled_absolute_abundance_vs_microcystin.png"
  ),
  plot = plot_pooled_absolute,
  width = 12,
  height = 6.5,
  dpi = 300
)

ggsave(
  filename = file.path(
    figure_dir,
    "pooled_relative_abundance_vs_microcystin.png"
  ),
  plot = plot_pooled_relative,
  width = 12,
  height = 6.5,
  dpi = 300
)

ggsave(
  filename = file.path(
    figure_dir,
    "within_lake_absolute_abundance_vs_microcystin.png"
  ),
  plot = plot_within_absolute,
  width = 14,
  height = 12,
  dpi = 300
)

ggsave(
  filename = file.path(
    figure_dir,
    "within_lake_relative_abundance_vs_microcystin.png"
  ),
  plot = plot_within_relative,
  width = 14,
  height = 12,
  dpi = 300
)