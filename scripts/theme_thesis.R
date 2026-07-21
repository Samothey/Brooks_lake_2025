# ======================================================================
# Thesis figure theme and publication-layout functions
# ======================================================================

library(tidyverse)
library(scales)
library(cowplot)


# ======================================================================
# Lake order
# ======================================================================

lake_order <- c(
  "Brooks Lake",
  "Lower Jade Lake",
  "Rainbow Lake",
  "Upper Brooks Lake"
)


# ======================================================================
# Standard spacing and dimensions
# ======================================================================

thesis_spacing <- list(
  plot_margin = 5,
  title_height = 0.10,
  panel_spacing = 0.10,
  legend_top_margin = 6,
  legend_bottom_margin = 3
)


thesis_panel_heights <- list(
  panel_large = 1.20,
  panel_medium = 1.10,
  panel_small = 0.80,
  
  # A one-row legend generally fits within this height.
  legend = 0.30,
  
  # Use this when a legend contains two rows.
  legend_two_row = 0.42,
  
  title = thesis_spacing$title_height,
  spacer = thesis_spacing$panel_spacing
)


thesis_figures <- list(
  width = 15,
  height = 10.5,
  height_four_panel = 15.5,
  dpi = 300
)


# ======================================================================
# Main thesis theme
# ======================================================================

theme_thesis <- function(
    base_size = 11,
    base_family = "Helvetica"
) {
  
  theme_bw(
    base_size = base_size,
    base_family = base_family
  ) +
    theme(
      axis.title = element_text(
        size = 10,
        color = "black"
      ),
      
      axis.text = element_text(
        size = 8.5,
        color = "black"
      ),
      
      axis.ticks = element_line(
        color = "black",
        linewidth = 0.35
      ),
      
      legend.title = element_blank(),
      
      legend.text = element_text(
        size = 8.5,
        color = "black"
      ),
      
      legend.position = "bottom",
      
      panel.grid.minor = element_blank(),
      
      panel.grid.major = element_line(
        linewidth = 0.25,
        color = "grey85"
      ),
      
      panel.border = element_rect(
        color = "black",
        fill = NA,
        linewidth = 0.5
      ),
      
      strip.background = element_rect(
        fill = "white",
        color = "black",
        linewidth = 0.5
      ),
      
      strip.text = element_text(
        face = "bold",
        size = 11,
        color = "black"
      ),
      
      plot.margin = margin(
        t = thesis_spacing$plot_margin,
        r = thesis_spacing$plot_margin,
        b = thesis_spacing$plot_margin,
        l = thesis_spacing$plot_margin
      )
    )
}


# ======================================================================
# Facet-strip theme
# ======================================================================

theme_lake_strip <- function(strip_size = 11) {
  
  theme(
    strip.background = element_rect(
      fill = "white",
      color = "black",
      linewidth = 0.5
    ),
    
    strip.text = element_text(
      face = "bold",
      size = strip_size,
      color = "black"
    )
  )
}


# ======================================================================
# Standard date scale
# ======================================================================

date_scale_thesis <- function(
    date_breaks = "1 month",
    date_labels = "%b"
) {
  
  scale_x_date(
    date_breaks = date_breaks,
    date_labels = date_labels,
    expand = expansion(
      mult = c(0.02, 0.02)
    )
  )
}


# ======================================================================
# Axis-removal helpers
# ======================================================================

remove_y_axis <- function() {
  
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )
}


remove_x_axis <- function() {
  
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )
}


theme_no_legend <- function() {
  
  theme(
    legend.position = "none"
  )
}


# ======================================================================
# Extract a horizontal legend
# ======================================================================

legend_row <- function(
    plot,
    aesthetic = c("fill", "color"),
    nrow = 1,
    top_margin = thesis_spacing$legend_top_margin,
    bottom_margin = thesis_spacing$legend_bottom_margin,
    key_width = 18,
    key_height = 11,
    spacing_x = 6
) {
  
  aesthetic <- match.arg(aesthetic)
  
  if (aesthetic == "fill") {
    
    plot <- plot +
      guides(
        fill = guide_legend(
          title = NULL,
          nrow = nrow,
          byrow = TRUE
        )
      )
  }
  
  if (aesthetic == "color") {
    
    plot <- plot +
      guides(
        color = guide_legend(
          title = NULL,
          nrow = nrow,
          byrow = TRUE
        )
      )
  }
  
  cowplot::get_legend(
    plot +
      theme(
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.box.just = "center",
        
        # Space between the plot and the legend.
        legend.margin = margin(
          t = top_margin,
          r = 0,
          b = bottom_margin,
          l = 0
        ),
        
        # Keep internal padding modest so the legend is not oversized.
        legend.box.margin = margin(
          t = 0,
          r = 0,
          b = 0,
          l = 0
        ),
        
        legend.spacing.x = grid::unit(
          spacing_x,
          "pt"
        ),
        
        legend.spacing.y = grid::unit(
          2,
          "pt"
        ),
        
        legend.key.width = grid::unit(
          key_width,
          "pt"
        ),
        
        legend.key.height = grid::unit(
          key_height,
          "pt"
        )
      )
  )
}


# Backward-compatible alias
make_legend <- legend_row


# ======================================================================
# Panel title
# ======================================================================

panel_title <- function(
    plot,
    label,
    title,
    title_size = 11,
    title_height = thesis_spacing$title_height
) {
  
  title_plot <- ggdraw() +
    draw_label(
      paste0(label, ". ", title),
      x = 0,
      y = 0.5,
      hjust = 0,
      vjust = 0.5,
      fontface = "bold",
      size = title_size
    )
  
  plot_grid(
    title_plot,
    plot,
    ncol = 1,
    rel_heights = c(
      title_height,
      1
    ),
    align = "v",
    axis = "lr"
  )
}


# ======================================================================
# Additional row helpers
# ======================================================================

four_lake_row <- function(plot_list) {
  
  plot_grid(
    plotlist = plot_list,
    nrow = 1,
    align = "hv",
    axis = "tblr"
  )
}


label_row <- function(
    row_plot,
    label,
    label_width = 0.035
) {
  
  plot_grid(
    ggdraw() +
      draw_label(
        label,
        fontface = "bold",
        size = 13
      ),
    
    row_plot,
    
    nrow = 1,
    
    rel_widths = c(
      label_width,
      1
    )
  )
}


# ======================================================================
# General stacked publication figure
# ======================================================================

stacked_publication_figure <- function(
    panels,
    legends = NULL,
    heights = NULL,
    panel_spacing = 0,
    add_spacers = FALSE,
    align = "v",
    axis = "lr"
) {
  
  if (is.null(legends)) {
    legends <- vector(
      mode = "list",
      length = length(panels)
    )
  }
  
  if (length(legends) < length(panels)) {
    legends <- c(
      legends,
      vector(
        mode = "list",
        length = length(panels) - length(legends)
      )
    )
  }
  
  pieces <- list()
  default_heights <- numeric()
  
  for (i in seq_along(panels)) {
    
    pieces <- append(
      pieces,
      list(panels[[i]])
    )
    
    default_heights <- c(
      default_heights,
      thesis_panel_heights$panel_medium
    )
    
    if (!is.null(legends[[i]])) {
      
      pieces <- append(
        pieces,
        list(legends[[i]])
      )
      
      default_heights <- c(
        default_heights,
        thesis_panel_heights$legend
      )
    }
    
    # Add a spacer after every panel except the final panel.
    if (
      add_spacers &&
      i < length(panels)
    ) {
      
      pieces <- append(
        pieces,
        list(NULL)
      )
      
      default_heights <- c(
        default_heights,
        panel_spacing
      )
    }
  }
  
  if (is.null(heights)) {
    
    heights <- default_heights
    
  } else if (length(heights) != length(pieces)) {
    
    stop(
      paste0(
        "`heights` contains ",
        length(heights),
        " values, but the layout contains ",
        length(pieces),
        " rows."
      )
    )
  }
  
  cowplot::plot_grid(
    plotlist = pieces,
    ncol = 1,
    rel_heights = heights,
    align = align,
    axis = axis
  )
}


# ======================================================================
# Three-row publication panel
# ======================================================================

publication_panel_3row <- function(
    row_A,
    row_B,
    row_C,
    legend_A = NULL,
    legend_B = NULL,
    legend_C = NULL,
    titles = c(
      "Panel A",
      "Panel B",
      "Panel C"
    ),
    heights = NULL,
    panel_spacing = thesis_spacing$panel_spacing
) {
  
  panels <- list(
    panel_title(
      row_A,
      "A",
      titles[1]
    ),
    
    panel_title(
      row_B,
      "B",
      titles[2]
    ),
    
    panel_title(
      row_C,
      "C",
      titles[3]
    )
  )
  
  legends <- list(
    legend_A,
    legend_B,
    legend_C
  )
  
  stacked_publication_figure(
    panels = panels,
    legends = legends,
    heights = heights,
    panel_spacing = panel_spacing,
    add_spacers = TRUE,
    align = "v",
    axis = "lr"
  )
}


# ======================================================================
# Four-row publication panel
# ======================================================================

publication_panel_4row <- function(
    row_A,
    row_B,
    row_C,
    row_D,
    legend_A = NULL,
    legend_B = NULL,
    legend_C = NULL,
    legend_D = NULL,
    titles = c(
      "Panel A",
      "Panel B",
      "Panel C",
      "Panel D"
    ),
    heights = NULL,
    panel_spacing = thesis_spacing$panel_spacing
) {
  
  panels <- list(
    panel_title(
      row_A,
      "A",
      titles[1]
    ),
    
    panel_title(
      row_B,
      "B",
      titles[2]
    ),
    
    panel_title(
      row_C,
      "C",
      titles[3]
    ),
    
    panel_title(
      row_D,
      "D",
      titles[4]
    )
  )
  
  legends <- list(
    legend_A,
    legend_B,
    legend_C,
    legend_D
  )
  
  stacked_publication_figure(
    panels = panels,
    legends = legends,
    heights = heights,
    panel_spacing = panel_spacing,
    add_spacers = TRUE,
    align = "v",
    axis = "lr"
  )
}


# ======================================================================
# Save thesis figure
# ======================================================================

save_thesis_fig <- function(
    plot,
    filename,
    width = thesis_figures$width,
    height = thesis_figures$height,
    dpi = thesis_figures$dpi,
    units = "in",
    background = "white"
) {
  
  ggsave(
    filename = filename,
    plot = plot,
    width = width,
    height = height,
    dpi = dpi,
    units = units,
    bg = background
  )
}