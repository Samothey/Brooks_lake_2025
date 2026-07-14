library(tidyverse)
library(scales)
library(cowplot)

lake_order <- c(
  "Brooks Lake",
  "Lower Jade Lake",
  "Rainbow Lake",
  "Upper Brooks Lake"
)

# ======================================================================
# Standard publication layout heights
# ======================================================================

thesis_panel_heights <- list(
  
  panel_large = 1.20,
  panel_medium = 1.10,
  panel_small = 0.80,
  
  legend = 0.22,
  
  title = 0.08
  
)
###
# ======================================================================
# Standard publication figure sizes
# ======================================================================

thesis_figures <- list(
  
  width = 15,
  height = 10.5,
  
  dpi = 300
  
)
#########
theme_thesis <- function(base_size = 11, base_family = "Helvetica") {
  theme_bw(base_size = base_size, base_family = base_family) +
    theme(
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 8.5, color = "black"),
      legend.title = element_blank(),
      legend.text = element_text(size = 8.5),
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(linewidth = 0.25),
      panel.border = element_rect(color = "black", fill = NA),
      plot.margin = margin(4, 4, 4, 4)
    )
}

theme_lake_strip <- function(strip_size = 11) {
  theme(
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(face = "bold", size = strip_size)
  )
}

date_scale_thesis <- function() {
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b",
    expand = expansion(mult = c(0.02, 0.02))
  )
}

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
  theme(legend.position = "none")
}

legend_row <- function(plot, aesthetic = "fill", nrow = 1) {
  
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
  
  get_legend(
    plot +
      theme(
        legend.position = "bottom",
        
        # space ABOVE the legend
        legend.margin = margin(
          t = 14,
          r = 0,
          b = 8,
          l = 0
        ),
        
        # padding INSIDE the legend box
        legend.box.margin = margin(
          t = 12,
          r = 0,
          b = 12,
          l = 0
        ),
        
        # spacing between legend keys
        legend.spacing.x = unit(6, "pt"),
        legend.key.width = unit(18, "pt")
      )
  )
}

make_legend <- legend_row

panel_title <- function(plot, label, title, title_size = 11) {
  title_plot <- ggdraw() +
    draw_label(
      paste0(label, ". ", title),
      x = 0,
      hjust = 0,
      fontface = "bold",
      size = title_size
    )
  
  plot_grid(
    title_plot,
    plot,
    ncol = 1,
    rel_heights = c(0.08, 1)
  )
}

four_lake_row <- function(plot_list) {
  plot_grid(
    plotlist = plot_list,
    nrow = 1,
    align = "hv",
    axis = "tblr"
  )
}

label_row <- function(row_plot, label) {
  plot_grid(
    ggdraw() + draw_label(label, fontface = "bold", size = 13),
    row_plot,
    nrow = 1,
    rel_widths = c(0.035, 1)
  )
}

stacked_publication_figure <- function(
    panels,
    legends = NULL,
    heights = NULL
) {
  pieces <- list()
  
  for (i in seq_along(panels)) {
    pieces <- c(pieces, list(panels[[i]]))
    
    if (!is.null(legends) && length(legends) >= i && !is.null(legends[[i]])) {
      pieces <- c(pieces, list(legends[[i]]))
    }
  }
  
  if (is.null(heights)) {
    heights <- rep(1, length(pieces))
  }
  
  plot_grid(
    plotlist = pieces,
    ncol = 1,
    rel_heights = heights
  )
}

publication_panel_3row <- function(
    row_A,
    row_B,
    row_C,
    legend_A = NULL,
    legend_B = NULL,
    legend_C = NULL,
    titles = c("Panel A", "Panel B", "Panel C"),
    heights = c(1.2, 0.18, 1.1, 0.18, 0.8, 0.18)
) {
  stacked_publication_figure(
    panels = list(
      panel_title(row_A, "A", titles[1]),
      panel_title(row_B, "B", titles[2]),
      panel_title(row_C, "C", titles[3])
    ),
    legends = list(
      legend_A,
      legend_B,
      legend_C
    ),
    heights = heights
  )
}

save_thesis_fig <- function(plot, filename, width = 15, height = 10.5) {
  ggsave(
    filename = filename,
    plot = plot,
    width = width,
    height = height,
    dpi = 300
  )
}


