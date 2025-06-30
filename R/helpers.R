#' Defines a reusable ggplot2 theme for LIFE Matrix charts.
#'
#' This function builds a consistent visual theme for LIFE Matrix plots, supporting:
#' - Dark blue background with optional overrides
#' - Bold and clearly visible axis text and lines
#' - Customizable title, grid, and axis color settings
#' - Compatible with bar and density charts in LIFE Matrix decks

global_theme <- function(
    base_size        = 14,
    background_color = "#004E73",
    title_color      = "white",
    axis_color       = "white",
    grid_color       = "white"
) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.background   = element_rect(
        fill  = background_color,
        color = NA
      ),
      plot.background    = element_rect(
        fill  = background_color,
        color = NA
      ),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(
        color = grid_color,
        size  = 0.2
      ),
      panel.grid.minor.y = element_blank(),
      axis.line.x        = element_line(
        color = axis_color,
        size  = 1.2
      ),
      axis.line.y        = element_line(
        color = axis_color,
        size  = 1.2
      ),
      axis.text.x        = element_text(
        color = axis_color,
        size  = 14,
        face  = "bold"
      ),
      axis.text.y        = element_text(
        color = axis_color,
        size  = 12,
        face  = "bold"
      ),
      axis.title.x       = element_text(
        color  = axis_color,
        size   = 14,
        face   = "bold",
        margin = margin(t = 10)
      ),
      axis.title.y       = element_text(
        color  = axis_color,
        size   = 14,
        face   = "bold",
        margin = margin(r = 15)
      ),
      plot.title         = element_text(
        color  = title_color,
        size   = 16,
        face   = "bold",
        hjust  = 0.5,
        margin = margin(b = 15)
      )
    )
}