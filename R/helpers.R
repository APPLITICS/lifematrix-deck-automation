# ------ GLOBAL THEMES --------------------------------------------------------

#' Create a consistent visual theme for LIFE Matrix ggplots
#'
#' Applies dark background, white grid lines, bold axis labels and titles.
#' Can be used across bar charts, density plots, etc.
#'
#' @param base_size Base font size for the plot.
#' @param background_color Background color of the plot.
#' @param title_color Color for the plot title.
#' @param axis_color Color for axis lines and text.
#' @param grid_color Color for y-axis major grid lines.
#'
#' @return A ggplot2 theme object.
global_theme <- function(
    base_size = 14,
    background_color = "#005981",
    title_color = "white",
    axis_color = "white",
    grid_color = "white"
) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.background = element_rect(
        fill = background_color, color = NA
      ),
      plot.background = element_rect(
        fill = background_color, color = NA
      ),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(
        color = grid_color, linewidth = 0.2
      ),
      panel.grid.minor.y = element_blank(),
      axis.line.x = element_line(
        color = axis_color, linewidth = 1.2
      ),
      axis.line.y = element_line(
        color = axis_color, linewidth = 1.2
      ),
      axis.text.x = element_text(
        color = axis_color, size = 20, face = "bold",
        margin = margin(t = 10)
      ),
      axis.text.y = element_text(
        color = axis_color, size = 20, face = "bold",
        margin = margin(r = 10)
      ),
      axis.title.x = element_text(
        color = axis_color, size = 20, face = "bold",
        margin = margin(t = 25)
      ),
      axis.title.y = element_text(
        color = axis_color, size = 20, face = "bold",
        margin = margin(r = 25)
      ),
      plot.title = element_text(
        color = title_color, size = 25, face = "bold",
        hjust = 0.5, margin = margin(b = 20)
      )
    )
}

# ------ THEME: MINIMAL Y-LESS -------------------------------------------------

#' Minimal theme with y-axis removed
#'
#' A variation of theme_minimal with no y-axis labels, ticks, or titles.
#' Useful for minimalist bar plots or metric-only displays.
#'
#' @return A ggplot2 theme object.
theme_minimal_yless <- function() {
  theme_minimal(base_size = 25) +
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line.x = element_line(color = "white", linewidth = 0.8),
      axis.text.x = element_text(
        face = "bold", size = 14, color = "white",
        margin = margin(t = 10)
      ),
      axis.title.x = element_blank(),
      plot.background = element_rect(fill = "#005981", color = NA),
      panel.background = element_rect(fill = "#005981", color = NA),
      plot.margin = margin(t = 40, r = 40, b = 40, l = 40)
    )
}

# ------ COLOR PALETTE HELPER --------------------------------------------------

#' Generate a color palette for unique groups
#'
#' Assigns predefined colors to each unique group label.
#'
#' @param groups A character vector of group names.
#'
#' @return A named character vector of hex color codes.
get_color_palette <- function(groups) {
  base_colors <- c("#70e2ff", "#97e37e", "#2dc595", "#ffc101", "#cccccc")
  unique_groups <- unique(groups)
  colors <- setNames(base_colors[seq_along(unique_groups)],
                             unique_groups)
  return(colors)
}

# ------ EXPORT TO POWERPOINT --------------------------------------------------

#' Export a ggplot2 chart to a PowerPoint slide
#'
#' Adds a new slide, inserts the chart as a transparent vector graphic,
#' and places a formatted title at the top.
#'
#' @param ppt_doc A `read_pptx()` object representing the PowerPoint deck.
#' @param plot_obj A `ggplot2` object representing the chart to export.
#' @param title_text A string used as the slide title.
#' @param is_first Logical. If `TRUE`, edits the first slide instead of 
#'        adding a new one.
#' @param layout Slide layout name (default: "Title and Content").
#'
#' @return An updated `officer` PowerPoint object.
export_plot_to_slide <- function(
    ppt_doc,
    plot_obj,
    title_text = " ",
    is_first = FALSE,
    layout = "Title and Content"
) {
  if (!is.null(ppt_doc)) {
    if (is.null(is_first) || !is_first) {
      ppt_doc <- add_slide(ppt_doc, layout = layout)
    }
    
    dims <- slide_size(ppt_doc)
    
    ppt_doc <- ppt_doc %>%
      ph_with(
        value = block_list(
          fpar(ftext(
            title_text,
            fp_text(
              font.size = 28,
              bold = TRUE,
              font.family = "Arial",
              color = "#323233"
            )
          ))
        ),
        location = ph_location(
          left = 0.4,
          top = 0.2,
          width = dims$width - 1,
          height = 0.8
        )
      ) %>%
      ph_with(
        value = dml(ggobj = plot_obj, bg = "transparent"),
        location = ph_location(
          left = 0,
          top = 1.0,
          width = dims$width,
          height = dims$height - 1.4
        )
      )
  }
  
  return(ppt_doc)
}

# ------ STYLE ORDINAL SUFFIX -------------------------------------------------
#' Format ordinal numbers with superscript suffixes (e.g., 40th+ → 40<sup>th+</sup>)
#'
#' @param x A character or numeric vector of ordinal labels.
#' @return A character vector with HTML-styled ordinal suffixes.
style_ordinal_suffix <- function(x) {
  x <- as.character(x)
  
  x <- ifelse(
    grepl("^[0-9]+(st|nd|rd|th)(\\+?)$", x),
    sub(
      pattern = "^(\\d+)(st\\+|nd\\+|rd\\+|th\\+|st|nd|rd|th)$",
      replacement = "\\1<sup>\\2</sup>",
      x,
      perl = TRUE
    ),
    x
  )
  
  return(x)
}

# ------ NORMALIZE NAs ---------------------------------------------------------
#' Normalize NA-like values in a data frame.
#'
#' This helper replaces user-defined values with NA after loading. Unlike the
#' `na` parameter in `readr::read_csv()`, this function applies consistent NA 
#' cleaning across any data source (`fread()`, `read.csv()`, or in-memory 
#' data frames).
#'
#' @param data A data frame to clean.
#' @param na_equivalent Character vector of values to be converted to NA
#'
#' @return The input data frame with normalized missing values.
normalize_na_tbl <- function(
    data,
    na_equivalent
) {
  for (col in names(data)) {
    if (is.numeric(data[[col]])) {
      # Replace +/- Inf with NA for numeric columns
      data[[col]][is.infinite(data[[col]])] <- NA_real_
    } else if (is.character(data[[col]]) || is.factor(data[[col]])) {
      # Trim whitespace and normalize NA-like values (case-insensitive)
      x <- trimws(as.character(data[[col]]))
      x[tolower(x) %in% tolower(na_equivalent)] <- NA_character_
      data[[col]] <- x
    }
  }
  
  return(data)
}

# ------ SLIDE PREVIEW TITLE ---------------------------------------------------

#' Add slide-style title to a ggplot object
#'
#' Adds a formatted slide title above a ggplot object for display in Shiny
#' previews. The title is left-aligned and styled to match the PowerPoint theme.
#'
#' @param plot_obj A `ggplot` object to display below the title.
#' @param title_text A character string to use as the slide title. Defaults to
#'   an empty string if `NULL`.
#'
#' @return A grob object combining the title and plot arranged vertically.
add_slide_title <- function(plot_obj, title_text) {
  if (is.null(title_text)) title_text <- ""
  
  title_grob <- textGrob(
    title_text,
    gp = gpar(
      fontsize = 28,
      fontface = "bold",
      col = "#323233",
      fontfamily = "Arial"
    ),
    x = 0.02,
    hjust = 0
  )
  
  grid.arrange(
    title_grob,
    plot_obj,
    ncol = 1,
    heights = c(0.15, 1)
  )
}

# ------ SCALE PLOT THEME -----------------------------------------------------
#' Scale Plot Theme
#'
#' Applies a scaled theme to a ggplot object by adjusting text, axis, and legend
#' sizes relative to a given base size and scaling factor.
#'
#' @param plot A `ggplot` object to which the theme will be applied.
#' @param base_size Numeric. The base font size for text elements (default = 12).
#' @param factor Numeric. Scaling factor applied to the base size (default = 1.2).
#'
#' @return A `ggplot` object with the updated theme applied.
scale_plot_theme <- function(
    plot,
    base_size = 12,
    factor = 1.2
) {
  plot + theme(
    text        = element_text(size = base_size * factor),
    axis.text   = element_text(size = (base_size - 2) * factor),
    axis.title  = element_text(size = (base_size + 2) * factor),
    legend.text = element_text(size = (base_size - 2) * factor),
    legend.title= element_text(size = base_size * factor)
  )
}
