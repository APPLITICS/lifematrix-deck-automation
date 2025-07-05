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
    base_size        = 14,
    background_color = "#005981",
    title_color      = "white",
    axis_color       = "white",
    grid_color       = "white"
) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.background    = element_rect(fill = background_color, color = NA),
      plot.background     = element_rect(fill = background_color, color = NA),
      panel.grid.major.x  = element_blank(),
      panel.grid.minor.x  = element_blank(),
      panel.grid.major.y  = element_line(color = grid_color, size = 0.2),
      panel.grid.minor.y  = element_blank(),
      axis.line.x         = element_line(color = axis_color, size = 1.2),
      axis.line.y         = element_line(color = axis_color, size = 1.2),
      axis.text.x         = element_text(
        color  = axis_color,
        size   = 20,
        face   = "bold",
        margin = margin(t = 10)
      ),
      axis.text.y         = element_text(
        color  = axis_color,
        size   = 20,
        face   = "bold",
        margin = margin(r = 10)
      ),
      axis.title.x        = element_text(
        color  = axis_color,
        size   = 20,
        face   = "bold",
        margin = margin(t = 25)
      ),
      axis.title.y        = element_text(
        color  = axis_color,
        size   = 20,
        face   = "bold",
        margin = margin(r = 25)
      ),
      plot.title          = element_text(
        color  = title_color,
        size   = 25,
        face   = "bold",
        hjust  = 0.5,
        margin = margin(b = 20)
      )
    )
}

#' Minimal theme with y-axis removed
#'
#' A variation of theme_minimal with no y-axis labels, ticks, or titles.
#' Useful for minimalist bar plots or metric-only displays.
#'
#' @return A ggplot2 theme object.
theme_minimal_yless <- function() {
  theme_minimal(base_size = 25) +
    theme(
      axis.title.y     = element_blank(),
      axis.text.y      = element_blank(),
      axis.ticks.y     = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border     = element_blank(),
      axis.line.x      = element_line(color = "white", linewidth = 0.8),
      axis.text.x      = element_text(
        face   = "bold",
        size   = 14,
        color  = "white",
        margin = margin(t = 10)
      ),
      axis.title.x     = element_blank(),
      plot.background  = element_rect(fill = "#005981", color = NA),
      panel.background = element_rect(fill = "#005981", color = NA),
      plot.margin      = margin(t = 40, r = 40, b = 40, l = 40)
    )
}

# ------ UNIT HELPERS ---------------------------------------------------------

#' Return scaling, labeling, and offset functions for a given unit
#'
#' Used to dynamically scale and format numeric values depending on unit type
#' such as percentage or raw measure.
#'
#' @param unit Unit type, one of "%" or "measure".
#'
#' @return A list of scale, label, target, and offset functions.
get_unit_helper <- function(unit = "measure") {
  unit_helpers <- list(
    "%" = list(
      scale  = function(x) x * 10,
      label  = function(x) paste0("ifelse(is.na(", x, ") | ", x, " == 0, '', paste0(round(", x, "), '%'))"),
      target = function(x) paste0("paste0(round(", x, "), '%')"),
      offset = function() 6
    ),
    "measure" = list(
      scale  = function(x) x,
      label  = function(x) paste0("ifelse(is.na(", x, ") | ", x, " == 0, '', round(", x, ", 1))"),
      target = function(x) paste0("round(", x, ", 1)"),
      offset = function() 0.4
    )
  )
  return(unit_helpers[[unit %||% "measure"]])
}

# ------ CATEGORIZATION HELPERS -----------------------------------------------

#' Sort category labels in numeric, human-readable order
#'
#' Handles labels like "Less than 50K", "More than 1M", "250K–1M", etc.
#'
#' @param v Character vector of labels.
#'
#' @return Ordered character vector.
sort_category_labels <- function(v) {
  extract_custom_order <- function(label) {
    label <- trimws(label)
    if (grepl("^Less than\\s*\\d+", label, ignore.case = TRUE)) return(0)
    if (grepl("^More than\\s*\\d+", label, ignore.case = TRUE)) return(Inf)
    num <- suppressWarnings(as.numeric(sub("^([0-9]+).*", "\\1", label)))
    if (is.na(num)) {
      num <- suppressWarnings(as.numeric(stringr::str_extract(label, "\\d+\\.*\\d*")))
    }
    return(num)
  }
  order_vec <- sapply(v, extract_custom_order)
  v[order(order_vec, na.last = TRUE)]
}

#' Apply grouping labels to a numeric column in a data frame
#'
#' Uses human-readable labels like "Less than 50K", "250K–1M", "More than 1M"
#' to classify continuous variables into categories.
#'
#' @param df Data frame with raw numeric values.
#' @param cat_name Column name in df to group.
#' @param grouping_vec Character vector of human-readable category rules.
#'
#' @return Data frame with added column `category_grouped` as factor.
apply_category_grouping <- function(
    df,
    cat_name,
    grouping_vec
) {
  raw_vals <- df[[cat_name]]
  if (!is.numeric(raw_vals)) return(df)
  
  parse_grouping_rule <- function(label) {
    label <- trimws(label)
    parse_numeric_unit <- function(x) {
      x <- tolower(x)
      x <- gsub(",", "", x)
      x <- gsub("k", "e3", x)
      x <- gsub("m", "e6", x)
      as.numeric(x)
    }
    
    if (grepl("^Less than\\s+[0-9.,]+[KkMm]?$", label)) {
      threshold <- parse_numeric_unit(gsub("^Less than\\s+", "", label))
      return(function(x) ifelse(x < threshold, label, NA))
    }
    if (grepl("^More than\\s+[0-9.,]+[KkMm]?$", label)) {
      threshold <- parse_numeric_unit(gsub("^More than\\s+", "", label))
      return(function(x) ifelse(x > threshold, label, NA))
    }
    if (grepl("^[0-9.,]+[KkMm]?\\s*(–|-)\\s*[0-9.,]+[KkMm]?$", label)) {
      bounds <- unlist(strsplit(label, "\\s*(–|-)\\s*"))
      lower  <- parse_numeric_unit(bounds[1])
      upper  <- parse_numeric_unit(bounds[2])
      return(function(x) ifelse(x >= lower & x <= upper, label, NA))
    }
    if (grepl("^[0-9.,]+[KkMm]?\\+$", label)) {
      threshold <- parse_numeric_unit(gsub("\\+", "", label))
      return(function(x) ifelse(x >= threshold, label, NA))
    }
    if (grepl("^[0-9.,]+[KkMm]?\\-$", label)) {
      threshold <- parse_numeric_unit(gsub("\\-", "", label))
      return(function(x) ifelse(x <= threshold, label, NA))
    }
    if (grepl("^[0-9.,]+[KkMm]?$", label)) {
      target <- parse_numeric_unit(label)
      return(function(x) ifelse(x == target, label, NA))
    }
    
    stop(paste("Unrecognized grouping label:", label))
  }
  
  group_assignments <- rep(NA_character_, length(raw_vals))
  for (label in grouping_vec) {
    rule_fn <- parse_grouping_rule(label)
    matched <- is.na(group_assignments) & !is.na(raw_vals)
    group_assignments[matched] <- rule_fn(raw_vals[matched])
  }
  
  group_assignments[is.na(group_assignments) & !is.na(raw_vals)] <-
    as.character(raw_vals[is.na(group_assignments) & !is.na(raw_vals)])
  
  get_custom_order <- function(labels) {
    get_sort_category <- function(label) {
      if (grepl("Less|less|-", label)) return("low")
      if (grepl("More|more|\\+", label)) return("high")
      return("mid")
    }
    extract_number <- function(label) {
      nums <- as.numeric(unlist(regmatches(label, gregexpr("\\d+\\.*\\d*", label))))
      if (length(nums) > 0) return(nums[1])
      return(NA_real_)
    }
    df <- data.frame(
      label    = labels,
      category = sapply(labels, get_sort_category),
      number   = sapply(labels, extract_number),
      stringsAsFactors = FALSE
    )
    df$category <- factor(df$category, levels = c("low", "mid", "high"))
    df_sorted   <- df[order(df$category, df$number), ]
    return(df_sorted$label)
  }
  
  extra_vals   <- setdiff(as.character(sort(unique(raw_vals))), grouping_vec)
  raw_levels   <- unique(c(grouping_vec, extra_vals))
  final_levels <- get_custom_order(raw_levels)
  
  df$category_grouped <- factor(group_assignments, levels = final_levels, ordered = TRUE)
  return(df)
}

# ------ ORDINAL HELPERS ------------------------------------------------------

#' Format x-axis labels as ordinal numbers
#'
#' Converts numeric strings to "1st", "2nd", "3rd", etc. for display.
#'
#' @param v A vector of strings or numbers.
#'
#' @return Vector with formatted ordinal strings or original values.
format_x_labels_as_ordinal <- function(v) {
  v_num <- suppressWarnings(as.numeric(as.character(v)))
  if (all(!is.na(v_num))) ordinal_label(v_num) else v
}

# ------ EXPORT TO SLIDE ------------------------------------------------------

#' Export a ggplot2 chart to a PowerPoint slide
#'
#' Adds a new slide, inserts the chart as a transparent vector graphic,
#' and places a formatted title at the top.
#'
#' @param ppt_doc An officer pptx object.
#' @param plot_obj A ggplot2 object.
#' @param title_text Title for the slide.
#' @param is_first Logical. If TRUE, modifies the first slide instead of adding new one.
#'
#' @return Modified ppt_doc object.
export_plot_to_slide <- function(
    ppt_doc,
    plot_obj,
    title_text = " ",
    is_first   = FALSE
) {
  if (!is.null(ppt_doc)) {
    if (is.null(is_first) || !is_first) {
      ppt_doc <- add_slide(ppt_doc)
    }
    
    dims <- slide_size(ppt_doc)
    
    ppt_doc <- ppt_doc %>%
      ph_with(
        value = block_list(
          fpar(
            ftext(
              title_text,
              fp_text(
                font.size   = 28,
                bold        = TRUE,
                font.family = "Arial",
                color       = "#323233"
              )
            )
          )
        ),
        location = ph_location(
          left   = 0.4,
          top    = 0.2,
          width  = dims$width - 1,
          height = 0.8
        )
      ) %>%
      ph_with(
        value = dml(ggobj = plot_obj, bg = "transparent"),
        location = ph_location(
          left   = 0,
          top    = 1.0,
          width  = dims$width,
          height = dims$height - 1.4
        )
      )
  }
  
  return(ppt_doc)
}
