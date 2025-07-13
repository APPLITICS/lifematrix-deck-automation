# ------ HELPER FUNCTION: WRAP LABEL ------------------------------------------

#' Wrap Text Label for Plotting
#'
#' Splits a character string into multiple lines to improve readability 
#' in plot annotations. Optionally limits the number of lines.
#'
#' @param text A character string to be wrapped.
#' @param width Maximum number of characters per line (default = 20).
#' @param max_lines Maximum number of lines to display (default = 2).
#'
#' @return A string with newline characters inserted for line breaks.

wrap_label <- function(text, width = 20, max_lines = 2) {
  lines <- strwrap(text, width = width)
  lines <- lines[seq_len(min(length(lines), max_lines))]
  paste(lines, collapse = "\n")
}
# ------ CHART FUNCTION: TILE CHART FOCAL ONLY -------------------------------

#' Draw Focal-Only Tile Chart
#'
#' Creates a tile-based vertical chart showing the top N activities for a single
#' focal group. Each activity is displayed with a rounded rectangle, circle marker,
#' and label pair (activity + duration).
#'
#' @param data A data frame with `activity`, `duration`, and `group` columns.
#' @param n_activities Number of activities to display (default = 5).
#' @param tile_fill Fill color for the tile background.
#'
#' @return A `ggplot` object showing the focal group's top activities.
draw_tile_chart_focal <- function(
    data,
    n_activities = 5,
    tile_fill
) {
  # ------ SETTINGS ------------------------------------------------------------
  circle_color <- "#21b2aa"
  font_family = "RoundedSans"
  # ------- PREPARE DATA -------------------------------------------------------
  df <- data %>%
    arrange(desc(as.numeric(str_extract(duration, "\\d+")))) %>%
    mutate(
      row = row_number(),
      y = rev(row_number()),
      x = 1,
      activity_label = sapply(
        activity,
        wrap_label,
        width = 25
      ),
      duration_label = duration,
      is_multiline = str_detect(activity_label, "\n"),
      lineheight = ifelse(is_multiline, 1.25, 1.00)
    )
  
  max_nchar <- max(nchar(data$activity), na.rm = TRUE)
  # ------ DYNAMIC CIRCLE SIZE -------------------------------------------------
  circle_size <- {
    n_min <- 2
    n_max <- 10
    size_min <- 10
    size_max <- 42
    
    scaled <- size_max - (size_max - size_min) * 
      (n_activities - n_min) / (n_max - n_min)
    
    pmax(size_min, pmin(size_max, scaled))
  }
  # ------ DYNAMIC MARGINS -----------------------------------------------------
  top_bottom_margin <- if (n_activities <= 4) {
    base_margin <- 120
    step_margin <- 20
    margin_val <- base_margin - (n_activities - 1) * step_margin
    pmax(0, margin_val)
  } else {
    0
  }
  
  left_right_margin <- {
    base_margin <- 200
    scale_factor <- 3
    dynamic_margin <- base_margin + (max_nchar - 10) * scale_factor
    pmax(100, dynamic_margin)
  }
  # ------ PLOT ----------------------------------------------------------------
  ggplot(df) +
    geom_tile(
      aes(
        x = x,
        y = y + 0.1
      ),
      width = 0.3,
      height = 0.8,
      fill = tile_fill,
      color = NA
    ) +
    geom_point(
      aes(
        x = x - 0.15,
        y = y + 0.1
      ),
      shape = 21,
      fill = "white",
      color = circle_color,
      size = circle_size,
      stroke = 3
    ) +
    geom_text(
      aes(
        x = x - 0.15,
        y = y + 0.05,
        label = row
      ),
      color = "black",
      fontface = "bold",
      size = 10,
      family = font_family
    ) +
    geom_text(
      aes(
        x = x - 0.12,
        y = y,
        label = activity_label,
        lineheight = lineheight
      ),
      color = "white",
      hjust = 0,
      size = 9,
      fontface = "bold",
      family = font_family
    ) +
    geom_text(
      aes(
        x = x + 0.14,
        y = y + 0.05,
        label = duration_label
      ),
      color = "white",
      hjust = 1,
      size = 9,
      fontface = "bold",
      family = font_family
    ) +
    coord_cartesian(clip = "off") +
    theme_void() +
    theme(
      plot.margin = margin(
        top_bottom_margin,
        left_right_margin,
        top_bottom_margin,
        left_right_margin
      )
    )
}
# ------ CHART FUNCTION: TILE CHART WITH GROUPS --------------------------------

#' Draw Grouped Tile Chart (Focal + Comparisons)
#'
#' Creates a grouped tile chart with one vertical column per group (focal +
#' comparisons), each showing the top N activities. Labels and duration values are
#' displayed within rounded tiles, with dynamic spacing and group headers.
#'
#' @param data A data frame with `activity`, `duration`, and `group` columns.
#' @param n_activities Number of activities to display per group (default = 5).
#' @param tile_fill Fill color for the tile background.
#'
#' @return A `ggplot` object showing grouped activity tiles per group.
draw_tile_chart_groups <- function(
    data,
    n_activities = 5,
    tile_fill
) {
  # ------ SETTINGS ------------------------------------------------------------
  header_fill = "white"
  font_family = "RoundedSans"
  group_spacing <- 0.8
  n_groups <- length(unique(data$group))
  # Determine label width and horizontal margin based on number of groups
  label_width <- case_when(
    n_groups == 2 ~ 25,
    n_groups == 3 ~ 20,
    n_groups == 4 ~ 15,
    TRUE ~ 25
  )
  
  horizontal_margin <- case_when(
    n_groups == 2 ~ 120,
    n_groups == 3 ~ 50,
    TRUE ~ 30
  )
  # ------ PREPARE DATA --------------------------------------------------------
  df <- data %>%
    group_by(group) %>%
    arrange(desc(as.numeric(str_extract(duration, "\\d+")))) %>%
    mutate(
      col = cur_group_id() * group_spacing,
      row = row_number(),
      y = row + 1,
      x = col,
      activity_label = sapply(
        activity,
        wrap_label,
        width = label_width - 4
      ),
      duration_label = duration,
      is_multiline = str_detect(activity_label, "\n"),
      lineheight = ifelse(is_multiline, 1.25, 1.00)
    ) %>%
    ungroup()
  
  headers <- df %>%
    distinct(group, col) %>%
    mutate(y = 1)
  
  activity_count <- data %>% count(group)
  avg_activity <- mean(activity_count$n)
  
  tile_padding <- unit(
    0.35 + (5 - avg_activity) * 0.05,
    "lines"
  )
 # ------ PLOT -----------------------------------------------------------------
  ggplot() +
    geom_label(
      data = df,
      aes(
        x = col,
        y = y,
        label = strrep(" ", label_width)
      ),
      fill = tile_fill,
      label.size = NA,
      label.r = unit(0.35, "lines"),
      label.padding = tile_padding,
      size = 14
    ) +
    geom_label(
      data = headers,
      aes(
        x = col,
        y = y,
        label = strrep(" ", label_width + 5)
      ),
      fill = header_fill,
      label.size = NA,
      label.r = unit(0.35, "lines"),
      label.padding = unit(0.25, "lines"),
      size = 12
    ) +
    geom_text(
      data = df,
      aes(
        x = col - 0.3,
        y = y,
        label = activity_label,
        lineheight = lineheight
      ),
      hjust = 0,
      family = font_family,
      fontface = "bold",
      color = "white",
      size = 6.5
    ) +
    geom_text(
      data = df,
      aes(
        x = col + 0.3,
        y = y,
        label = duration_label
      ),
      hjust = 1,
      family = font_family,
      fontface = "bold",
      color = "white",
      size = 6.5
    ) +
    geom_text(
      data = headers,
      aes(
        x = col,
        y = y,
        label = group
      ),
      family = font_family,
      fontface = "bold",
      color = "black",
      size = 8
    ) +
    scale_y_reverse(
      limits = c(max(df$y) + 0.2, 0.7)
    ) +
    theme_void() +
    theme(
      plot.margin = margin(
        0,
        horizontal_margin,
        0,
        horizontal_margin
      )
    )
}

# ------ CHART FUNCTION: TILE SLIDE -------------------------------------------

#' Generate Tile Chart Slide
#'
#' Creates a PowerPoint slide with a tile-based chart showing the top N activities
#' and their durations for a focal group, with optional comparison groups. 
#' Tiles are colored based on the preferred value direction (e.g., high = green).
#'
#' The chart can be rendered in two modes:
#' - Focal-only mode: A single vertical column of activity tiles.
#' - Grouped mode: One column per group (focal + comparisons), each with top activities.
#'
#' @param data A data frame containing activity metrics and group identifiers.
#' @param instruction A list of slide instruction configurations, including:
#' @param ppt_doc Optional `read_pptx()` object to append the slide to.
#'
#' @return Updated `pptx` object if `ppt_doc` is provided, otherwise `NULL`.
generate_tile_slide <- function(
    data,
    instruction,
    ppt_doc
) {
  # ------ EXTRACT instruction FIELDS ------------------------------------------
  font_family <- "RoundedSans"
  metric_names <- instruction$metric
  common_suffix <- stringr::str_extract(metric_names, "_[^_]+$")
  suffix_to_remove <- if (length(unique(common_suffix)) == 1) {
    unique(common_suffix)
  } else {
    ""
  }
  
  focal_name <- instruction$focal_group$name
  comp_groups <- instruction$comparison_groups %||% list()
  has_comparisons <- length(comp_groups) > 0
  
  # ------ FOCAL GROUP FILTERING -----------------------------------------------
  data_focal <- data %>% filter(group == focal_name)
  
  fg_subset_col <- instruction$focal_group$subset$title %||% NULL
  fg_subset_val <- instruction$focal_group$subset$value %||% NULL
  
  if (!is.null(fg_subset_col) && fg_subset_col %in% names(data)) {
    data_focal <- data_focal %>%
      filter(.data[[fg_subset_col]] %in% fg_subset_val)
    
    data_focal$group <- if (length(fg_subset_val) == 1) {
      paste(focal_name, fg_subset_val)
    } else {
      focal_name
    }
  }
  
  # ------ COMPARISON GROUPS FILTERING -----------------------------------------
  comparison_data_list <- if (!is.null(comp_groups)) {
    lapply(comp_groups, function(cg) {
      subset_col <- cg$subset$title %||% NULL
      subset_val <- cg$subset$value %||% NULL
      
      data_comp <- data %>% filter(group == cg$name)
      
      if (!is.null(subset_col) && subset_col %in% names(data)) {
        data_comp <- data_comp %>%
          filter(.data[[subset_col]] %in% subset_val)
        
        data_comp$group <- if (length(subset_val) == 1) {
          paste(cg$name, subset_val)
        } else {
          cg$name
        }
      }
      
      data_comp
    })
  } else {
    list()
  }
  
  # ------ FINAL SUMMARY -------------------------------------------------------
  combined_data <- bind_rows(data_focal, !!!comparison_data_list)
  
  summary_table <- combined_data %>%
    group_by(group) %>%
    summarise(
      across(
        all_of(metric_names),
        ~ mean(.x, na.rm = TRUE)
      )
    ) %>%
    ungroup()
  
  # ------ GROUP LEVELS --------------------------------------------------------
  group_levels <- if (
    is.null(instruction$focal_group$subset) ||
    length(instruction$focal_group$subset$value) > 1
  ) {
    instruction$focal_group$name
  } else {
    paste(
      instruction$focal_group$name,
      instruction$focal_group$subset$value
    )
  }
  
  comp_names <- vapply(
    comp_groups,
    function(g) {
      if (is.null(g$subset) || length(g$subset$value) > 1) {
        g$name
      } else {
        paste(g$name, g$subset$value)
      }
    },
    character(1)
  )
  
  group_levels <- c(group_levels, comp_names)
  
  # ------ PREPARE DATA FOR PLOT -----------------------------------------------
  tile_data <- summary_table %>%
    pivot_longer(
      cols = all_of(metric_names),
      names_to = "activity",
      values_to = "value"
    ) %>%
    mutate(
      activity = activity %>%
        str_remove(paste0(suffix_to_remove, "$")) %>%
        str_replace_all("_", " ") %>%
        str_to_title(),
      group = factor(group, levels = group_levels)
    )
  
  preferred_value <- instruction$preferred_value %||% "high"
  preferred_value <- preferred_value[1]
  
  tile_fill <- switch(
    preferred_value,
    "high" = "#6ec17c",
    "low" = "#d95b61",
    "#a569bd"
  )
  
  n_activities <- instruction$n_activities %||% 5
  
  tile_data_top <- tile_data %>%
    group_by(group) %>%
    slice_max(
      order_by = value,
      n = n_activities,
      with_ties = FALSE
    ) %>%
    ungroup() %>%
    mutate(duration = paste0(round(value, 0), " hrs")) %>%
    select(group, activity, duration)
  
  # ------ GENERATE PLOT -------------------------------------------------------
  plot_obj <- if (!has_comparisons) {
    draw_tile_chart_focal(
      data = tile_data_top,
      n_activities = n_activities,
      tile_fill = tile_fill
    )
  } else {
    draw_tile_chart_groups(
      data = tile_data_top,
      n_activities = n_activities,
      tile_fill = tile_fill
    )
  }
  
  # ------ EXPORT TO SLIDE -----------------------------------------------------
  if (!is.null(ppt_doc)) {
    ppt_doc <- export_plot_to_slide(
      ppt_doc = ppt_doc,
      plot_obj = plot_obj,
      title_text = instruction$title %||% " ",
      is_first = instruction$is_first
    )
    return(ppt_doc)
  }
  
  return(invisible(NULL))
}
