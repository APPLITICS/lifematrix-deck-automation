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
    n_activities,
    unit,
    tile_fill
) {
  
  # ------ SETTINGS ------------------------------------------------------------
  circle_color <- "#21b2aa"
  # ------- PREPARE DATA -------------------------------------------------------
  data <- data %>%
    mutate(
      row = row_number(),
      y = rev(row_number()),
      x = 1,
      lineheight = 1.00
    )
  
  # Format duration label with optional unit and singular "hr" handling.
  data$label_text <- if (is.null(unit)) {
    as.character(data$avg_hours)
  } else {
    suffix <- ifelse(data$avg_hours == 1 & unit == "hrs", "hr", unit)
    paste0(data$avg_hours, " ", suffix)
  }
  
  max_nchar <- max(nchar(data$label ), na.rm = TRUE)
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
  ggplot(data) +
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
    ) +
    geom_text(
      aes(
        x = x - 0.12,
        y = y,
        label = label,
        lineheight = lineheight
      ),
      color = "white",
      hjust = 0,
      size = 9,
      fontface = "bold",
    ) +
    geom_text(
      aes(
        x = x + 0.14,
        y = y + 0.1,
        label = label_text
      ),
      color = "white",
      hjust = 1,
      size = 9,
      fontface = "bold",
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
    n_activities,
    unit,
    tile_fill
) {
  
  # ------ SETTINGS ------------------------------------------------------------
  header_fill = "white"
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
  data <- data %>%
    group_by(group) %>%
    mutate(
      col = cur_group_id() * group_spacing,
      row = row_number(),
      y = row + 1,
      x = col,
      label = sapply(
        label,
        wrap_label,
        width = label_width - 4
      ),
      avg_hours = avg_hours,
      is_multiline = str_detect(label, "\n"),
      lineheight = ifelse(is_multiline, 1.25, 1.00)
    ) %>%
    ungroup()
  # Format duration label with optional unit and singular "hr" handling.
  data$label_text <- if (is.null(unit)) {
    as.character(data$avg_hours)
  } else {
    suffix <- ifelse(data$avg_hours == 1 & unit == "hrs", "hr", unit)
    paste0(data$avg_hours, " ", suffix)
  }
  
  headers <- data %>%
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
      data = data,
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
      data = data,
      aes(
        x = col - 0.3,
        y = y,
        label = label,
        lineheight = lineheight
      ),
      hjust = 0,
      fontface = "bold",
      color = "white",
      size = 6.5
    ) +
    geom_text(
      data = data,
      aes(
        x = col + 0.3,
        y = y,
        label = label_text
      ),
      hjust = 1,
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
      fontface = "bold",
      color = "black",
      size = 8
    ) +
    scale_y_reverse(
      limits = c(max(data$y) + 0.2, 0.7)
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
#' Tiles are colored based on the preferred value direction (e.g.,high = green).
#'
#' The chart can be rendered in two modes:
#' - Focal-only mode: A single vertical column of activity tiles.
#' - Grouped mode: One column per group (focal + comparisons), each with top 
#' activities.
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
  
  # ------ EXTRACT INSTRUCTION FIELDS ------------------------------------------
  # Centralize all inputs and defaults from `instruction` for clarity
  metric_names <- instruction$metrics %||% character()
  metric_hours <- instruction$metric_hours %||% character()
  focal_group <- instruction$focal_group
  comparison_groups <- instruction$comparison_groups %||% list()
  has_comparisons <- length(comparison_groups) > 0
  unit <- instruction$unit
  preferred_value <- instruction$preferred_value %||% "high"
  n_activities <- instruction$n_activities %||% 5
  
  # ------ HELPERS -------------------------------------------------------------
  # Avoid repeating null/NA checks when extracting subset titles
  get_subset_title <- function(x) {
    if (is.null(x) || is.null(x$title) || is.na(x$title)) return(NULL)
    x$title
  }
  
  # ------ EARLY VALIDATION ----------------------------------------------------
  # Prevent runtime errors by verifying required metrics/subsets exist
  required_metrics <- c(metric_names, metric_hours)
  required_metrics <- required_metrics[!is.null(required_metrics) & 
                                         !is.na(required_metrics)]
  missing_metric_cols <- setdiff(required_metrics, names(data))
  
  subset_titles <- character()
  fg_subset_title <- get_subset_title(focal_group$subset)
  if (!is.null(fg_subset_title)){
    subset_titles <- c(subset_titles, fg_subset_title)
  }
  if (has_comparisons) {
    for (cg in comparison_groups) {
      cg_subset_title <- get_subset_title(cg$subset)
      if (!is.null(cg_subset_title)){
        subset_titles <- c(subset_titles, cg_subset_title)
      }
    }
  }
  subset_titles <- unique(subset_titles)
  missing_subset_cols <- setdiff(subset_titles, names(data))
  
  all_missing_cols <- unique(c(missing_metric_cols, missing_subset_cols))
  if (length(all_missing_cols) > 0) {
    message(
      "❌ missing_vars column(s): ",
      paste(all_missing_cols, collapse = ", "),
      ". Slide skipped."
    )
    return(NULL)
  }
  
  # ------ VARIABLE MAP SYNC ---------------------------------------------------
  # Guarantee that every metric is present in `variable_map` for labels
  missing_vars <- setdiff(metric_hours, variable_map$variable)
  if (length(missing_vars)) {
    variable_map <- bind_rows(
      variable_map,
      tibble(variable = missing_vars, label = missing_vars)
    ) %>% distinct(variable, .keep_all = TRUE)
  }
  
  # ------ FOCAL GROUP FILTERING -----------------------------------------------
  # Isolate focal group data and apply subset rules for filtering
  focal_name <- focal_group$name
  fg_subset_vals <- focal_group$subset$value %||% NULL
  
  data_focal <- data %>%
    filter(group == focal_name)
  
  if (!is.null(fg_subset_title) && fg_subset_title %in% names(data)) {
    data_focal <- data_focal %>%
      filter(.data[[fg_subset_title]] %in% fg_subset_vals)
    
    data_focal$group <- if (length(fg_subset_vals) == 1) {
      paste(focal_name, fg_subset_vals)
    } else {
      focal_name
    }
  }
  
  # ------ COMPARISON GROUPS FILTERING -----------------------------------------
  # Apply the same logic for comparison groups to allow benchmarking
  comparison_data_list <- list()
  if (has_comparisons) {
    comparison_data_list <- lapply(comparison_groups, function(cg) {
      cg_name <- cg$name
      cg_subset_title <- get_subset_title(cg$subset)
      cg_subset_vals <- cg$subset$value %||% NULL
      
      comp_df <- data %>%
        filter(group == cg_name)
      
      if (!is.null(cg_subset_title) && cg_subset_title %in% names(data)) {
        comp_df <- comp_df %>%
          filter(.data[[cg_subset_title]] %in% cg_subset_vals)
        
        comp_df$group <- if (length(cg_subset_vals) == 1) {
          paste(cg_name, cg_subset_vals)
        } else {
          cg_name
        }
      }
      comp_df
    })
  }
  
  # ------ FINAL SUMMARY -------------------------------------------------------
  # Create a unified dataset for subsequent aggregation and plotting
  combined_data <- bind_rows(c(list(data_focal), comparison_data_list))
  
  # ------ GROUP LEVELS (ORDERING) ---------------------------------------------
  # Ensure group display order matches instructions for consistency
  focal_level <- if (is.null(fg_subset_title) || is.null(fg_subset_vals) || 
                     length(fg_subset_vals) != 1) {
    focal_group$name
  } else {
    paste(focal_group$name, fg_subset_vals)
  }
  
  comp_levels <- vapply(
    comparison_groups,
    function(g) {
      has_subset <- !is.null(g$subset) && !is.null(g$subset$title)
      one_val <- has_subset && !is.null(g$subset$value) && 
        length(g$subset$value) == 1
      if (one_val) paste(g$name, g$subset$value) else g$name
    },
    character(1)
  )
  
  group_levels <- c(focal_level, comp_levels)
  
  # ------ ACTIVITY MAP (VALUE ↔ HOURS WITH LABELS) ----------------------------
  # Link each value metric to its corresponding hours metric + label
  activity_mapping <- tibble(
    activity_value = metric_names,
    activity_hours = metric_hours
  ) %>%
    left_join(variable_map, by = c("activity_hours" = "variable")) 
  
  # ------ STEP 2: MEANS (VALUE METRICS) ---------------------------------------
  # Compute per-group averages for subjective/activity values
  activity_value_means <- combined_data %>%
    group_by(group) %>%
    summarise(
      across(all_of(metric_names), ~ mean(.x, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    pivot_longer(
      cols = all_of(metric_names),
      names_to = "activity_value",
      values_to = "avg_value"
    )
  
  # ------ STEP 3: MEANS (HOURS METRICS) ---------------------------------------
  # Compute per-group averages for objective time spent (hours)
  activity_hours_means <- combined_data %>%
    group_by(group) %>%
    summarise(
      across(all_of(metric_hours), ~ mean(.x, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    pivot_longer(
      cols = all_of(metric_hours),
      names_to = "activity_hours",
      values_to = "avg_hours"
    )
  
  # ------ STEP 4: MAP TO HOURS ------------------------------------------------
  # Merge subjective values and objective hours for each activity
  tile_data <- activity_value_means %>%
    left_join(activity_mapping, by = "activity_value") %>%
    left_join(activity_hours_means, by = c("group", "activity_hours"))
  
  # ------ CHOOSE SLICE FUNCTION -----------------------------------------------
  # Select the slice function based on preferred value direction (high or low)
  slice_fun <- if (preferred_value == "low") slice_min else slice_max
  
  # ------ TILE DATA SELECTION -------------------------------------------------
  # Select top-N activities, order by group, and clean values
  tile_data <- tile_data %>%
    group_by(group) %>%
    slice_fun(order_by = avg_value, n = n_activities, with_ties = FALSE) %>%
    arrange(group, desc(avg_hours)) %>%
    ungroup() %>%
    mutate(
      avg_value = round(avg_value, 1),
      avg_hours = round(avg_hours, 0)
    )
  
  # ------ TILE FILL COLOR -----------------------------------------------------
  # Visually distinguish high vs low preference tiles
  tile_fill <- switch(
    preferred_value,
    "high" = "#6ec17c",
    "low" = "#d95b61",
    "#a569bd" # default
  )
  
  # ------ GENERATE PLOT -------------------------------------------------------
  # Use correct drawing function depending on comparison presence
  plot_obj <- if (!has_comparisons) {
    draw_tile_chart_focal(
      data = tile_data,
      n_activities = n_activities,
      unit = unit,
      tile_fill = tile_fill
    )
  } else {
    draw_tile_chart_groups(
      data = tile_data,
      n_activities = n_activities,
      unit = unit,
      tile_fill = tile_fill
    )
  }
  
  # ------ EXPORT TO SLIDE -----------------------------------------------------
  # Embed final plot into PowerPoint, preserving order and flags
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

