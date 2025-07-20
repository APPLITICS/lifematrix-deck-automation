# ------ BAR STACKED SLIDE --------------------------------------------------
#' Generate Stacked Bar Chart Slide
#'
#' Creates a stacked bar chart with vertical bars segmented by a categorical
#' variable. Supports comparison of multiple `category_y` variables side by
#' side using horizontal offsets. Optionally overlays average metric values
#' above each bar.
#'
#' @param data A data frame with preprocessed survey data.
#' @param instruction A list of instructions
#' @param ppt_doc A PowerPoint object to append the slide to.
#'
#' @return Updated pptx object (if `ppt_doc` is provided).
generate_bar_stacked_slide <- function(
    data,
    instruction,
    ppt_doc = NULL
) {
  # ------ EXTRACT SETTINGS -------------------------------------------------
  # Retrieve variables and metadata from the instruction list
  cat_x <- instruction$category_x$name
  order_x <- instruction$category_x$order
  category_y <- instruction$category_y
  focal_name <- instruction$focal_group$name
  metric_var <- instruction$metric %||% NULL
  unit_label <- instruction$unit %||% ""
  chart_title <- instruction$title %||% " "
  
  # ------ ADJUST VISUAL PARAMETERS -----------------------------------------
  # Tune bar width, spacing, and margins based on number of category_y
  multi_y <- length(category_y) > 1
  bar_width <- if (multi_y) 0.35 else 0.55
  x_expand <- if (multi_y) c(0.3, 0.3) else c(0.1, 0.1)
  plot_margin <- if (multi_y) margin(0, 20, 0, 20) else margin(0, 100, 0, 100)
  
  # ------ DETERMINE X-AXIS ORDER -------------------------------------------
  # Sort and subset x-axis categories if ordering is specified
  lvl_x <- if (!is.null(order_x) && all(c(cat_x, order_x) %in% names(data))) {
    data %>%
      select(x_val = all_of(cat_x), x_order = all_of(order_x)) %>%
      filter(!is.na(x_val), !is.na(x_order)) %>%
      distinct() %>%
      arrange(x_order) %>%
      pull(x_val)
  } else {
    unique(data[[cat_x]])
  }
  x_values <- instruction$category_x$value %||% lvl_x
  
  # ------ FILTER AND RENAME BASE DATA --------------------------------------
  # Subset data to focal group and apply optional subset filters
  df <- data %>%
    filter(group == focal_name) %>%
    rename(x = all_of(cat_x)) %>%
    filter(x %in% x_values) %>%
    mutate(x = factor(x, levels = x_values))
  
  if (!is.null(instruction$focal_group$subset)) {
    subset_col <- instruction$focal_group$subset$title
    subset_val <- instruction$focal_group$subset$value
    if (!is.null(subset_col) && !is.null(subset_val)) {
      df <- df %>% filter(.data[[subset_col]] == subset_val)
    }
  }
  
  # ------ COMPUTE GLOBAL Y-LEVEL ORDERING ----------------------------------
  # Use global frequencies across all category_y to define consistent fill order
  all_y_names <- purrr::map_chr(category_y, "name")
  lvl_y_global <- df %>%
    select(any_of(all_y_names)) %>%
    pivot_longer(cols = everything(), values_to = "val", names_to = NULL) %>%
    filter(!is.na(val)) %>%
    count(val) %>%
    arrange(desc(n)) %>%
    pull(val) %>%
    as.character()
  
  # ------ LOOP OVER CATEGORY_Y TO CREATE TABS ------------------------------
  # Generate a table of frequencies for each category_y, with optional filtering
  all_tabs <- list()
  for (i in seq_along(category_y)) {
    cat_y_info <- category_y[[i]]
    cat_y_name <- cat_y_info$name
    order_y <- cat_y_info$order
    value_y <- cat_y_info$value %||% NULL
    
    # ------ DEFINE BAR POSITION LABEL -------------------------------------
    bar_label <- if (multi_y) {
      label_match <- variable_map$label[variable_map$variable == cat_y_name]
      if (length(label_match) > 0) label_match else paste0("Bar_", i)
    } else {
      "Stack"
    }
    
    # ------ DETERMINE Y ORDERING ------------------------------------------
    lvl_y <- if (!is.null(order_y) && all(c(cat_y_name, order_y) %in% names(data))) {
      data %>%
        select(y_val = all_of(cat_y_name), y_order = all_of(order_y)) %>%
        filter(!is.na(y_val), !is.na(y_order)) %>%
        distinct() %>%
        arrange(y_order) %>%
        pull(y_val)
    } else {
      intersect(lvl_y_global, unique(na.omit(df[[cat_y_name]])))
    }
    
    # ------ CREATE STACKED FREQUENCY TABLE --------------------------------
    tab <- df %>%
      filter(.data[[cat_y_name]] %in% lvl_y) %>%
      rename(y = all_of(cat_y_name)) %>%
      mutate(
        y = factor(y, levels = lvl_y),
        bar_position = bar_label
      ) %>%
      count(x, bar_position, y) %>%
      group_by(x, bar_position) %>%
      mutate(prop = n / sum(n)) %>%
      ungroup()
    
    if (!is.null(value_y)) {
      tab <- tab %>% filter(y %in% value_y)
    }
    
    all_tabs[[i]] <- tab
  }
  
  # ------ COMBINE & OFFSET MULTIPLE BARS -----------------------------------
  # Apply horizontal offset to bars if comparing multiple category_y
  combined_data <- bind_rows(all_tabs) %>%
    mutate(x = factor(x, levels = x_values))
  
  x_map <- tibble(
    x = factor(x_values, levels = x_values),
    x_num = seq_along(x_values)
  )
  
  n_positions <- length(unique(combined_data$bar_position))
  offsets <- seq(-0.22, 0.22, length.out = n_positions)
  offset_map <- tibble(
    bar_position = unique(combined_data$bar_position),
    offset = if (multi_y) offsets else rep(0, n_positions)
  )
  
  combined_data <- combined_data %>%
    left_join(x_map, by = "x") %>%
    left_join(offset_map, by = "bar_position") %>%
    mutate(x_offset = x_num + offset)
  
  # ------ COLOR PALETTE ----------------------------------------------------
  # Generate fill color palette based on y-levels
  fill_levels <- unique(combined_data$y)
  pal <- get_color_palette(fill_levels)
  
  # ------ TEXT LABELS ------------------------------------------------------
  # Prepare top labels (bar label), bottom labels (x-axis group), value labels
  top_bar_labels <- combined_data %>%
    distinct(x, x_num, bar_position, x_offset) %>%
    mutate(label = bar_position)
  
  bottom_group_labels <- combined_data %>%
    distinct(x, x_num) %>%
    mutate(label = as.character(x))
  
  # ------ METRIC LABELS (OPTIONAL) -----------------------------------------
  # If a numeric metric is provided, compute group-level averages to annotate
  label_df <- NULL
  if (!is.null(metric_var) && metric_var %in% names(df)) {
    label_df <- df %>%
      select(x, all_of(metric_var)) %>%
      group_by(x) %>%
      summarise(value = mean(.data[[metric_var]], na.rm = TRUE), .groups = "drop") %>%
      mutate(
        label = round(value, 1),
        y_pos = 1.13
      ) %>%
      left_join(x_map, by = "x")
  }
  
  y_limit_top <- if (!is.null(label_df)) max(label_df$y_pos, 1.05) else 1.05
  
  # ------ BUILD PLOT OBJECT ------------------------------------------------
  # Assemble the full ggplot chart with stacked bars, annotations, and styling
  plot_obj <- ggplot(combined_data) +
    geom_bar(
      aes(x = x_offset, y = prop, fill = y, group = interaction(x, bar_position)),
      stat = "identity",
      width = bar_width
    ) +
    geom_text(
      data = combined_data %>% filter(prop >= 0.03),
      aes(
        x = x_offset,
        y = prop,
        label = scales::percent(prop, accuracy = 1),
        group = interaction(x, bar_position)
      ),
      stat = "identity",
      position = position_stack(vjust = 0.5),
      color = "black",
      fontface = "bold",
      size = 5
    ) +
    {
      if (multi_y) {
        geom_text(
          data = top_bar_labels,
          aes(x = x_offset, y = 1.05, label = label),
          inherit.aes = FALSE,
          size = 4,
          fontface = "bold",
          color = "white"
        )
      }
    } +
    {
      if (!is.null(label_df)) {
        list(
          geom_text(
            data = label_df,
            aes(x = x_num, y = y_pos, label = label),
            inherit.aes = FALSE,
            color = "yellow",
            fontface = "bold",
            size = 7
          ),
          annotate(
            "text",
            x = 0.7,
            y = label_df$y_pos[1],
            label = "Avg :",
            hjust = 1,
            color = "yellow",
            fontface = "bold",
            size = 7
          )
        )
      }
    } +
    geom_hline(
      yintercept = 0,
      color = "white",
      linewidth = 1.5
    ) +
    geom_text(
      data = bottom_group_labels,
      aes(x = x_num, y = -0.07, label = label),
      inherit.aes = FALSE,
      size = 6,
      fontface = "bold",
      color = "white"
    ) +
    scale_x_continuous(
      breaks = x_map$x_num,
      labels = rep("", length(x_map$x_num)),
      expand = expansion(add = x_expand)
    ) +
    scale_y_continuous(
      limits = c(-0.1, y_limit_top),
      breaks = NULL,
      labels = NULL,
      expand = expansion(mult = c(0, 0.2))
    ) +
    scale_fill_manual(values = pal, drop = FALSE) +
    labs(x = NULL, y = NULL, fill = NULL) +
    global_theme() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.line.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin = plot_margin,
      legend.position = if (length(fill_levels) <= 1) "none" else "bottom",
      legend.direction = "horizontal",
      legend.text = element_text(color = "white", size = 14, face = "bold"),
      legend.key.height = unit(12, "pt"),
      legend.key.width = unit(12, "pt"),
      legend.title = element_blank()
    ) +
    coord_cartesian(clip = "off")
  
  # ------ EXPORT TO POWERPOINT ---------------------------------------------
  # Add the completed plot to ppt_doc if provided
  if (!is.null(ppt_doc)) {
    ppt_doc <- export_plot_to_slide(
      ppt_doc = ppt_doc,
      plot_obj = plot_obj,
      title_text = chart_title,
      is_first = instruction$is_first
    )
    return(ppt_doc)
  }
  
  return(invisible(NULL))
}
