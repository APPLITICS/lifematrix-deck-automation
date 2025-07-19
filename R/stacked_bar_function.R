# ------ BAR STACKED SLIDE -------------------------------------------------
#' Generate Stacked Bar Chart Slide
#'
#' Creates a stacked bar chart showing categorical distributions per group.
#' Optionally overlays mean value labels above each bar.
#'
#' @param data A data frame with preprocessed survey data.
#' @param instruction A list with plot settings (categories, metric, etc.).
#' @param ppt_doc Optional PowerPoint object to append the slide to.
#' 
#' @return Updated pptx object if provided, otherwise invisible(NULL).
generate_bar_stacked_slide <- function(
    data,
    instruction,
    ppt_doc = NULL
) {
  # ------ EXTRACT SETTINGS -------------------------------------------------
  # Pull variable names and chart title from instruction list
  cat_x <- instruction$category_x$name
  order_x <- instruction$category_x$order
  cat_y <- instruction$category_y$name
  order_y <- instruction$category_y$order
  value_y <- instruction$category_y$value %||% NULL
  focal_name <- instruction$focal_group$name
  metric_var <- instruction$metric %||% NULL
  unit_label <- instruction$unit %||% ""
  chart_title <- instruction$title %||% " "
  
  # ------ EXTRACT ORDERING LEVELS ------------------------------------------
  # Create x and y category levels based on *_levels columns if available
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
  
  lvl_y <- if (!is.null(order_y) && all(c(cat_y, order_y) %in% names(data))) {
    data %>%
      select(y_val = all_of(cat_y), y_order = all_of(order_y)) %>%
      filter(!is.na(y_val), !is.na(y_order)) %>%
      distinct() %>%
      arrange(y_order) %>%
      pull(y_val)
  } else {
    unique(data[[cat_y]])
  }
  
  # ------ FILTER AND FACTORIZE ---------------------------------------------
  # Filter for focal group and valid categories, then factor levels
  df <- data %>%
    filter(group == focal_name) %>%
    filter(.data[[cat_x]] %in% lvl_x) %>%
    filter(.data[[cat_y]] %in% lvl_y) %>%
    rename(x = all_of(cat_x), y = all_of(cat_y)) %>%
    mutate(
      x = factor(x, levels = lvl_x),
      y = factor(y, levels = lvl_y)
    )
  
  # ------ APPLY SUBSET FILTER IF PROVIDED ----------------------------------
  # Optionally filter based on a subgroup column (e.g., gender)
  if (!is.null(instruction$focal_group$subset)) {
    subset_col <- instruction$focal_group$subset$title
    subset_val <- instruction$focal_group$subset$value
    
    if (!is.null(subset_col) && !is.null(subset_val)) {
      df <- df %>%
        filter(.data[[subset_col]] == subset_val)
    }
  }
  
  # ------ COMPUTE STACKED PROPORTIONS --------------------------------------
  # Count y within x, compute proportions and label positions
  tab <- df %>%
    count(x, y) %>%
    group_by(x) %>%
    mutate(
      prop = n / sum(n),
      label_pos = cumsum(prop) - prop / 2
    ) %>%
    ungroup()
  
  # ------ SELECT VISIBLE CATEGORIES ----------------------------------------
  # Keep only levels specified in value_y (if provided)
  full_levels <- levels(df$y)
  used_levels <- if (!is.null(value_y)) intersect(full_levels, value_y) else full_levels
  visible_tab <- tab %>% filter(y %in% used_levels)
  
  # ------ COLOR PALETTE ----------------------------------------------------
  # Generate fill colors for stacked segments
  pal <- get_color_palette(used_levels)
  
  # ------ COMPUTE AVERAGE LABEL POSITIONS ABOVE STACK ----------------------
  # Optionally compute mean value and position label above bar
  label_df <- NULL
  if (!is.null(metric_var) && metric_var %in% names(df)) {
    label_df <- df %>%
      group_by(x) %>%
      summarise(mean_val = mean(.data[[metric_var]], na.rm = TRUE)) %>%
      left_join(
        tab %>% group_by(x) %>% summarise(bar_top = sum(prop)),
        by = "x"
      ) %>%
      mutate(
        label = round(mean_val, 0),
        y_pos = bar_top + 0.06
      )
  }
  
  # ------ DETERMINE Y-AXIS LIMIT BASED ON AVERAGE LABELS -------------------
  # Ensure space for average labels if present
  y_limit <- if (!is.null(label_df)) max(label_df$y_pos, 1.01) else 1.01
  
  # ------ BUILD PLOT -------------------------------------------------------
  # Create stacked bar chart with percent labels and styling
  plot_obj <- ggplot(tab, aes(x = x, y = prop, fill = y)) +
    geom_bar(
      data = visible_tab,
      stat = "identity",
      width = 0.6,
      position = position_stack(reverse = TRUE)
    ) +
    geom_text(
      data = visible_tab,
      aes(label = percent(prop, accuracy = 1)),
      position = position_stack(reverse = TRUE, vjust = 0.5),
      color = "black",
      fontface = "bold",
      size = 6,
      inherit.aes = TRUE
    ) +
    scale_x_discrete(
      expand = expansion(add = c(0.7, 0.4))
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.05)),
      limits = c(0, y_limit),
      breaks = NULL,
      labels = NULL
    ) +
    scale_fill_manual(values = pal, drop = FALSE) +
    labs(
      x = NULL,
      y = NULL,
      fill = NULL,
      title = " "
    ) +
    global_theme() +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin = margin(0, 80, 0, 80),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.text = element_text(color = "white", size = 14, face = "bold"),
      legend.key.height = unit(12, "pt"),
      legend.key.width = unit(12, "pt")
    ) +
    coord_cartesian(clip = "off")
  
  # ------ ADD AVERAGE LABELS ABOVE BARS ------------------------------------
  # Draw yellow text label and annotation for average values
  if (!is.null(label_df)) {
    plot_obj <- plot_obj +
      geom_text(
        data = label_df,
        aes(x = x, y = y_pos, label = label),
        inherit.aes = FALSE,
        color = "yellow",
        fontface = "bold",
        size = 7
      ) +
      annotate(
        "text",
        x = lvl_x[1],
        y = label_df$y_pos[1],
        label = paste("Avg", unit_label, ":"),
        hjust = 1.4,
        color = "yellow",
        fontface = "bold",
        size = 7
      )
  }
  
  # ------ HIDE LEGEND CONTENT IF ONLY ONE CATEGORY DISPLAYED ---------------
  # Suppress legend but keep space if only one visible fill group
  if (length(used_levels) <= 1) {
    plot_obj <- plot_obj +
      guides(fill = guide_legend(override.aes = list(fill = NA))) +
      theme(
        legend.text = element_blank(),
        legend.title = element_blank(),
        legend.key = element_blank()
      )
  }
  
  # ------ EXPORT TO POWERPOINT (OPTIONAL) ----------------------------------
  # Send plot to PowerPoint slide if ppt_doc is supplied
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
