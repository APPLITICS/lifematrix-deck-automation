# ------ BAR STACKED HORIZONTAL SLIDE ------------------------------------------
#' Horizontal Stacked Bar Slide
#'
#' Creates a horizontal stacked bar chart comparing y-group categories
#' (e.g., gender), optionally split by a subset (e.g., Kids/No Kids),
#' with stack segments from x-categories (e.g., working range). 
#' Supports average metric display and PowerPoint export.
#'
#' @param data A preprocessed dataframe (e.g., survey or summary data).
#' @param instruction A list of plotting instructions (categories, filters, etc.).
#' @param ppt_doc A PowerPoint object to append slide to (optional).
#' 
#' @return Updated PowerPoint object (`ppt_doc`) and density plot (`plot_obj`).
generate_stacked_horizontal_slide <- function(
    data,
    instruction,
    ppt_doc = NULL
) {
  # ------ SPACING PARAMETERS --------------------------------------------------
  inter_group_spacing <- 2
  intra_group_spacing <- 0.7
  
  # ------ EXTRACT SETTINGS ----------------------------------------------------
  # Read and standardize chart configuration from instruction list.
  category_x <- instruction$category_x[[1]]
  category_y <- instruction$category_y
  focal_name <- instruction$focal_group$name
  title_text <- instruction$title %||% " "
  metric_var <- instruction$metric %||% NULL
  unit_label <- instruction$unit %||% ""
  cat_x_name <- category_x$name
  order_x <- category_x$order
  value_x <- category_x$value %||% NULL
  cat_y_subset <- category_y[[1]]$subset %||% NULL
  bar_height <- if (is.null(cat_y_subset)) 0.5 else 0.3
  
  # ------ EARLY VALIDATION ----------------------------------------------------
  x_cols <- c(
    cat_x_name,
    order_x %||% NULL
  )
  
  y_cols <- c()
  for (cy in category_y) {
    y_cols <- c(y_cols, cy$name)
    if (!is.null(cy$order)) {
      y_cols <- c(y_cols, cy$order)
    }
    if (!is.null(cy$subset) && !is.null(cy$subset$title)) {
      y_cols <- c(y_cols, cy$subset$title)
    }
  }
  
  fg_subset_col <- instruction$focal_group$subset$title %||% NULL
  required_cols <- unique(
    na.omit(
      c(metric_var,
        x_cols,
        y_cols,
        fg_subset_col
      )
    )
  )
  
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    message("❌ Missing column(s): ", 
            paste(missing_cols, collapse = ", "),
            ". Slide skipped."
    )
    return(NULL)
  }
  
  # ------ FILTER TO FOCAL GROUP -----------------------------------------------
  # Retain only the relevant group for the chart (e.g., "Focal").
  df <- data %>%
    filter(group == focal_name)
  
  # ------ APPLY FOCAL SUBSET FILTER -------------------------------------------
  # Optionally filter to a subgroup (e.g., reunion_class == "5th").
  focal_subset <- instruction$focal_group$subset
  if (!is.null(focal_subset)) {
    subset_col <- focal_subset$title
    subset_val <- focal_subset$value
    if (!is.null(subset_col) && !is.null(subset_val)) {
      df <- df %>%
        filter(.data[[subset_col]] %in% subset_val)
    }
  }
  
  # ------ REMOVE ROWS WITH NA IN KEY VARIABLES --------------------------------
  key_vars <- c(
    category_y[[1]]$name,
    cat_x_name,
    metric_var
  )
  # Add subset variable of category_y (if exists)
  if (!is.null(category_y[[1]]$subset)) {
    key_vars <- c(key_vars, category_y[[1]]$subset$title)
  }
  
  key_vars <- intersect(key_vars, names(df))
  df <- df %>%
    filter(if_all(all_of(key_vars), ~ !is.na(.)))
  # ------ VALIDATE VARIABLES --------------------------------------------------
  # Ensure all required columns are present.
  if (!cat_x_name %in% names(df)) return(NULL)
  for (cat_y in category_y) {
    if (!cat_y$name %in% names(df)) return(NULL)
  }
  
  # ------ DETERMINE X LEVELS --------------------------------------------------
  # Extract x-levels for bar segment ordering.
  lvl_x <- if (!is.null(order_x) && order_x %in% names(df)) {
    df %>%
      select(
        x_val = all_of(cat_x_name),
        x_order = all_of(order_x)
      ) %>%
      filter(!is.na(x_val), !is.na(x_order)) %>%
      distinct() %>%
      arrange(x_order) %>%
      pull(x_val)
  } else {
    df %>%
      pull(cat_x_name) %>%
      unique() %>%
      na.omit()
  }
  
  if (!is.null(value_x)) {
    lvl_x <- lvl_x[lvl_x %in% value_x]
  }
  
  # ------ BUILD COMBINED DATA -------------------------------------------------
  # Tabulate bar composition for each y_group and optional subset.
  combined_data <- list()
  
  for (cy in category_y) {
    cy_name <- cy$name
    subset <- cy$subset
    if (!cy_name %in% names(df)) next
    
    levels_y <- unique(df[[cy_name]])
    for (y_val in levels_y) {
      df_sub <- df %>%
        filter(.data[[cy_name]] == y_val)
      
      if (!is.null(subset)) {
        df_sub <- df_sub %>%
          filter(.data[[subset$title]] == subset$value)
      }
      
      if (nrow(df_sub) == 0) next
      
      df_sub <- df_sub %>%
        rename(x = all_of(cat_x_name)) %>%
        filter(!is.na(x)) %>%
        mutate(
          x = factor(x, levels = lvl_x),
          y_group = y_val,
          subset_label = if (is.null(subset)) NA_character_ else subset$value,
          bar_id = if (is.null(subset)) {
            y_val
          } else { 
            paste0(y_val, "__", subset$value)
          }
        )
      
      tab <- df_sub %>%
        count(bar_id, y_group, subset_label, x) %>%
        group_by(bar_id) %>%
        mutate(
          prop = n / sum(n),
          prop_start = cumsum(lag(prop, default = 0)),
          prop_end = prop_start + prop,
          prop_mid = (prop_start + prop_end) / 2
        ) %>%
        ungroup()
      
      if (!is.null(value_x)) {
        tab <- tab %>%
          filter(x %in% value_x)
      }
      
      combined_data[[length(combined_data) + 1]] <- tab
    }
  }
  
  combined_data <- bind_rows(combined_data)
  if (nrow(combined_data) == 0) return(NULL)
  
  # ------ ASSIGN POSITIONS FOR STACKED BARS -----------------------------------
  # Compute y positions for each stacked bar, grouped and aligned visually.
  y_group_levels <- unique(combined_data$y_group) %>%
    sort()
  
  combined_data <- combined_data %>%
    mutate(
      y_group = factor(y_group, levels = y_group_levels),
      group_index = as.numeric(factor(y_group, levels = y_group_levels))
    )
  
  bar_positions <- combined_data %>%
    distinct(y_group, subset_label) %>%
    group_by(y_group) %>%
    mutate(subset_index = row_number()) %>%
    ungroup() %>%
    mutate(
      base_y = (as.numeric(factor(y_group)) - 1) * inter_group_spacing,
      y_pos = base_y + (subset_index - 1) * intra_group_spacing
    )
  
  combined_data <- combined_data %>%
    left_join(bar_positions, by = c("y_group", "subset_label"))
  
  y_label_df <- bar_positions %>%
    group_by(y_group) %>%
    summarise(
      y_label_pos = mean(y_pos),
      .groups = "drop"
    )
  
  # ------ COMPUTE AVERAGE METRIC LABELS ---------------------------------------
  # Optionally display a summary label per bar (e.g., average hours).
  x_max <- max(combined_data$prop_end, na.rm = TRUE)
  pal <- get_color_palette(levels(combined_data$x))
  label_df <- NULL
  
  if (!is.null(metric_var) && metric_var %in% names(df)) {
    label_df <- df %>%
      mutate(
        y_group = .data[[category_y[[1]]$name]],
        subset_label = if (!is.null(category_y[[1]]$subset)) {
          .data[[category_y[[1]]$subset$title]]
        } else {
          NA_character_
        }
      ) %>%
      group_by(y_group, subset_label) %>%
      summarise(
        value = mean(.data[[metric_var]], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        label = sprintf("%.1f", value),
        x_pos = x_max + 0.02
      ) %>%
      left_join(bar_positions, by = c("y_group", "subset_label"))
  }
  
  # ------ BUILD STACKED BAR PLOT ----------------------------------------------
  # Compose all elements: bars, proportions, subset labels, axis, etc.
  plot_obj <- ggplot(combined_data) +
    annotate(
      "rect",
      xmin = 0, xmax = Inf,
      ymin = min(y_label_df$y_label_pos) - 1,
      ymax = max(y_label_df$y_label_pos) + 1,
      fill = "#005377", color = NA
    ) +
    geom_rect(
      aes(
        xmin = prop_start,
        xmax = prop_end,
        ymin = y_pos - bar_height,
        ymax = y_pos + bar_height,
        fill = x
      ),
      color = NA
    ) +
    geom_text(
      data = combined_data %>%
        filter(prop >= 0.03),
      aes(
        x = prop_mid,
        y = y_pos,
        label = percent(prop, accuracy = 1)
      ),
      color = "black", fontface = "bold", size = 5
    ) +
    annotate(
      "segment",
      x = 0, xend = 0,
      y = min(y_label_df$y_label_pos) - 1,
      yend = max(y_label_df$y_label_pos) + 1,
      color = "white", linewidth = 1.5
    ) +
    geom_text(
      data = bar_positions %>%
        filter(!is.na(subset_label)),
      aes(
        x = x_max + 0.02,
        y = y_pos,
        label = subset_label
      ),
      color = "white", fontface = "bold", size = 4,
      angle = -90, hjust = 0.5, vjust = 0.5
    )
  
  if (!is.null(label_df)) {
    plot_obj <- plot_obj +
      geom_text(
        data = label_df,
        aes(x = x_pos + 0.08, y = y_pos, label = label),
        inherit.aes = FALSE,
        color = "yellow", fontface = "bold", size = 6
      ) +
      annotate(
        "text",
        x = x_max + 0.04,
        y = max(y_label_df$y_label_pos) + 1,
        label = paste0("Avg ", unit_label, ":"),
        hjust = 0, color = "yellow", fontface = "bold", size = 6
      )
  }
  
  # ------ FINALIZE STYLING ----------------------------------------------------
  # Apply visual theming and coordinate limits.
  plot_obj <- plot_obj +
    scale_fill_manual(values = pal, drop = FALSE) +
    scale_x_continuous(
      breaks = NULL,
      labels = NULL,
      expand = expansion(mult = c(0, 0.2))
    ) +
    scale_y_continuous(
      breaks = y_label_df$y_label_pos,
      labels = y_label_df$y_group,
      expand = expansion(add = 0.3)
    ) +
    labs(x = NULL, y = NULL, fill = NULL, title = NULL) +
    global_theme() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.line.x = element_blank(),
      axis.text.y = element_text(
        color = "white", size = 16, face = "bold",
        angle = +90, vjust = 0.5, hjust = 0.5
      ),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank(),
      axis.ticks.length = unit(0, "pt"),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      panel.spacing = unit(0, "pt"),
      panel.background = element_rect(fill = "#005377", color = NA),
      plot.background = element_rect(fill = "#005377", color = NA),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.text = element_text(color = "white", size = 18, face = "bold"),
      legend.key.height = unit(20, "pt"),
      legend.key.width = unit(20, "pt"),
      legend.title = element_blank(),
      plot.margin = margin(t = 0, r = 0, b = 0, l = 80)
    ) +
    coord_cartesian(clip = "off")
  
  # ------ RETURN OR EXPORT SLIDE ----------------------------------------------
  # Return ggplot or export to PowerPoint slide if ppt_doc is provided.
  if (!is.null(ppt_doc)) {
    ppt_doc <- export_plot_to_slide(
      ppt_doc = ppt_doc,
      plot_obj = plot_obj,
      title_text = title_text,
      is_first = instruction$is_first %||% FALSE
    )
  }
  
  return(
    list(
      ppt_doc = ppt_doc,
      plot_obj = plot_obj
    )
  )
}
