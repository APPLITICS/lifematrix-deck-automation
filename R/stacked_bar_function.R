# ------ BAR STACKED VERTICAL SLIDE -------------------------------------------------
#' Generate Stacked Bar Chart Slide
#'
#' Creates a grouped or classic stacked bar chart with optional average labels
#' and top/bottom labeling logic. Handles multiple x or y categories, subset 
#' filtering, and flexible theming. Optimized for PowerPoint export.
#'
#' @param data A preprocessed dataframe (e.g., survey or summary data).
#' @param instruction A list of plotting instructions (categories, filters, etc.).
#' @param ppt_doc A PowerPoint object to append slide to (optional).
#' 
#' @return Updated PowerPoint object (if provided), or invisible(NULL).
generate_stacked_vertical_slide <- function(
    data,
    instruction,
    ppt_doc = NULL
) {
  # ------ SPACING PARAMETERS -----------------------------------------------
  intra_group_offset <- 0.2
  inter_group_expand <- 0.35
  
  # ------ EXTRACT SETTINGS -------------------------------------------------
  category_x <- instruction$category_x
  category_y <- instruction$category_y
  focal_name <- instruction$focal_group$name
  focal_subset <- instruction$focal_group$subset
  metric_var <- instruction$metric %||% NULL
  unit_label <- instruction$unit %||% ""
  chart_title <- instruction$title %||% " "
  
  # ------ VALIDITY CHECK ---------------------------------------------------
  if (length(category_x) > 1 && length(category_y) > 1) {
    stop("Multiple definitions in both category_x and category_y are not supported.")
  }
  
  # ------ DETECT MULTIPLE AXES ---------------------------------------------
  multi_y <- length(category_y) > 1
  multi_x <- length(category_x) > 1
  
  # ------ ADJUST VISUAL PARAMETERS -----------------------------------------
  bar_width <- if (multi_y || multi_x) 0.35 else 0.55
  x_expand <- if (multi_y || multi_x) rep(inter_group_expand, 2) else c(0.1, 0.1)
  plot_margin <- if (multi_y || multi_x) margin(0, 20, 0, 20) else margin(0, 100, 0, 100)
  
  # ------ FILTER TO FOCAL GROUP --------------------------------------------
  df <- data %>%
    filter(group == focal_name)
  
  if (!is.null(focal_subset)) {
    subset_col <- focal_subset$title
    subset_val <- focal_subset$value
    if (!is.null(subset_col) && !is.null(subset_val)) {
      df <- df %>% filter(.data[[subset_col]] %in% subset_val)
    }
  }
  # ------ EXTRACT BASE X VARIABLE ------------------------------------------
  base_x_name <- category_x[[1]]$name
  base_x_order <- category_x[[1]]$order
  
  # ------ DETERMINE X LEVELS ------------------------------------------------
  base_x_value <- category_x[[1]]$value %||% NULL
  
  if (!is.null(base_x_value)) {
  df <- df %>% filter(.data[[base_x_name]] %in% base_x_value)
}
  
  lvl_x <- if (!is.null(base_x_order) && all(c(base_x_name, base_x_order) %in% names(data))) {
    df %>%
      select(x_val = all_of(base_x_name), x_order = all_of(base_x_order)) %>%
      filter(!is.na(x_val), !is.na(x_order)) %>%
      distinct() %>%
      arrange(x_order) %>%
      pull(x_val)
  } else {
    unique(df[[base_x_name]])
  }
  
  # ------ FILTER X LEVELS BASED ON VALUE (if defined) -----------------------
  if (!is.null(base_x_value)) {
    lvl_x <- lvl_x[lvl_x %in% base_x_value]
  }
  
  # ------ MULTIPLE SUBSET BARS PER X ---------------------------------------
  df <- df %>% rename(x = all_of(base_x_name))
  subset_tabs <- list()
  label_df <- NULL
  if (multi_x) {
    
    for (i in seq_along(category_x)) {
      cx <- category_x[[i]]
      subset_df <- df
      
      if (!is.null(cx$subset)) {
        sub_col <- cx$subset$title
        sub_val <- cx$subset$value
        subset_df <- subset_df %>% filter(.data[[sub_col]] == sub_val)
        bar_label <- sub_val
      } else {
        bar_label <- "All"
      }
      
      cat_y <- category_y[[1]]
      cat_y_name <- cat_y$name
      cat_y_order <- cat_y$order
      cat_y_value <- cat_y$value %||% NULL
      
      lvl_y <- if (!is.null(cat_y_order) && all(c(cat_y_name, cat_y_order) %in% names(data))) {
        df %>%
          select(y_val = all_of(cat_y_name), y_order = all_of(cat_y_order)) %>%
          filter(!is.na(y_val), !is.na(y_order)) %>%
          distinct() %>%
          arrange(y_order) %>%
          pull(y_val)
      } else {
        df %>%
          pull(cat_y_name) %>%
          unique() %>%
          na.omit()
      }
      
      tab <- subset_df %>%
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
      
      if (!is.null(cat_y_value)) {
        tab <- tab %>% filter(y %in% cat_y_value)
      }
      
      subset_tabs[[i]] <- tab
      
      if (!is.null(metric_var) && metric_var %in% names(subset_df)) {
        subset_avg <- subset_df %>%
          group_by(x) %>%
          summarise(value = mean(.data[[metric_var]], na.rm = TRUE), .groups = "drop") %>%
          mutate(
            bar_position = bar_label,
            label = sprintf("%.1f", value),
            y_pos = 1.13
          )
        label_df <- bind_rows(label_df, subset_avg)
      }
    }
    combined_data <- bind_rows(subset_tabs) %>%
      mutate(x = factor(x, levels = lvl_x))
    
    x_map <- tibble(x = factor(lvl_x, levels = lvl_x), x_num = seq_along(lvl_x))
    n_pos <- length(unique(combined_data$bar_position))
    offsets <- seq(-intra_group_offset, intra_group_offset, length.out = n_pos)
    offset_map <- tibble(
      bar_position = unique(combined_data$bar_position),
      offset = offsets
    )
    
    combined_data <- combined_data %>%
      left_join(x_map, by = "x") %>%
      left_join(offset_map, by = "bar_position") %>%
      mutate(x_offset = x_num + offset)
    
    if (!is.null(label_df)) {
      label_df <- label_df %>%
        left_join(x_map, by = "x") %>%
        left_join(offset_map, by = "bar_position") %>%
        mutate(x_offset = x_num + offset)
    }
    
    top_bar_labels <- combined_data %>%
      distinct(x, x_num, bar_position, x_offset) %>%
      mutate(label = bar_position)
    
    bottom_group_labels <- x_map %>% mutate(label = as.character(x))
    
  } else {
    all_y_names <- purrr::map_chr(category_y, "name")
        lvl_y_global <- df %>%
      select(any_of(all_y_names)) %>%
      pivot_longer(cols = everything(), values_to = "val", names_to = NULL) %>%
      filter(!is.na(val)) %>%
      count(val) %>%
      arrange(desc(n)) %>%
      pull(val) %>%
      as.character()
    
    all_tabs <- list()
    for (i in seq_along(category_y)) {
      cat_y_info <- category_y[[i]]
      cat_y_name <- cat_y_info$name
      order_y <- cat_y_info$order
      value_y <- cat_y_info$value %||% NULL
      
      label_match <- variable_map$label[variable_map$variable == cat_y_name]
      bar_label <- if (length(label_match) > 0) label_match else paste0("Bar_", i)
      
      lvl_y <- if (!is.null(order_y) && all(c(cat_y_name, order_y) %in% names(data))) {
        df %>%
          select(y_val = all_of(cat_y_name), y_order = all_of(order_y)) %>%
          filter(!is.na(y_val), !is.na(y_order)) %>%
          distinct() %>%
          arrange(y_order) %>%
          pull(y_val)
      } else {
        intersect(lvl_y_global, unique(na.omit(df[[cat_y_name]])))
      }
      
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
    combined_data <- bind_rows(all_tabs) %>%
      mutate(x = factor(x, levels = lvl_x))
    x_map <- tibble(x = factor(lvl_x, levels = lvl_x), x_num = seq_along(lvl_x))
    n_pos <- length(unique(combined_data$bar_position))
    offsets <- seq(-0.22, 0.22, length.out = n_pos)
    offset_map <- tibble(
      bar_position = unique(combined_data$bar_position),
      offset = if (multi_y) offsets else rep(0, n_pos)
    )
    
    combined_data <- combined_data %>%
      left_join(x_map, by = "x") %>%
      left_join(offset_map, by = "bar_position") %>%
      mutate(x_offset = x_num + offset)
    
    top_bar_labels <- if (multi_y) {
      combined_data %>%
        distinct(x, x_num, bar_position, x_offset) %>%
        mutate(label = bar_position)
    } else NULL
    
    bottom_group_labels <- x_map %>% mutate(label = as.character(x))
    
    if (!is.null(metric_var) && metric_var %in% names(df)) {
      label_df <- df %>%
        select(x, all_of(metric_var)) %>%
        group_by(x) %>%
        summarise(value = mean(.data[[metric_var]], na.rm = TRUE), .groups = "drop") %>%
        mutate(
          label = sprintf("%.1f", value),
          y_pos = 1.13
        ) %>%
        left_join(x_map, by = "x")
    }
  }
  
  # ------ FINAL PLOT BUILDING ----------------------------------------------
  y_limit_top  <- if (!is.null(label_df)) max(label_df$y_pos, 1.1) else 1.1
  fill_levels  <- unique(combined_data$y)
  pal          <- get_color_palette(fill_levels)
  
  plot_obj <- ggplot(combined_data) +
    geom_bar(
      aes(x = x_offset, y = prop, fill = y, group = interaction(x, bar_position)),
      stat = "identity",
      width = bar_width
    ) +
    geom_text(
      data = combined_data %>% filter(prop >= 0.03),
      aes(x = x_offset, y = prop, label = scales::percent(prop, accuracy = 1)),
      stat = "identity",
      position = position_stack(vjust = 0.5),
      color = "black",
      fontface = "bold",
      size = 5
    ) +
    {
      if (!is.null(top_bar_labels)) {
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
            aes(
              x = .data[[if ("x_offset" %in% names(label_df)) "x_offset" else "x_num"]],
              y = y_pos,
              label = label
            ),
            inherit.aes = FALSE,
            color = "yellow",
            fontface = "bold",
            size = 7
          ),
          annotate(
            "text",
            x = 0.5,
            y = label_df$y_pos[1],
            label = paste0("Avg ", unit_label, ":"),
            hjust = 1,
            color = "yellow",
            fontface = "bold",
            size = 7
          )
        )
      }
    } +
    geom_hline(yintercept = 0, color = "white", linewidth = 1.5) +
    geom_text(
      data = bottom_group_labels,
      aes(x = x_num, y = -0.07, label = label),
      inherit.aes = FALSE,
      size = 7,
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
  
  # ------ EXPORT TO SLIDE (IF NEEDED) --------------------------------------
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


# ------ BAR STACKED HORIZONTAL SLIDE -----------------------------------------
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
#' @return Updated PowerPoint object (if provided), or invisible(NULL).
generate_stacked_horizontal_slide <- function(
    data,
    instruction,
    ppt_doc = NULL
) {
  # ------ SPACING PARAMETERS -----------------------------------------------
  inter_group_spacing <- 2
  intra_group_spacing <- 0.7
  
  # ------ EXTRACT SETTINGS -------------------------------------------------
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
  
  # ------ FILTER TO FOCAL GROUP --------------------------------------------
  # Retain only the relevant group for the chart (e.g., "Focal").
  df <- data %>%
    filter(group == focal_name)
  
  # ------ APPLY FOCAL SUBSET FILTER ----------------------------------------
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
  
  # ------ VALIDATE VARIABLES ------------------------------------------------
  # Ensure all required columns are present.
  if (!cat_x_name %in% names(df)) return(NULL)
  for (cat_y in category_y) {
    if (!cat_y$name %in% names(df)) return(NULL)
  }
  
  # ------ DETERMINE X LEVELS ------------------------------------------------
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
  
  # ------ BUILD COMBINED DATA ----------------------------------------------
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
          bar_id = if (is.null(subset)) y_val else paste0(y_val, "__", subset$value)
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
  
  # ------ ASSIGN POSITIONS FOR STACKED BARS ---------------------------------
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
  
  # ------ COMPUTE AVERAGE METRIC LABELS ------------------------------------
  # Optionally display a summary label per bar (e.g., average hours).
  x_max <- max(combined_data$prop_end, na.rm = TRUE)
  pal <- get_color_palette(levels(combined_data$x))
  label_df <- NULL
  
  if (!is.null(metric_var) && metric_var %in% names(df)) {
    label_df <- df %>%
      mutate(
        y_group = .data[[category_y[[1]]$name]],
        subset_label = if (!is.null(category_y[[1]]$subset)) .data[[category_y[[1]]$subset$title]] else NA_character_
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
  
  # ------ BUILD STACKED BAR PLOT -------------------------------------------
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
    annotate(
      "rect",
      xmin = x_max + 0.001, xmax = x_max + 0.15,
      ymin = min(y_label_df$y_label_pos) - 1,
      ymax = max(y_label_df$y_label_pos) + 1,
      fill = "#005377", color = NA
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
  
  # ------ FINALIZE STYLING -------------------------------------------------
  # Apply visual theming and coordinate limits.
  plot_obj <- plot_obj +
    scale_fill_manual(values = pal, drop = FALSE) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.02))) +
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
      plot.margin = margin(t = 0, r = 40, b = 0, l = 80)
    ) +
    coord_cartesian(clip = "off")
  
  # ------ RETURN OR EXPORT SLIDE -------------------------------------------
  # Return ggplot or export to PowerPoint slide if ppt_doc is provided.
  if (!is.null(ppt_doc)) {
    ppt_doc <- export_plot_to_slide(
      ppt_doc = ppt_doc,
      plot_obj = plot_obj,
      title_text = title_text,
      is_first = instruction$is_first %||% FALSE
    )
    return(ppt_doc)
  }
  
  return(invisible(NULL))
}
